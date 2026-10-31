#!/usr/bin/env python3
"""Local vector indexing and embedding worker. Model weights are installed separately."""
import argparse
import base64
import errno
import hashlib
import io
import json
import os
import socket
import struct
import subprocess
import sys
import time
from pathlib import Path

sys.dont_write_bytecode = True  # ひかるの中に __pycache__ を作らない

import numpy as np

HERE = Path(__file__).resolve()
sys.path.insert(0, str(HERE.parent))
import _hk  # noqa: E402  ロック（索引を同時に書き換えないため）
from _tool_data import read_json, run_cli

ROOT = HERE.parent.parent
INDEX_ROOT = ROOT / ".tools" / "index"

# 名前: (Hugging Face の ID, 問いの前置き, 本文の前置き, 意味の側の許容幅)
# 許容幅: いちばん近い断片のコサイン類似度から、この幅より離れた断片は候補にしない。
#   モデルで分布が違う（e5 は 0.85 前後に密集、bge-m3 は 0.6〜0.75 と広い）ので、モデルごとに持つ。
MODELS = {
    "e5-small": ("intfloat/multilingual-e5-small", "query: ", "passage: ", 0.06),
    "e5-base": ("intfloat/multilingual-e5-base", "query: ", "passage: ", 0.06),
    "e5-large": ("intfloat/multilingual-e5-large", "query: ", "passage: ", 0.06),
    "bge-m3": ("BAAI/bge-m3", "", "", 0.10),
}
DEFAULT_MODEL = os.environ.get("HIKARU_EMBED", "bge-m3")
DEFAULT_DEVICE = os.environ.get("HIKARU_EMBED_DEVICE", "cpu")
WINDOW, OVERLAP = 400, 60   # 長い断片は 400 字ごとに切って（60 字重ねて）埋め込む。検索時は断片の中で最も近いウィンドウを取る

SOCK = Path(os.environ.get("HIKARU_EMBED_SOCK", str(ROOT / ".local" / "embed.sock")))
LOCK = SOCK.with_suffix(".lock")
LOG = SOCK.with_suffix(".log")
IDLE_SECONDS = int(os.environ.get("HIKARU_EMBED_IDLE", str(2 * 3600)))
USE_SERVER = os.environ.get("HIKARU_EMBED_SERVER", "1") != "0"
CODE_STAMP = hashlib.sha1(HERE.read_bytes()).hexdigest()[:12]   # このファイルの版。常駐と食い違えば入れ替わる


def sha(text):
    return hashlib.sha1(text.encode("utf-8")).hexdigest()


def windows(text, size=WINDOW, overlap=OVERLAP):
    """長い断片をウィンドウに切る。空白（改行も）は一つの空白に潰してから切る。"""
    t = " ".join(text.split())
    if len(t) <= size:
        return [t]
    out, i = [], 0
    while True:
        out.append(t[i:i + size])
        if i + size >= len(t):
            return out
        i += size - overlap


def window_span(text, k, size=WINDOW, overlap=OVERLAP):
    """windows(text) の k 番目のウィンドウが、元の本文（空白を潰す前）のどこにあたるか。(始まり, 終わり) の位置。
    omoide.py が、意味で当たったウィンドウのあたりを表示するのに使う。切り方は windows と同じ。"""
    pos, space_at = [], None               # 潰したあとの n 字目 → 元の位置。space_at は空白の連なりの最初の位置
    for i, ch in enumerate(text):
        if ch.isspace():
            if space_at is None:
                space_at = i
            continue
        if space_at is not None and pos:
            pos.append(space_at)           # 潰した空白は、連なりの最初の空白の位置（先頭の空白は windows でも消えるので入れない）
        space_at = None
        pos.append(i)
    if not pos:
        return 0, 0
    n = len(pos)
    start = 0 if n <= size else max(0, k) * (size - overlap)
    start = min(start, len(pos) - 1)
    end = min(start + size, len(pos))
    return pos[start], pos[end - 1] + 1


# ---------- 常駐とのやりとり（長さ 8 バイト ＋ JSON） ----------

def _send(conn, obj):
    data = json.dumps(obj, ensure_ascii=False).encode("utf-8")
    conn.sendall(struct.pack("!Q", len(data)) + data)


def _recv_exact(conn, n):
    buf = bytearray()
    while len(buf) < n:
        chunk = conn.recv(min(1 << 20, n - len(buf)))
        if not chunk:
            return None
        buf += chunk
    return bytes(buf)


def _recv(conn):
    head = _recv_exact(conn, 8)
    if head is None:
        return None
    (n,) = struct.unpack("!Q", head)
    data = _recv_exact(conn, n)
    return None if data is None else json.loads(data.decode("utf-8"))


def _pack(arr):
    arr = np.ascontiguousarray(arr, dtype=np.float32)
    return {"shape": list(arr.shape), "b64": base64.b64encode(arr.tobytes()).decode("ascii")}


def _unpack(d):
    return np.frombuffer(base64.b64decode(d["b64"]), dtype=np.float32).reshape(d["shape"]).copy()


def _ask(req, timeout=None):
    """常駐に一つ頼んで答えをもらう。いなければ例外。timeout は答えを待つ上限（None は待ち続ける）。"""
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    try:
        s.settimeout(2)
        s.connect(str(SOCK))
        s.settimeout(timeout)
        _send(s, req)
        resp = _recv(s)
    finally:
        s.close()
    if resp is None:
        raise ConnectionError(f"embedding server {SOCK} closed without a response")
    return resp


def _log(msg):
    new = not LOG.exists()
    with open(LOG, "a", encoding="utf-8") as f:
        f.write(f"{time.strftime('%Y-%m-%d %H:%M:%S')} [{os.getpid()}] {msg}\n")
    if new:
        os.chmod(LOG, 0o600)


def _spawn():
    """常駐を裏で起こす（親が終わっても生きる）。"""
    env = dict(os.environ, TRANSFORMERS_VERBOSITY="error", HF_HUB_DISABLE_PROGRESS_BARS="1", TQDM_DISABLE="1",
               TOKENIZERS_PARALLELISM="false")
    with open(LOG, "ab") as log:
        subprocess.Popen([sys.executable, str(HERE), "--serve"], stdin=subprocess.DEVNULL, stdout=log, stderr=log,
                         start_new_session=True, close_fds=True, cwd=str(ROOT), env=env)


def _ping(timeout=2):
    try:
        r = _ask({"op": "ping"}, timeout=timeout)
        return r if r["ok"] else None
    except (FileNotFoundError, ConnectionRefusedError, TimeoutError):
        return None


def _wait_alive(timeout=20, old_pid=None):
    """常駐が答えるまで待つ。old_pid（古い版の pid）があれば、ping の pid がそれと変わるまでを「起きた」とみなす。"""
    t0 = time.time()
    while time.time() - t0 < timeout:
        r = _ping()
        if r and (old_pid is None or r.get("pid") != old_pid):
            return True
        time.sleep(0.2)
    return False


def serve():
    """常駐の本体。ロックを取れた一つだけが動く。要求ごとにスレッドで答える（埋め込みは一つずつ、ping は即答）。"""
    import fcntl
    import threading
    lock_f = open(LOCK, "w")
    for _ in range(50):
        try:
            fcntl.flock(lock_f, fcntl.LOCK_EX | fcntl.LOCK_NB)
            break
        except BlockingIOError as error:
            if error.errno != errno.EWOULDBLOCK:
                lock_f.close()
                raise
            time.sleep(0.1)
        except OSError:
            lock_f.close()
            raise
    else:
        lock_f.close()
        return  # ほかの常駐が起きている
    state = {"last": time.time(), "last_used": None, "quit": None}
    embedders = {}
    enc_lock = threading.Lock()

    def handle(conn):
        with conn:
            conn.settimeout(60)
            try:
                req = _recv(conn)
                if not req:
                    return
                op = req.get("op")
                if op == "ping":
                    _send(conn, {"ok": True, "pid": os.getpid(), "stamp": CODE_STAMP,
                                 "models": sorted(k[0] for k in embedders), "last_used": state["last_used"]})
                elif op == "stop":
                    state["quit"] = "止めてと言われた"
                    _send(conn, {"ok": True})
                elif op == "encode":
                    if req.get("stamp") != CODE_STAMP:
                        state["quit"] = "新しい版があるので入れ替わる"
                        _send(conn, {"stale": True, "pid": os.getpid()})   # 頼んだ側は、この pid が変わるまで待つ
                        return
                    key = (req["model"], req.get("device", DEFAULT_DEVICE))
                    with enc_lock:
                        emb = embedders.get(key)
                        if emb is None:
                            t0 = time.time()
                            emb = Embedder(key[0], key[1], use_server=False)
                            emb.model  # ここで読む
                            embedders[key] = emb
                            _log(f"{key[0]} を読んだ（{time.time() - t0:.1f} 秒）")
                        t0 = time.time()
                        vecs = emb._encode_local(req["texts"])
                    state["last_used"] = time.strftime("%Y-%m-%d %H:%M:%S")
                    _send(conn, {"vecs": _pack(vecs)})
                    _log(f"{key[0]}: {len(req['texts'])} 件 {time.time() - t0:.2f} 秒")
                else:
                    _send(conn, {"error": f"ValueError: unknown embedding operation: {op}"})
            except (BrokenPipeError, ConnectionResetError, socket.timeout):
                pass  # 相手が待ちきれずに閉じた
            except (OSError, ValueError, KeyError, TypeError, RuntimeError, ImportError) as e:
                _log(f"{type(e).__name__}: {e}")
                try:
                    _send(conn, {"error": f"{type(e).__name__}: {e}"})
                except (BrokenPipeError, ConnectionResetError, socket.timeout):
                    pass

    try:
        os.chmod(LOCK, 0o600)
        SOCK.unlink(missing_ok=True)
        srv = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        srv.bind(str(SOCK))
        os.chmod(SOCK, 0o600)
        srv.listen(16)
        srv.settimeout(1)
        _log(f"起きた 版={CODE_STAMP}")
        workers = []
        while state["quit"] is None:
            try:
                conn, _ = srv.accept()
            except socket.timeout:
                workers = [t for t in workers if t.is_alive()]
                if not workers and time.time() - state["last"] > IDLE_SECONDS:
                    state["quit"] = "しばらく呼ばれないので寝る"
                continue
            state["last"] = time.time()
            t = threading.Thread(target=handle, args=(conn,), daemon=True)
            t.start()
            workers.append(t)
        srv.close()
        SOCK.unlink(missing_ok=True)  # 新しい要求はもう受けない。進んでいる分は待つ
        for t in workers:
            t.join(timeout=600)
        _log(state["quit"])
    finally:
        try:
            SOCK.unlink(missing_ok=True)
        finally:
            try:
                fcntl.flock(lock_f, fcntl.LOCK_UN)
            finally:
                lock_f.close()
        _log("寝た")


def status():
    r = _ping()
    if not r:
        print("常駐はいない（次に意味で検索したとき、起きる）。")
        return
    note = "" if r["stamp"] == CODE_STAMP else f"（このファイルは {CODE_STAMP}。次に頼まれたとき入れ替わる）"
    print(f"常駐している。pid {r['pid']}、版 {r['stamp']}{note}、読んであるモデル: {', '.join(r['models']) or 'まだなし'}、"
          f"最後に使われたのは {r['last_used'] or 'まだ'}。")


def stop():
    try:
        _ask({"op": "stop"}, timeout=5)
        print("常駐を止めた。")
    except (FileNotFoundError, ConnectionRefusedError):
        print("常駐はいない。")


# ---------- 埋め込み ----------

class Embedder:
    def __init__(self, name=DEFAULT_MODEL, device=DEFAULT_DEVICE, use_server=USE_SERVER):
        if name not in MODELS:
            raise SystemExit(f"知らないモデル: {name}（使えるのは {', '.join(MODELS)}）")
        self.name = name
        self.model_id, self.q_prefix, self.p_prefix, self.margin = MODELS[name]
        self.device = device
        self.use_server = use_server
        self._m = None

    @property
    def model(self):
        if self._m is None:
            os.environ.setdefault("TOKENIZERS_PARALLELISM", "false")
            from sentence_transformers import SentenceTransformer
            self._m = SentenceTransformer(self.model_id, device=self.device, local_files_only=True)
        return self._m

    def _encode_local(self, texts):
        v = self.model.encode(texts, batch_size=32, normalize_embeddings=True, convert_to_numpy=True, show_progress_bar=False)
        return np.asarray(v, dtype=np.float32)

    def _encode_server(self, texts):
        """常駐に頼む。いなければ起こす。応答がなければ止める。
        古い版が「stale」と答えたときは、その pid を覚えておき、ping の pid が変わるまで（最大 20 秒）待って、新しい版に頼む。"""
        req = {"op": "encode", "model": self.name, "device": self.device, "stamp": CODE_STAMP, "texts": texts}
        spawned = False
        while True:
            try:
                resp = _ask(req)
            except (FileNotFoundError, ConnectionRefusedError):
                resp = None
            if resp and "vecs" in resp:
                return _unpack(resp["vecs"])
            if resp and resp.get("error"):
                raise RuntimeError(f"embedding server {SOCK} failed: {resp['error']}")
            if spawned:   # 起こしたのに答えない
                raise RuntimeError(f"embedding server {SOCK} did not respond after startup")
            spawned = True
            old_pid = None
            if resp and resp.get("stale"):
                old_pid = resp.get("pid")   # 古い版の pid（もっと古い版は返さないので None のこともある）
                time.sleep(0.5)             # 古い版がソケットを片づけるのを待つ
            _spawn()
            if not _wait_alive(old_pid=old_pid):
                raise TimeoutError(f"embedding server {SOCK} did not become ready within 20 seconds")

    def _encode(self, texts, prefix):
        texts = [prefix + t for t in texts]
        if self.use_server:
            return self._encode_server(texts)
        return self._encode_local(texts)

    def passages(self, texts):
        return self._encode(texts, self.p_prefix)

    def queries(self, texts):
        return self._encode(texts, self.q_prefix)


class TooMany(Exception):
    """prune が、決めた上限より多く落とそうとした（落としていない）。args = (落とそうとした断片の数, 索引の断片の数)。"""


class Index:
    """内容の sha1 → ベクトルの行番号（ウィンドウの順）。平文は持たない。

    ディスクには vectors.npy と meta.json。meta の vectors_sha1 は vectors.npy のバイト列の sha1 で、
    読むときに照合する。合わなければ 0.2 秒待って一度だけ読み直し、それでも合わなければ索引は空から作り直す。
    形が使えないものも無いものとする。権限などの読み取りエラーでは止める。空の索引はファイルを置かない（無いのと同じ）。
    書くのは save だけで、add と prune は _hk.lock(meta.json) の中で読み直してから書く。"""

    def __init__(self, name=DEFAULT_MODEL):
        if name not in MODELS:
            raise SystemExit(f"知らないモデル: {name}（使えるのは {', '.join(MODELS)}）")
        self.name = name
        self.dir = INDEX_ROOT / name
        self.vec_path = self.dir / "vectors.npy"
        self.meta_path = self.dir / "meta.json"
        self.model_id = MODELS[name][0]
        self._load()

    def _read_disk(self):
        """meta.json と vectors.npy を読んで (meta, バイト列, その sha1) を返す。読めなければ例外。"""
        m = read_json(self.meta_path)
        data = self.vec_path.read_bytes()
        return m, data, hashlib.sha1(data).hexdigest()

    def _load(self):
        """ディスクの索引を読む。壊れていたり、sha1 が合わなかったり、形が使えなければ、空から始める。
        権限などの読み取りエラーは、そのまま呼んだ側に返す。"""
        self.rows, self.vectors, self.built_at, self.vectors_sha1 = {}, None, None, None
        try:
            m, data, digest = self._read_disk()
        except (FileNotFoundError, UnicodeDecodeError, json.JSONDecodeError):
            return
        if not isinstance(m, dict):
            return
        if m.get("vectors_sha1") is not None and m["vectors_sha1"] != digest:
            time.sleep(0.2)  # save の二段 rename の隙間だけ、一度読み直す。
            try:
                m, data, digest = self._read_disk()
            except (FileNotFoundError, UnicodeDecodeError, json.JSONDecodeError):
                return
            if not isinstance(m, dict) or m.get("vectors_sha1") is not None and m["vectors_sha1"] != digest:
                return
        if not (m.get("model") == self.model_id and m.get("window") == WINDOW and m.get("overlap") == OVERLAP):
            return
        try:
            vectors = np.load(io.BytesIO(data))
        except (ValueError, EOFError):
            return  # npy の内容が壊れている。
        if not isinstance(vectors, np.ndarray):
            vectors.close()  # npz はこの索引の形式ではない。
            return
        rows = m.get("rows")
        if vectors.ndim != 2 or vectors.shape[1] == 0 or len(vectors) == 0 or not isinstance(rows, dict) or not rows:
            return
        if any(not isinstance(rs, list) or any(not isinstance(r, int) or not 0 <= r < len(vectors) for r in rs)
               for rs in rows.values()):
            return
        self.vectors, self.rows, self.built_at, self.vectors_sha1 = vectors, rows, m.get("built_at"), digest

    def __len__(self):
        return 0 if self.vectors is None else len(self.vectors)

    def save(self):
        """vectors.npy と meta.json を書く。meta に vectors.npy のバイト列の sha1 も記す。
        vectors.npy を先に置き換え、meta.json をあとから置き換える（その隙間に読んだ索引は使わない）。
        ベクトルが無い（None か 0 行）ときは (0,0) を書かず、ファイルを消す（空の索引は無いのと同じ）。"""
        if self.vectors is None or len(self.vectors) == 0:
            for p in (self.vec_path, self.meta_path):
                p.unlink(missing_ok=True)
            self.vectors, self.rows, self.built_at, self.vectors_sha1 = None, {}, None, None
            return
        self.dir.mkdir(parents=True, exist_ok=True)
        vectors = self.vectors
        buf = io.BytesIO()
        np.save(buf, vectors)
        data = buf.getvalue()
        self.vectors_sha1 = hashlib.sha1(data).hexdigest()
        tmp = self.dir / "vectors.tmp"
        tmp.write_bytes(data)
        tmp.replace(self.vec_path)
        self.built_at = time.strftime("%Y-%m-%d %H:%M:%S")
        meta = {"model": self.model_id, "window": WINDOW, "overlap": OVERLAP, "dim": int(vectors.shape[1]),
                "n_rows": len(self), "n_chunks": len(self.rows), "built_at": self.built_at,
                "vectors_sha1": self.vectors_sha1, "rows": self.rows}
        tmp = self.dir / "meta.tmp"
        tmp.write_text(json.dumps(meta, ensure_ascii=False), encoding="utf-8")
        tmp.replace(self.meta_path)

    def reset(self):
        """ディスクの索引を捨てる（--rebuild 用）。"""
        with _hk.lock(self.meta_path):
            for p in (self.vec_path, self.meta_path):
                p.unlink(missing_ok=True)
            self._load()

    def add(self, items, embedder, _again=True):
        """items: [(sha1, 本文)]。索引にないものだけ埋め込んで足す。足した断片の数を返す。
        埋め込みの計算はロックの外で。足すところは _hk.lock の中で、ディスクの索引を読み直し
        （ほかの会話が足した分を取り込む）、まだ無いものだけ足して保存する。消すことはしない。
        読み直したら、はじめに見たときにはあった断片が消えていた（その間に掃除や作り直しがあった）ときは、
        その分をもう一度だけ埋め込んで足す（そうしないと、この回の検索でその断片が意味の側から漏れる）。"""
        todo = {s: t for s, t in items if s not in self.rows}
        if not todo:
            return 0
        texts, owners = [], []
        for s, t in todo.items():
            for w in windows(t):
                texts.append(w)
                owners.append(s)
        vecs = embedder.passages(texts).astype(np.float16)
        n = 0
        with _hk.lock(self.meta_path):
            self._load()
            keep = [k for k, s in enumerate(owners) if s not in self.rows]
            if keep:
                base = len(self)
                for i, k in enumerate(keep):
                    self.rows.setdefault(owners[k], []).append(base + i)
                self.vectors = vecs[keep] if self.vectors is None else np.concatenate([self.vectors, vecs[keep]])
                self.save()
                n = len({owners[k] for k in keep})
        if _again and any(s not in self.rows for s, _ in items):
            n += self.add(items, embedder, _again=False)
        return n

    def prune(self, shas, cap=None):  # shas には、ふつう live_shas を渡す
        """shas（いま記録にある断片の sha1 の集まり）に無い断片の行を落として詰める。落とした断片の数を返す。
        shas は関数でもよい。そのときは、鍵の中で、索引を読み直したあとに呼んで集める
        （先に集めておくと、そのあとで記録に足されて索引にも入った断片を、「記録に無い」と見て落としてしまう）。
        cap（索引の断片の数 → 一度に落としてよい数）があって、落とす数がそれを超えるなら、何も落とさずに TooMany を投げる。
        数えるのも落とすのも、同じ鍵の中で、読み直したあとの索引に対して行う（鍵の外で数えると、そのあいだに足された分を数え落とす）。
        _embed.py の単体実行と --rebuild と fuutou prune のときだけ呼ぶ（recall は呼ばない）。"""
        with _hk.lock(self.meta_path):
            self._load()
            shas = set(shas() if callable(shas) else shas)
            gone = [s for s in self.rows if s not in shas]
            if not gone or self.vectors is None:
                return 0
            if cap is not None and len(gone) > cap(len(self.rows)):
                raise TooMany(len(gone), len(self.rows))
            keep_rows = sorted(r for s, rs in self.rows.items() if s in shas for r in rs)
            remap = {old: new for new, old in enumerate(keep_rows)}
            self.rows = {s: [remap[r] for r in rs] for s, rs in self.rows.items() if s in shas}
            self.vectors = self.vectors[keep_rows] if keep_rows else None   # 全部落ちたら空（save がファイルを消す）
            self.save()
            return len(gone)

    def scores(self, shas, qvecs):
        """各 sha1 について、問い（複数可）と断片のウィンドウのあいだで最も近いコサイン類似度と、そのウィンドウの番号。
        返り値は (点の列, ウィンドウ番号の列)。索引にないものは点が nan、ウィンドウ番号が -1。
        ウィンドウ番号は windows(本文) の順で、window_span で元の本文の位置に戻せる。"""
        out = np.full(len(shas), np.nan, dtype=np.float32)
        best = np.full(len(shas), -1, dtype=np.int64)
        if self.vectors is None or len(qvecs) == 0:
            return out, best
        sims = (self.vectors.astype(np.float32) @ np.asarray(qvecs, dtype=np.float32).T).max(axis=1)
        for i, s in enumerate(shas):
            r = self.rows.get(s)
            if r:
                k = int(np.argmax(sims[r]))
                out[i], best[i] = sims[r][k], k
        return out, best


def live_shas():
    """いま記録にある断片の sha1。全部の記録の断片（recall.collect(all_=True)）に、こより（.tools/_koyori.py）が
    索引に入れた判・足跡・ストロボの項目を足したもの。掃除（prune）は、これに無い行だけを落とす。
    こよりが読めなければ例外のまま上げる（掃除をやめる。読めないまま掃除すると、判と足跡の行を落としてしまう）。"""
    import omoide as recall
    import _koyori
    out = {sha(cs[i]) for _, i, cs in recall.collect(all_=True)}
    return out | {e["sha"] for e in _koyori.load()["entries"] if e["kind"] in _koyori.OWN_INDEX_KINDS}


def main():
    ap = argparse.ArgumentParser(description="意味の索引を最新にする（全部の記録。増えた断片だけ埋め込み、記録にない断片の行を落とす）。"
                                 "常駐の様子を見る・止める。")
    ap.add_argument("--model", default=DEFAULT_MODEL, choices=sorted(MODELS))
    ap.add_argument("--device", default=DEFAULT_DEVICE)
    ap.add_argument("--rebuild", action="store_true", help="索引を捨てて作り直す")
    ap.add_argument("--prune", action="store_true", help="掃除だけ。モデルを使わず、記録にない断片の行を落とす（数秒）")
    ap.add_argument("--status", action="store_true", help="常駐の様子")
    ap.add_argument("--stop", action="store_true", help="常駐を止める")
    ap.add_argument("--serve", action="store_true", help=argparse.SUPPRESS)   # 内部用。常駐として動く
    a = ap.parse_args()
    if a.serve:
        return serve()
    if a.status:
        return status()
    if a.stop:
        return stop()
    if a.prune and a.rebuild:
        ap.error("--prune と --rebuild は一緒に使えない（作り直すなら --rebuild だけ）")
    import omoide as recall
    t_start = time.time()
    idx = Index(a.model)
    if a.rebuild:
        idx.reset()
    docs = recall.collect(all_=True)    # 単体実行は全部の記録（recall の既定は原文だけ）
    items = [(sha(cs[i]), cs[i]) for _, i, cs in docs]
    shas = {s for s, _ in items}
    t0 = time.time()
    now_shas = live_shas                # 掃除のときは、鍵の中で集め直す（集めたあとで足された記録を落とさないように）

    if a.prune:                         # 掃除だけ。Embedder を作らない（モデルを読まない）
        dropped = idx.prune(now_shas)
        print(f"{a.model}: 記録にない {dropped} 件の断片の行を落とした（記録を読むところから {time.time() - t_start:.1f} 秒）。"
              f"索引は {len(idx)} 行、{idx.dir.relative_to(ROOT)}/")
        return
    n = idx.add(items, Embedder(a.model, a.device))
    dropped = idx.prune(now_shas)       # もう記録にない断片の行を落とす（ここと --rebuild のときだけ）
    note = f"、記録にない {dropped} 件の行を落とした" if dropped else ""
    print(f"{a.model}: 断片 {len(shas)} 件のうち、新しく {n} 件を埋め込んだ{note}（{time.time() - t0:.1f} 秒）。"
          f"索引は {len(idx)} 行、{idx.dir.relative_to(ROOT)}/")


if __name__ == "__main__":
    run_cli(main)
