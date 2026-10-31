#!/usr/bin/env python3
"""Shared paths, archive exclusion rules, sealed text I/O and session metadata."""
import sys

sys.dont_write_bytecode = True  # ひかるの中に __pycache__ を作らない

import base64
import hashlib
import contextlib
import fcntl
import json
import os
import re
import shutil
import tempfile
from datetime import date, datetime, timedelta, timezone
from pathlib import Path

ROOT = Path(os.environ.get("HIKARU_DATA_ROOT", Path(__file__).resolve().parent.parent)).resolve()
HIKARU = str(ROOT)
PROJECTS = Path(os.environ.get("HIKARU_LOG_ROOT", ROOT / ".local" / "logs"))
TM = ROOT / ".タイムマシン"
DIARY = ROOT / ".一週間フレンズ"
AN = ROOT / ".ありあけ"            # 会話にしるしをつけて読んだ記録
PIC = ROOT / ".おしばな"           # ユーザーが会話で送った画像
SHIORI = ROOT / ".しおり.md.b64"   # まだ聞いていないこと、まだ入れていない候補
BAK = ROOT / ".tools" / "bak"      # 書き換える前の版。日付のフォルダに、その日最初の書き換えの前の版だけ。7 日で消す
LOCKS = ROOT / ".tools" / "locks"
PROGRESS = TM / ".progress.json"   # 朝の仕事の進捗（日付・時刻・ひととき ID だけ）
SESSIONS = TM / ".sessions.json"   # 会話の短い名 → 完全な sessionId
JST = timezone(timedelta(hours=9))
WD = "月火水木金土日"
PAGE_BYTES = 16000   # これより大きい出力は、ページに分けて表示する
DAY_START = 5        # ユーザーの一日は朝 5 時から
BAK_KEEP_DAYS = 7

# Generated locally from settings.local.json by scripts/configure.py.
_privacy_file = Path(__file__).resolve().parent.parent / ".local" / "privacy.json"
try:
    _privacy_bytes = _privacy_file.read_bytes()
    if hashlib.sha256(_privacy_bytes).hexdigest() != (_privacy_file.parent / "compiled-settings.sha256").read_text().strip():
        raise RuntimeError("Local settings changed; rebuild Hikaru before using the tools")
    TOBARI_PHRASES = tuple(tuple(p) for p in json.loads(_privacy_bytes)["privacy_phrases"])
except (OSError, ValueError, KeyError) as error:
    raise RuntimeError("Run scripts/configure.py with your local settings before using Hikaru") from error
TOBARI_PUNCTUATION = "、。，．？?,.｡､"
START, END = TOBARI_PHRASES[0]  # 以前からの呼び出し用。判定には classify_phrase を使う。


def normalize_phrase(text):
    return text.strip().translate(_PHRASE_TRANSLATION)


_PHRASE_TRANSLATION = str.maketrans("", "", TOBARI_PUNCTUATION)
_PHRASE_STARTS = frozenset(normalize_phrase(a) for a, _ in TOBARI_PHRASES)
_PHRASE_ENDS = frozenset(normalize_phrase(b) for _, b in TOBARI_PHRASES)


def classify_phrase(text):
    """一言全体を見る。戻り値は start / end / None。途中の空白や文字は変えない。"""
    normalized = normalize_phrase(text)
    if normalized in _PHRASE_STARTS:
        return "start"
    if normalized in _PHRASE_ENDS:
        return "end"
    return None

EXT = {"image/png": "png", "image/jpeg": "jpg", "image/webp": "webp", "image/gif": "gif"}


# ---------- 日 ----------

def hikaru_day(dt):
    """ユーザーにとっての、その時刻の「日」。朝 5 時で切る（夜中の 0 時すぎは、まだ前の日）。"""
    return (dt.astimezone(JST) - timedelta(hours=DAY_START)).date()


def entry_datetime(day, hh, mm, ss=0):
    """タイムマシンのファイル（日 = hikaru_day）に書かれた時刻から datetime を作る。5 時前は翌暦日の未明。"""
    d = day + timedelta(days=1) if hh < DAY_START else day
    return datetime(d.year, d.month, d.day, hh, mm, ss, tzinfo=JST)


# ---------- 封 ----------

def read_sealed(path):
    """封（base64）を解読して文字列で返す。"""
    return base64.b64decode(Path(path).read_bytes()).decode("utf-8")


def write_sealed(path, text):
    """文字列に封をして書く。書く前に、解読して元と同じになるか確かめる。途中で平文はディスクに置かない。"""
    path = Path(path)
    data = base64.b64encode(text.encode("utf-8"))
    if base64.b64decode(data).decode("utf-8") != text:
        raise RuntimeError(f"封の確認に失敗: {path.name}")
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, tmp = tempfile.mkstemp(prefix=".tmp-", suffix=".b64", dir=str(path.parent))
    try:
        with os.fdopen(fd, "wb") as f:
            f.write(data)
        os.replace(tmp, path)
    finally:
        if os.path.exists(tmp):
            os.unlink(tmp)


def backup(path):
    """封をしたままの今の版を .tools/bak/日付/ に残す。同じ日に二度目以降は残さない（その日最初の書き換えの前の版が残る）。"""
    path = Path(path)
    if not path.exists():
        return
    d = BAK / f"{hikaru_day(datetime.now(JST)):%Y-%m-%d}"
    d.mkdir(parents=True, exist_ok=True)
    dst = d / path.name.lstrip(".")
    if not dst.exists():
        dst.write_bytes(path.read_bytes())
    prune_bak()


def prune_bak(keep=BAK_KEEP_DAYS):
    """bak/ の日付フォルダのうち、keep 日より古いものを消す。日付でないものは触らない。"""
    if not BAK.exists():
        return
    today = hikaru_day(datetime.now(JST))
    for p in BAK.iterdir():
        if p.is_dir() and re.fullmatch(r"\d{4}-\d{2}-\d{2}", p.name):
            try:
                old = (today - date.fromisoformat(p.name)).days > keep
            except ValueError:
                continue
            if old:
                shutil.rmtree(p, ignore_errors=True)


# ---------- 排他 ----------

@contextlib.contextmanager
def lock(path):
    """同じ記録を同時に書き換えないための鍵。with lock(目的のファイル): の中で読んで書く。ロックは .tools/locks/ に置く。"""
    LOCKS.mkdir(parents=True, exist_ok=True)
    lp = LOCKS / (Path(path).name.lstrip(".") + ".lock")
    f = open(lp, "w")
    try:
        fcntl.flock(f, fcntl.LOCK_EX)
        yield
    finally:
        try:
            fcntl.flock(f, fcntl.LOCK_UN)
        finally:
            f.close()


# ---------- 会話記録（jsonl） ----------

def ts(s):
    return datetime.fromisoformat(s.replace("Z", "+00:00"))


def text_of(content):
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        return "".join(b.get("text", "") for b in content if isinstance(b, dict) and b.get("type") == "text")
    return ""


def is_human(origin):
    return isinstance(origin, dict) and origin.get("kind") == "human"


def in_hikaru(d):
    cwd = d.get("cwd") or ""
    return cwd == HIKARU or cwd.startswith(HIKARU + "/")


def pictures(content, t, uuid=None):
    """発言に貼られた画像。[(保存名, 種類, base64 の文字列)]。保存名は 日(hikaru_day)/HHMMSS_uuid先頭4_番号.拡張子。"""
    if not isinstance(content, list):
        return []
    out, tag = [], (uuid or "")[:4] or "0000"
    for b in content:
        if not (isinstance(b, dict) and b.get("type") == "image"):
            continue
        s = b.get("source") or {}
        if s.get("type") != "base64" or not s.get("data"):
            continue
        ext = EXT.get(s.get("media_type"), "bin")
        out.append((f"{hikaru_day(t):%Y-%m-%d}/{t.astimezone(JST):%H%M%S}_{tag}_{len(out) + 1}.{ext}", s.get("media_type"), s["data"]))
    return out


def human_content(d, content):
    """普通の一言も列から届いた一言も、画像を含めた全体で見る。"""
    text = text_of(content).strip()
    pics = pictures(content, ts(d["timestamp"]), d.get("uuid"))
    k = len(d.get("imagePasteIds") or [])
    if pics:
        text = (text + "\n\n" + "\n".join(f"【画像: {name}】" for name, _, _ in pics)).strip()
    elif k:
        text = (text + f"\n\n【画像{k}枚（記録に残っていない）】").strip()
    return text, pics


def parse_line(d):
    """会話記録（jsonl）の一行を発言にする。発言でなければ None。hidden はここでは付けない（mark_hidden で）。
    返すもの：t（datetime）raw（元の timestamp）role（"ふ"／"Claude"）text typed（ユーザーが自分で打ったか。合言葉になりうるのはこれだけ）
             pics（[(名, 種類, data)]）uuid sid（行の sessionId。fork で複製された行も元の会話の ID を持つ）"""
    if not isinstance(d, dict) or d.get("isSidechain") or not d.get("timestamp") or not in_hikaru(d):
        return None
    role = text = None
    typed, pics = False, []
    kind = d.get("type")
    if kind == "user":
        r = d.get("toolUseResult")
        if isinstance(r, dict) and isinstance(r.get("answers"), dict) and r["answers"]:
            role = "ふ"      # Claude が選択肢つきで聞いた質問への、ユーザーの答え
            text = "\n\n".join(f"【質問への答え】{q}\n{a}" for q, a in r["answers"].items()).strip()
        elif d.get("isMeta") or d.get("isCompactSummary") or not is_human(d.get("origin")):
            return None
        else:
            role, typed = "ふ", True
            content = d.get("message", {}).get("content")
            text, pics = human_content(d, content)
    elif kind == "attachment":
        a = d.get("attachment") or {}
        if a.get("type") != "queued_command" or not a.get("humanTurn") or not is_human(a.get("origin")):
            return None
        if a.get("commandMode") not in (None, "prompt"):
            return None
        prompt = a.get("prompt") or ""
        role, typed = "ふ", True
        text, pics = human_content(d, prompt if isinstance(prompt, list) else str(prompt))
    elif kind == "assistant":
        if d.get("isApiErrorMessage"):
            return None
        role, text = "Claude", text_of(d.get("message", {}).get("content")).strip()
    if not role or not text:
        return None
    return {"t": ts(d["timestamp"]), "raw": d["timestamp"], "role": role, "text": text, "typed": typed,
            "pics": pics, "uuid": d.get("uuid"), "sid": d.get("sessionId")}


def mark_hidden(msgs, ranges=()):
    """一つの会話（ファイル内の順に並んだ発言）に、合言葉と .exclude の範囲で hidden を付ける。
    始まりの合言葉から終わりの合言葉まで（両方を含む）と、終わりの合言葉への Claude の返事は hidden。
    次にユーザーが自分で打ったところから、また記録する。戻り値：合言葉が開いたままか。"""
    closed, tail = True, False
    for m in msgs:
        if m["typed"]:
            phrase = classify_phrase(m["text"])
            if phrase == "start":
                closed, tail = False, False
            elif phrase == "end":
                closed, tail = True, True
            else:
                tail = False
        t = m["t"]
        m["hidden"] = (not closed) or tail or any(a <= t and (b is None or t <= b) for a, b in ranges)
    return not closed


# ---------- 会話の短い名 ----------

def load_sessions():
    """{短い名: 完全な sessionId}"""
    try:
        return json.loads(SESSIONS.read_text(encoding="utf-8")) if SESSIONS.exists() else {}
    except Exception:
        return {}


def save_sessions(table):
    TM.mkdir(parents=True, exist_ok=True)
    tmp = SESSIONS.with_suffix(".tmp")
    tmp.write_text(json.dumps(table, ensure_ascii=False, indent=0, sort_keys=True), encoding="utf-8")
    os.replace(tmp, SESSIONS)


def short_sid(full, table):
    """完全な sessionId の短い名。表にあればそれ、なければ衝突しない最短の先頭（3 文字から）を決めて表に足す。決めた名は変えない。"""
    if not full:
        return "----"
    for k, v in table.items():
        if v == full:
            return k
    k = 3
    while full[:k] in table and k < len(full):
        k += 1
    table[full[:k]] = full
    return full[:k]


# ---------- 進捗 ----------

def load_progress():
    """朝の仕事の進捗（.タイムマシン/.progress.json）。どれも「どこまで済んだか」で持つ。日の単位では持たない
    （日記を書いたあとで、その日のファイルに未明の会話が足されることがあるから）。
      diary      {日: {"until": その日のファイルで日記に書いた最後の発言の時刻, "n": そのときの発言の数}}
      shiori     同じ形。その日の日記の「問いたいこと」「候補」を、しおりに移したところ
      index_seen 夕顔で見た、保存の位置（.last と同じ形の時刻）
      read_ids   読んだひとときの ID
    「status が見せたときの様子」の控えは、ここには置かない。会話ごとの作業フォルダに置く（fuutou の asked_path）。
    ここに一つだけ置くと、別の会話が status を実行したとき、その新しい様子で「済」が記されてしまう。"""
    base = {"diary": {}, "shiori": {}, "index_seen": None, "read_ids": []}
    try:
        if PROGRESS.exists():
            d = json.loads(PROGRESS.read_text(encoding="utf-8"))
            for k in base:
                if d.get(k) is not None:
                    base[k] = d[k]
    except Exception:
        pass
    return base


def save_progress(p):
    TM.mkdir(parents=True, exist_ok=True)
    p = {"diary": dict(sorted((p.get("diary") or {}).items())), "shiori": dict(sorted((p.get("shiori") or {}).items())),
         "index_seen": p.get("index_seen"), "read_ids": sorted(set(p.get("read_ids") or []))}
    tmp = PROGRESS.with_suffix(".tmp")
    tmp.write_text(json.dumps(p, ensure_ascii=False, indent=1), encoding="utf-8")
    os.replace(tmp, PROGRESS)


# ---------- 作業場所・表示 ----------

def work_dir():
    """平文を一時的に置く作業フォルダ（ひかるの外）。HIKARU_WORK があればそこ、なければ会話ごとの scratchpad、なければ TMPDIR/hikaru_work。
    open と close は別のプロセスなので、場所は環境から決まるものだけを使う（プロセス ID は使わない）。"""
    env = os.environ.get("HIKARU_WORK")
    if env:
        base = Path(env)
    else:
        sid = os.environ.get("CLAUDE_CODE_SESSION_ID", "")
        base = Path(f"/private/tmp/claude-{os.getuid()}/{os.environ.get('HIKARU_PROJECT_SLUG', 'hikaru-memory')}/{sid}/scratchpad") if sid else None
        if base is None or not base.parent.exists():
            base = Path(os.environ.get("TMPDIR", "/tmp")) / "hikaru_work"
    d = base / "hikaru"
    d.mkdir(parents=True, exist_ok=True)
    try:
        os.chmod(d, 0o700)
    except OSError:
        pass
    return d


def pages(text, limit=PAGE_BYTES):
    """長い文字列を、行の切れ目でページに分ける。"""
    out, cur, size = [], [], 0
    for line in text.split("\n"):
        b = len(line.encode("utf-8")) + 1
        while b > limit:                      # 1行がとても長いとき
            cut = limit // 4
            piece, line = line[:cut], line[cut:]
            if cur:
                out.append("\n".join(cur)); cur, size = [], 0
            out.append(piece)
            b = len(line.encode("utf-8")) + 1
        if size + b > limit and cur:
            out.append("\n".join(cur)); cur, size = [], 0
        cur.append(line); size += b
    if cur:
        out.append("\n".join(cur))
    return out or [""]


def show_page(text, n):
    ps = pages(text)
    if not 1 <= n <= len(ps):
        print(f"ページは 1〜{len(ps)} です。")
        return
    print(ps[n - 1])
    if len(ps) > 1:
        nxt = f"続きは --page {n + 1}" if n < len(ps) else "これで最後"
        print(f"\n［ページ {n}/{len(ps)}。{nxt}］")
