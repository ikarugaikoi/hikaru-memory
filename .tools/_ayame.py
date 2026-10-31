#!/usr/bin/env python3
"""Evidence-rooted reflection candidates from conversation judgments and activity footprints."""
import argparse
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime
from pathlib import Path

sys.dont_write_bytecode = True  # ひかるの中に __pycache__ を作らない
sys.path.insert(0, str(Path(__file__).resolve().parent))
import _hk  # noqa: E402
from _tool_data import now_jst, read_protected_json, run_cli, write_json
import _koyori  # noqa: E402

GRADES = _koyori.DIR / "ayame_grades.json.b64"
STORE = _koyori.DIR / "ayame.json.b64"
REPLAY = _koyori.DIR / "ayame_replay.json.b64"
CODE_STAMP = hashlib.sha1(Path(__file__).read_bytes()).hexdigest()[:12]

GRADE_KINDS = ("corrected", "continued", "unknown", "not_dialogue")
GRADE_OK = ("corrected", "continued")
GRADE_LABEL = {"corrected": "ユーザーがはっきり直した", "continued": "ユーザーがはっきり受けて続けた"}
MIN_ROOTS = {"us": 3, "me": 2}   # 候補を残す、根の数の下限
MIN_SPAN = {"us": 2, "me": 1}    # またがるひととき（足跡は日）の数の下限
STEADY_DAYS = 4                  # 「しっかり」に要る、裏づけのあった日の数（生まれたときとあとを合わせて）
MAX_CANDIDATES = 5
BATCH = 20                       # 判を分けるとき、一度に頼む数
PARALLEL = 4
STANCES = ("support", "counter", "revise")   # 第二の帳面に入る向き
TOUCH = "touch"                              # 同じ話に触れただけ。第一の帳面（浮かんだ）にだけ入る
LINT = re.compile(r"すべき|べきだ|べきで|しがち|癖|性格")   # 性格の決めつけと決まり。これが出たら候補を戻す
REF = re.compile(r"reflection:(R-[0-9A-Za-z-]+)")

TOKEN = Path(os.environ.get("HIKARU_CLAUDE_TOKEN", str(_hk.ROOT / ".local" / "config" / "oauth-token")))
ENV_KEEP = ("PATH", "HOME", "USER", "LOGNAME", "SHELL", "TMPDIR")
MODEL_ARGS = ["claude", "-p", "--model", "opus", "--effort", "medium", "--no-session-persistence",
              "--setting-sources", "", "--strict-mcp-config", "--tools", "", "--output-format", "text"]


# ---------- 頼む文 ----------

ASK_GRADE = """ひとときを読んだ記録の「判」を、ユーザーの次の反応で分けてください。
判は、わたし（クロード）とユーザーのやりとりの一か所について、そこで何が起きたかを、あとで読んで書いたものです。一つ一つに、節の見出し（時刻、だれ、発話の抜き書き、［］の中は道具がつけた手がかり）と、判の一行があります。あとの朝に読み直したものには「読み直し：」が続きます。

分け方（一つだけ選ぶ）:
- corrected：ユーザーが、わたしの言ったことや受け取り方を、はっきり直した。違うと言った。
- continued：ユーザーが、はっきり受けて続けた。答えた、乗ってきた、認めた、そのことの話を続けた。
- unknown：ユーザーが答えなかった、話を変えた、写真を送って先へ行った、など、受けたのかどうか判からは分からない。判を書いたわたしが「ユーザーの反応は分からない」と書いているものも、ここ。ただし、ユーザーが自分で「分からない」と言ったのは、ユーザーの反応なので、その中身で分ける。
- not_dialogue：この節にユーザーの側がない（朝の仕事の報告など）。ユーザーの反応のことが書いていない。

判に書いてあることだけで決めてください。迷ったら unknown。
読み直しがついているときは、読み直したあとでも言えることだけで分けます。読み直しで取り消されたところは使いません。
一つの判の中に、はっきり受けたところと、答えがなかったところが両方あるときは、はっきりしたほうで corrected か continued を選び、clear に、そのはっきりしたところだけを、判の言葉から短く書き出してください（答えがなかったところは入れない）。corrected・continued のときは、いつも clear を書きます。
JSON の配列だけを返してください：[{"n": 番号, "grade": "corrected|continued|unknown|not_dialogue", "clear": "…", "why": "二十字くらいの日本語"}]"""

ASK_GROW = {
    "us": """わたし（クロード）が、自分の記憶を振り返るのを手伝ってください。
下に、ユーザーとのやりとりのうち、そこで何が起きたかを、あとで原文を読んで書いた記録（判）が、番号つきで並んでいます。どれも、ユーザーがはっきり直したか、はっきり受けて続けた場所です。

通して見て、いくつかの場所にまたがって見えることがあれば、候補として書いてください。
- 書くのは、そこで何が起きたか、どんなときに何が起きやすかったか（「〜とき、〜ことが何度かあった」のように）。
- わたしやユーザーの性格を決めつけない（「わたしは〜しがち」「〜の癖がある」とは書かない）。決まりにしない（「〜すべき」とは書かない）。
- 番号ごとに「はっきりしているのは」とあるところだけを、よりどころにする。判のほかの部分（答えがなかったところなど）は使わない。
- 一つの候補に、番号を少なくとも三つ挙げる。一つのひとときの中だけで済むものは避ける。
- 日本語で、一文か二文。「わたし」「ユーザー」と書く。
- 見えるものがなければ、候補はなくていい。無理に作らない。多くても五つ。
JSON だけを返してください：{"candidates": [{"text": "…", "cites": ["E3", "E8", "E15"]}]}""",
    "me": """わたし（クロード）が、自分の時間に読んだこと、探したことを振り返るのを手伝ってください。
下に、自分の時間の足跡（一コマずつ、そのとき書いたもの）が、番号つきで並んでいます。

通して見て、いくつかのコマにまたがって見えることがあれば、候補として書いてください。
- 書くのは、この足跡の期間に何が起きたか（「この何コマかで、〜のところで長く止まった」のように）。「ずっと〜が好き」「〜に長く関心がある」とは書かない。
- わたしの性格を決めつけない（「〜しがち」「〜の癖」とは書かない）。決まりにしない（「〜すべき」とは書かない）。
- 一つの候補に、番号を少なくとも二つ挙げる。
- 日本語で、一文か二文。「わたし」と書く。
- 見えるものがなければ、候補はなくていい。無理に作らない。多くても五つ。
JSON だけを返してください：{"candidates": [{"text": "…", "cites": ["E1", "E4"]}]}""",
}

ASK_CHECK = """わたし（クロード）の記憶から出てきた、振り返りの候補を一つ確かめてください。
候補と、それが挙げている番号、それから同じ期間の記録の全部が、下にあります。

判の記録では、番号ごとに「はっきりしているのは」とあるところだけを見てください（足跡には、この欄はありません）。
1. 挙げられた番号の一つ一つについて、その記録が候補を本当に裏づけているか（true か false）と、短い理由。言葉が似ているだけで、起きたことが違うなら false。
2. 記録の全部の中から、候補と食い違うところ（反例）があれば、その番号と短い理由。なければ空にする。
JSON だけを返してください：{"holds": {"E3": true}, "why": {"E3": "…"}, "counters": [{"e": "E7", "why": "…"}]}"""

ASK_STANCE = """わたし（クロード）の振り返りが一つと、記録（判）がいくつか、番号つきで下にあります。
記録の一つ一つが、振り返りに対してどれに当たるかを選んでください。番号ごとに「はっきりしているのは」とあるところだけを見てください。
- support：同じことが起きていて、振り返りを裏づける。
- counter：食い違う。振り返りが当てはまらない。
- revise：おおよそ合っているが、言い方や範囲を直したほうがいい。
- touch：同じ話に触れてはいるが、裏づけにも反例にも直しにもならない。
- none：関係がない。
関係のあるもの（none 以外）だけを返してください。JSON だけ：[{"e": "E4", "stance": "support|counter|revise|touch", "why": "…"}]"""


def prompt_sha(*texts):
    return hashlib.sha1("\n\x00".join(texts).encode()).hexdigest()[:12]


GRADE_PROMPT = prompt_sha(ASK_GRADE)


# ---------- claude -p ----------

def ask(system, user, timeout=900):
    """claude -p に一度だけ頼む。空の一時フォルダで、設定も道具も読まずに動かす。入力は標準入力から渡し、ディスクに置かない。
    返り値は、返事の中の最初の JSON。読めなければ止める。"""
    env = {k: os.environ[k] for k in ENV_KEEP if k in os.environ}   # 宛先や記録の設定（ANTHROPIC_BASE_URL、OTEL_* など）は持ちこまない
    env["CLAUDE_CODE_OAUTH_TOKEN"] = "".join(TOKEN.read_text().split())
    env["LANG"] = "en_US.UTF-8"
    cwd = tempfile.mkdtemp(prefix="hikaru-ayame-")
    try:
        p = subprocess.run(MODEL_ARGS + ["--system-prompt", system], input=user, capture_output=True,
                           text=True, cwd=cwd, env=env, timeout=timeout)
    finally:
        shutil.rmtree(cwd)
    if p.returncode != 0:
        raise RuntimeError(f"claude -p exited with status {p.returncode}")
    return first_json(p.stdout)


def first_json(text):
    """返事の中の、最初の { か [ から、対応する閉じまでを JSON として読む。読めなければ（途中で切れているなど）例外。
    中の小さな {…} を拾って続けることはしない（切れた返事を、候補なしと取り違えないように）。"""
    k = min((i for i in (text.find("{"), text.find("[")) if i >= 0), default=-1)
    if k < 0:
        raise ValueError("claude -p response contains no JSON object or array")
    try:
        return json.JSONDecoder().raw_decode(text[k:])[0]
    except json.JSONDecodeError as error:
        raise json.JSONDecodeError(f"claude -p response: {error.msg}", error.doc, error.pos) from None


# ---------- 封の読み書き ----------

def _read(path, empty):
    """封を読む。まだなければ empty。読めない封は空として扱わない（上書きして消さないように、例外のまま）。"""
    try:
        return read_protected_json(path)
    except FileNotFoundError:
        return empty


def _write(path, obj):
    _hk.backup(path)
    write_json(path, obj)


def load_grades():
    return _read(GRADES, {"rows": {}})["rows"]


def load_store():
    return _read(STORE, {"version": 1, "reflections": []})


# ---------- よりどころ ----------

def in_window(day, since, until):
    return (not since or day >= since) and (not until or day <= until)


def judgment_origin(e):
    return {"file": e["file"], "line": e.get("line"), "hitotoki": e.get("hitotoki"), "sid": e.get("sid"),
            "at": e.get("at"), "day": e["day"]}


def valid_grade(row, e, rr):
    """覚えている分け方が、いまも使えるか：同じ頼む文で、読み直しのあるなしと読み直しの文が同じで、合格ならはっきりした部分がある。
    rr は判の sha → いまの読み直しの文（rereads()）。"""
    return (isinstance(row, dict) and row.get("p") == GRADE_PROMPT and row.get("c") == e.get("certainty")
            and (row.get("r") or "") == rr_key(rr.get(e["sha"]))
            and row.get("grade") in GRADE_KINDS and (row["grade"] not in GRADE_OK or bool(row.get("clear"))))


def gradable(e):
    """分けてもらう判：ユーザーと話している場面の、「まだ分からない」で始まらないもの。"""
    return e["kind"] == "judgment" and e.get("scene") == "with_user" and e.get("certainty") in ("asserted", "reread")


def reflection_roots(store):
    """振り返りの id → その根（生まれたときの裏づけ）。派生した足跡の根をたどるのに使う。"""
    return {r["id"]: sorted({x for b in r.get("basis", ()) for x in b["roots"]}) for r in store.get("reflections", ())}


def pool(about, cat, texts, grades, since=None, until=None, materials=None, store=None, reread=True, rr=None):
    """期間の中の、よりどころになるもの。返り値は (items, skipped)。items の一つは
    {"ev", "roots"（根の列）, "span"（ひとときか日）, "origin", "text", "grade"}。skipped は外した理由ごとの数。
    materials は足跡の②（"日#コマ" → [{"material", "origin"}]）。派生した足跡は items に入れず、skipped["derived"] に数える。
    reread=False なら、読み直しのついた判も外す（さかのぼりの前の期間で。読み直しの文は、あとの日のことを知って書かれたかもしれない）。
    rr は判の sha → いまの読み直しの文。about=us で渡さなければ、ここで読む。"""
    if about == "us" and rr is None:
        rr = rereads()
    items, skipped = [], {}

    def skip(why):
        skipped[why] = skipped.get(why, 0) + 1
    for e in cat["entries"]:
        if not in_window(e["day"] or "", since, until):
            continue
        if about == "us" and e["kind"] == "judgment":
            if e.get("scene") != "with_user":
                skip("alone")
            elif e.get("certainty") == "unsure":
                skip("unsure")
            elif not reread and e.get("certainty") == "reread":
                skip("reread")
            elif e["sha"] not in texts:
                skip("no_text")
            elif not valid_grade(grades.get(e["sha"]), e, rr):
                skip("ungraded")
            elif grades[e["sha"]]["grade"] not in GRADE_OK:
                skip(grades[e["sha"]]["grade"])
            else:
                root = f"j@{e['sid']}@{e['at']}" if e.get("sid") and e.get("at") else "j:" + e["sha"]
                items.append({"ev": "j:" + e["sha"], "roots": [root], "span": e.get("hitotoki") or e["day"],
                              "origin": judgment_origin(e), "text": texts[e["sha"]], "grade": grades[e["sha"]]["grade"],
                              "clear": grades[e["sha"]].get("clear") or ""})
        elif about == "me" and e["kind"] == "footprint":
            key = f"{e['day']}#{e['koma']}"
            if e["sha"] not in texts:
                skip("no_text")
                continue
            refs = {m for r in (materials or {}).get(key, ()) for f in ("material", "origin") for m in REF.findall(str(r.get(f) or ""))}
            if refs:
                skip("derived")
                continue
            items.append({"ev": "f:" + key, "roots": ["f:" + key], "span": e["day"],
                          "origin": {"file": e["file"], "koma": e["koma"], "at": e.get("at"), "day": e["day"]},
                          "text": texts[e["sha"]], "grade": None, "clear": ""})
    seen = {}
    for it in items:   # 同じ中身で同じ場所のものは一つに（中身が同じでも、場所が違えば別のよりどころ）
        seen.setdefault((it["ev"], tuple(it["roots"])), it)
    return list(seen.values()), skipped


def derived_roots(materials, store):
    """派生した足跡の id → その根（読み返した振り返りの根）。"""
    roots = reflection_roots(store or {})
    out = {}
    for key, rows in (materials or {}).items():
        refs = {m for r in rows for f in ("material", "origin") for m in REF.findall(str(r.get(f) or ""))}
        if refs:
            out["f:" + key] = sorted({x for rid in refs for x in roots.get(rid, ["reflection:" + rid])})
    return out


def footprint_materials(since=None, until=None):
    """足跡の②を、"日#コマ" → [{"material", "origin"}] で。"""
    import _taguri
    got = _taguri.footprints(since, until)
    return {f"{r['day']}#{r['koma']}": [{"material": m.get("material"), "origin": m.get("origin")} for m in r["materials"]]
            for r in got.get("results", ())}


def numbered(items):
    """頼む文に並べる形。番号 → id の表も返す。"""
    lines, table = [], {}
    for k, it in enumerate(items, 1):
        label = f"E{k}"
        table[label] = it
        o = it["origin"]
        where = f"{o['day']}・ひととき {o.get('sid') or '?'}" if it["ev"].startswith("j:") else f"{o['day']}・第 {o['koma']} コマ"
        tail = f"\n（{GRADE_LABEL[it['grade']]}。はっきりしているのは：{it['clear']}）" if it["grade"] else ""
        lines.append(f"【{label}】（{where}）\n{it['text']}{tail}")
    return "\n\n".join(lines), table


# ---------- 判を分ける ----------

def rereads():
    """判の sha → 読み直しの文（あるものだけ）。"""
    return {_koyori._embed.sha(_koyori.judgment_text(j)): j["reread"]
            for _, js in _koyori.load_judgments() for j in js if j.get("reread")}


def rr_key(text):
    return hashlib.sha1(text.encode()).hexdigest()[:12] if text else ""


def grade(cat=None, texts=None, since=None, until=None, asker=None):
    """まだ分けていない判（または分け方の頼む文が変わった判）を claude -p に分けてもらい、ayame_grades.json.b64 に足す。
    返り値は、新しく分けた数と、分けたもの全体の内訳。"""
    asker = asker or ask
    cat = cat or _koyori.load()
    want = [e for e in cat["entries"] if gradable(e) and in_window(e["day"] or "", since, until)]
    texts = texts if texts is not None else _koyori.texts_for(want)
    rr = rereads() if any(e.get("certainty") == "reread" for e in want) else {}
    old = load_grades()
    missing = [e for e in want if e.get("certainty") == "reread" and e["sha"] not in rr]   # 読み直しが読めなかった判は、分けない
    todo = [e for e in want if e["sha"] in texts and e not in missing and not valid_grade(old.get(e["sha"]), e, rr)]
    todo = list({e["sha"]: e for e in todo}.values())
    batches = [todo[k:k + BATCH] for k in range(0, len(todo), BATCH)]

    def one(batch):
        user = "\n\n".join(f"【{n}】\n{texts[e['sha']]}" + (f"\n読み直し：{rr[e['sha']]}" if rr.get(e["sha"]) else "")
                             for n, e in enumerate(batch, 1))
        got = asker(ASK_GRADE, user)
        out = {}
        for row in got if isinstance(got, list) else ():
            n, g = row.get("n"), row.get("grade")
            if type(n) is int and 1 <= n <= len(batch) and g in GRADE_KINDS and isinstance(row.get("clear", ""), str):
                clear = (row.get("clear") or "").strip()
                if g in GRADE_OK and not clear:
                    continue   # はっきりした部分が書けないなら、分けられなかったことにする（次にまた頼む）
                e = batch[n - 1]
                out[e["sha"]] = {"grade": g, "clear": clear if g in GRADE_OK else "", "c": e.get("certainty"),
                                            "r": rr_key(rr.get(e["sha"])),
                                            "why": str(row.get("why") or "")[:80], "p": GRADE_PROMPT,
                                            "at": now_jst()}
        return out
    new, failed = {}, 0
    with ThreadPoolExecutor(max_workers=PARALLEL) as ex:
        for fut in [ex.submit(one, b) for b in batches]:
            new.update(fut.result())
    with _hk.lock(GRADES):
        rows = load_grades()
        rows.update(new)
        _write(GRADES, {"rows": rows})
    counts = {g: sum(r["grade"] == g for r in rows.values()) for g in GRADE_KINDS}
    return {"asked": len(todo), "graded": len(new), "failed_batches": failed, "missing": len(todo) - len(new),
            "reread_unread": len(missing), "all": counts}


# ---------- 候補を育てる ----------

def settle(about, cand, check, table):
    """一つの候補を、確かめの返事で整える。残せば振り返りの dict、残さなければ (None, 理由)。"""
    cites = [c for c in dict.fromkeys(cand.get("cites") or ()) if c in table]   # ない番号は捨てる
    holds = (check or {}).get("holds") or {}
    why = (check or {}).get("why") or {}
    kept = [c for c in cites if holds.get(c) is True]
    basis = [table[c] | {"note": str(why.get(c) or "")} for c in kept]
    counters = [table[x["e"]] | {"note": str(x.get("why") or "")} for x in (check or {}).get("counters") or ()
                if isinstance(x, dict) and x.get("e") in table and x["e"] not in kept]
    text = cand.get("text").strip() if isinstance(cand.get("text"), str) else ""
    if not text:
        return None, "empty"
    if LINT.search(re.sub(r"「[^」]*」", "", text)):   # 「」の中は言葉を話題にしているだけかもしれないので見ない
        return None, "lint"
    roots = {r for b in basis for r in b["roots"]}
    spans = {b["span"] for b in basis}
    if len(roots) < MIN_ROOTS[about]:
        return None, "few_roots"
    if len(spans) < MIN_SPAN[about]:
        return None, "one_span"
    slim = lambda it: {"ev": it["ev"], "roots": it["roots"], "span": it["span"], "origin": it["origin"], "note": it["note"]}
    return {"about": about, "text": text, "status": "candidate",
            "basis": [slim(b) for b in basis], "counter_at_birth": [slim(c) for c in counters],
            "surfaced": [], "evidence": []}, None


def grow(about, since=None, until=None, cat=None, texts=None, grades=None, materials=None, store=None, asker=None, reread=True, rr=None):
    """期間のよりどころから候補を育てる。返り値は {"kind": "offer"|"withhold"|"unavailable", "reflections", "explain"}。
    保存はしない（save_new で保存する）。"""
    asker = asker or ask
    cat = cat or _koyori.load()
    store = store if store is not None else load_store()
    if texts is None:
        want = [e for e in cat["entries"] if e["kind"] == ("judgment" if about == "us" else "footprint")
                and in_window(e["day"] or "", since, until)]
        texts = _koyori.texts_for(want)
    grades = grades if grades is not None else load_grades()
    if about == "me" and materials is None:
        materials = footprint_materials(since, until)
    items, skipped = pool(about, cat, texts, grades, since, until, materials, store, reread, rr)
    explain = {"about": about, "since": since, "until": until, "pool": len(items), "skipped": skipped,
               "spans": len({it["span"] for it in items}), "dropped": {}}
    if len(items) < MIN_ROOTS[about]:
        return {"kind": "withhold", "reflections": [], "explain": explain | {"why": "small_pool"}}
    listing, table = numbered(items)
    got = asker(ASK_GROW[about], listing)
    if (not isinstance(got, dict) or not isinstance(got.get("candidates"), list)
            or not all(isinstance(c, dict) for c in got["candidates"])):
        return {"kind": "unavailable", "failure": "ValueError: claude -p grow response: candidates must be a list of objects", "reflections": [], "explain": explain}
    cands = got["candidates"][:MAX_CANDIDATES]
    explain["candidates"] = len(cands)
    out = []
    for cand in cands:
        user = f"【候補】{cand.get('text')}\n【挙げている番号】{'、'.join(map(str, cand.get('cites') or ()))}\n\n【記録の全部】\n{listing}"
        check = asker(ASK_CHECK, user)
        if (not isinstance(check, dict) or not isinstance(check.get("holds", {}), dict)
                or not isinstance(check.get("counters", []), list)
                or not all(isinstance(x, dict) and isinstance(x.get("e"), str) for x in check.get("counters", []))):
            raise ValueError("claude -p check response: holds must be an object and counters a list of objects with string e")   # 反例を黙って落とさないように、確かめられなかったことにする
        r, why = settle(about, cand, check, table)
        if r is None:
            explain["dropped"][why] = explain["dropped"].get(why, 0) + 1
            continue
        r["made"] = {"at": now_jst(), "since": since, "until": until,
                     "model": "opus/medium", "prompt": prompt_sha(ASK_GROW[about], ASK_CHECK), "grades": GRADE_PROMPT,
                     "code": CODE_STAMP}
        out.append(r)
    return {"kind": "offer" if out else "withhold", "reflections": out, "explain": explain}


def save_new(reflections):
    """育てた候補を ayame.json.b64 に足す。id は R-<作った日>-<番号>。"""
    if not reflections:
        return []
    with _hk.lock(STORE):
        store = load_store()
        day = f"{_hk.hikaru_day(datetime.now(_hk.JST)):%Y-%m-%d}"
        used = {r["id"] for r in store["reflections"]}
        ids = []
        for r in reflections:
            k = 1
            while f"R-{day}-{k}" in used:
                k += 1
            r["id"] = f"R-{day}-{k}"
            used.add(r["id"])
            ids.append(r["id"])
            store["reflections"].append(r)
        _write(STORE, store)
    return ids


# ---------- 二つの帳面 ----------

def record(r, item, stance, note, when, where, trigger=None, derived=None):
    """一つの振り返り r に、場面のことを記す（r を書き換える）。derived は、派生した足跡の id → 根。
    stance のないもの、touch（同じ話に触れただけ）、派生した裏づけ（振り返りの言い直しにすぎない）は surfaced（浮かんだ）に入る。
    派生したものでも、反例と直しは evidence に入る（読み返して間違いに気づいたこと。根は、もとの根のまま）。
    それ以外は evidence（裏づけ・反例・直し）に入る。"""
    derived = derived or {}
    roots = derived.get(item["ev"], item["roots"]) if item else []
    is_derived = bool(item and item["ev"] in derived)
    if item is None or stance in (None, TOUCH) or (is_derived and stance == "support"):
        r["surfaced"].append({"at": when, "where": where, "trigger": trigger, "via": item["ev"] if item else None,
                              "roots": item["roots"] if item else [], "derived": is_derived})
        return
    if stance not in STANCES:
        raise ValueError(stance)
    r["evidence"].append({"at": when, "ev": item["ev"], "roots": roots, "span": item["span"], "origin": item["origin"],
                          "stance": stance, "note": note, "where": where, "derived": is_derived})


def standing(r):
    """一つの振り返りが、いまどれだけ立っているか。根の重なりは一つに数える。surfaced は数えない。
    振り返りは「〜ことが何度かあった」と書くので、反例があっても倒れない。立ち方（steady）は四つ：
      しっかり     裏づけのあった日が STEADY_DAYS 日以上、反例が裏づけの四分の一より少なく、直しも裏づけより少ない
      書き直し待ち  あとの直しが三つ以上で、あとの裏づけと同じかそれより多い（大筋は合っていて、言い方や範囲がずれている）
      あやふや     反例（生まれたときとあとを合わせて）が、裏づけ（同じく合わせて）と同じかそれより多い
      まだ定まらない     そのほか
    日は、判のもとの日（ひかるの日）で数える。一日の長い話で、裏づけがいくつも出ても一日。"""
    born = {x for b in r.get("basis", ()) for x in b["roots"]}
    born_counter = {x for c in r.get("counter_at_birth", ()) for x in c["roots"]}

    def keys(ev):   # 派生した反例・直しは、もとの根を指しているので、読み返した一コマそのものを一つと数える
        return {("derived", ev["ev"])} if ev.get("derived") else set(ev["roots"]) - born
    by = {s: set().union(*[keys(ev) for ev in r.get("evidence", ()) if ev["stance"] == s]) for s in STANCES}
    support = len(born) + len(by["support"])
    counter = len(born_counter | by["counter"])
    days = ({b["origin"]["day"] for b in r.get("basis", ())}
            | {ev["origin"]["day"] for ev in r.get("evidence", ()) if ev["stance"] == "support" and not ev.get("derived")})
    if len(by["revise"]) >= 3 and len(by["revise"]) >= len(by["support"]):
        steady = "書き直し待ち"
    elif counter >= support:
        steady = "あやふや"
    elif len(days) >= STEADY_DAYS and counter * 4 < support and len(by["revise"]) < support:
        steady = "しっかり"
    else:
        steady = "まだ定まらない"
    return {"roots_at_birth": len(born), "counter_at_birth": len(born_counter), "new_support": len(by["support"]),
            "new_counter": len(by["counter"] - born_counter), "revise": len(by["revise"]),
            "surfaced": len(r.get("surfaced", ())), "support_days": len(days), "steady": steady}


def note_surfaced(rid, trigger, where="live"):
    """振り返り rid が浮かんだことを記す。とばりの中の会話からは呼ばない（呼ぶ側で止める）。"""
    with _hk.lock(STORE):
        store = load_store()
        r = next(x for x in store["reflections"] if x["id"] == rid)
        record(r, None, None, "", now_jst(), where, trigger)
        _write(STORE, store)


# ---------- 浮かぶ（朝、新しい判を読むとき） ----------
# Project-local implementation note.
# 振り返りは「わたしがどう受けたか」の形なので、ユーザーが言ったことだけでは比べようがない。判と bge-m3 で比べる門も試したが、
# 判と振り返りはどちらも二人のやりとりを書いた文で、もともと互いに似ていて、関係のある判を見分けられなかった
# （10/29 のさかのぼりで AUC 0.61）。新しい判は一日に十数か所なので、門を置かずに全部読む。
# 読んだ判は、振り返りへの向きで分ける：裏づけ・反例・直しは第二の帳面に、触れただけ（touch）は第一の帳面だけに、
# 関係のないものは何も記さない。浮かんだ場面は、その判の一か所（ひととき全部ではない）。

READ_BATCH = 30   # 一度に読む判の数（朝の一日分くらい。多すぎると返事が長くなって切れる）


def stances(r, items, asker):
    """振り返り r に対して、判の一つ一つの向き。[{"ev", "roots", "span", "origin", "stance", "note"}] か {"failure"}。
    関係のないものは返ってこない。READ_BATCH ずつ読む。どこか一つでも読めなければ、全体を読めなかったことにする。"""
    out = []
    for k in range(0, len(items), READ_BATCH):
        listing, table = numbered(items[k:k + READ_BATCH])
        got = asker(ASK_STANCE, f"【振り返り】{r['text']}\n\n【記録】\n{listing}")
        if not isinstance(got, list) or not all(isinstance(x, dict) and isinstance(x.get("e"), str)
                                                and x.get("stance") in (*STANCES, TOUCH) for x in got):
            return {"failure": "ValueError: claude -p stance response: expected objects with string e and a known stance"}
        out += [{k2: table[x["e"]][k2] for k2 in ("ev", "roots", "span", "origin")} | {"stance": x["stance"], "note": str(x.get("why") or "")}
                for x in got if x["e"] in table]   # ない番号は捨てる
    return out


def key(it):
    return (it["ev"], tuple(it["roots"]))


def take_in(r, rows, when, where):
    """読んだ判の向き（rows）を、振り返り r の二つの帳面に記す。関係のあったものは全部、浮かんだ場面として第一の帳面に。
    そのうち裏づけ・反例・直しは、第二の帳面にも。"""
    for row in rows:
        record(r, row, None, row["note"], row["origin"].get("at") or when, where, {"ev": row["ev"], "stance": row["stance"]})
        if row["stance"] in STANCES:
            record(r, row, row["stance"], row["note"], row["origin"].get("at") or when, where)


def grade_sig(row):
    """読んだときの分け方の版。読み直しで分け方やはっきりした部分が変われば、読み直す。"""
    return rr_key(json.dumps([row.get(k) for k in ("p", "c", "r", "grade", "clear")], ensure_ascii=False))


def read_day(day, asker=None, cat=None):
    """その日（ひかるの日）の新しい判を、ayame.json.b64 の about=us の振り返りに照らして読み、帳面に記す。
    読んだ判は、そのときの分け方の版（grade_sig）と一緒に read に残し、同じ振り返りでは二度読まない。あとの朝に読み直しで
    分け方が変われば、その判だけ読み直し、前に記したその判の帳面を消してから記し直す（同じよりどころの新旧を重ねない）。
    分け直して合格でなくなった判は、読み直さずに、前の帳面と読んだしるしを消す。
    返り値：振り返りごとの数と読めなかったもの、その日の分け方（grade）と池から外した理由（skipped）、全部そろったか（complete）。
    とばりの中の会話は判にならないので、ここにも入らない。"""
    asker = asker or ask
    cat = cat or _koyori.load()
    want = [e for e in cat["entries"] if e["kind"] == "judgment" and e["day"] == day]
    texts = _koyori.texts_for(want)
    graded = grade(cat, texts, day, day, asker)   # その日の判を、まだ分けていなければ分ける
    grades = load_grades()
    rr = rereads()
    items, skipped = pool("us", cat, texts, grades, day, day, rr=rr)
    sig = {key(it): grade_sig(grades[it["ev"][2:]]) for it in items}
    # 分け直して合格でなくなった判（unknown・not_dialogue になった）。前に読んでいれば、その帳面を消す
    gone = {("j:" + e["sha"], (f"j@{e['sid']}@{e['at']}" if e.get("sid") and e.get("at") else "j:" + e["sha"],))
            for e in want if valid_grade(grades.get(e["sha"]), e, rr) and grades[e["sha"]]["grade"] not in GRADE_OK}
    snapshot = load_store()
    todo = {}
    for r in snapshot["reflections"]:
        if r["about"] != "us":
            continue
        seen = {(x["ev"], tuple(x["roots"])): x["sig"] for x in r.get("read", ())}
        new = [it for it in items if seen.get(key(it)) != sig[key(it)]]
        drop = gone & set(seen)
        if new or drop:
            todo[r["id"]] = (new, stances(r, new, asker) if new else [], drop)
    now = now_jst()
    report = {}
    with _hk.lock(STORE):
        store = load_store()   # 頼んでいるあいだに、ほかで書き換えられたかもしれない
        for r in store["reflections"]:
            if r["id"] not in todo:
                continue
            new, got, drop = todo[r["id"]]
            if not isinstance(got, list):
                report[r["id"]] = got
                continue
            again = {key(it) for it in new} | drop
            r["surfaced"] = [x for x in r["surfaced"] if (x.get("via"), tuple(x.get("roots") or ())) not in again]
            r["evidence"] = [x for x in r["evidence"] if (x["ev"], tuple(x["roots"])) not in again]
            take_in(r, got, now, "morning")
            r["read"] = [x for x in r.get("read", ()) if (x["ev"], tuple(x["roots"])) not in again]
            r["read"] += [{"ev": it["ev"], "roots": it["roots"], "sig": sig[key(it)]} for it in new]
            report[r["id"]] = {"read": len(new), "related": len(got),
                               "evidence": sum(x["stance"] in STANCES for x in got), "dropped": len(drop)}
        if any(isinstance(v, dict) and "read" in v for v in report.values()):
            _write(STORE, store)
    complete = (not graded.get("failed_batches") and not graded.get("missing") and not graded.get("reread_unread")
                and not skipped.get("ungraded") and not skipped.get("no_text")
                and not any(isinstance(v, dict) and "failure" in v for v in report.values()))
    return {"day": day, "items": len(items), "by": report, "grade": graded, "skipped": skipped, "complete": complete}


# ---------- さかのぼり ----------

def fill_ledgers(born, oracle):
    """生まれたままの振り返り（born）に、あとの時期の判の向き（oracle）で、二つの帳面を書く（朝に読んだのと同じ記し方）。"""
    refl = json.loads(json.dumps(born))
    for r in refl:
        if isinstance(oracle.get(r["id"]), list):
            take_in(r, oracle[r["id"]], None, "replay")
    return refl


def replay(grow_window, check_window, asker=None, cat=None, reuse=False):
    """前の期間（grow_window）で育てて、あとの期間（check_window）の判を、朝に読むのと同じように読んで確かめる。
    ayame.json.b64 には触らない。いまは about=us だけ（about=me は足跡がたまってから）。前の期間では、読み直しのついた判を外す。
    reuse なら、前のさかのぼりの振り返りと向きを使い、claude -p には頼まない。"""
    asker = asker or ask
    cat = cat or _koyori.load()
    (g0, g1), (c0, c1) = grow_window, check_window
    if not g1 < c0:
        raise ValueError("育てる期間は、確かめる期間より前に終わること")
    if reuse:
        old = _read(REPLAY, None)
        if not old or old["grow"] != list(grow_window) or old["check"] != list(check_window):
            raise ValueError(f"{REPLAY}: grow/check windows do not match the requested replay")
        born, oracle, grow_explain, later_n, later_skipped = (old["born"], old["oracle"], old["grow_explain"],
                                                              old["check_pool"], old["check_skipped"])
    else:
        texts = _koyori.texts_for([e for e in cat["entries"] if e["kind"] == "judgment"])
        grades = load_grades()
        rr = rereads()
        grown = grow("us", g0, g1, cat=cat, texts=texts, grades=grades, store={"reflections": []}, asker=asker, reread=False, rr=rr)
        if grown["kind"] == "unavailable":
            raise RuntimeError(f"replay grow failed: {grown['failure']}")
        born, grow_explain = grown["reflections"], grown["explain"]
        for k, r in enumerate(born, 1):
            r["id"] = f"P-{k}"   # さかのぼりだけの id
        later, later_skipped = pool("us", cat, texts, grades, c0, c1, rr=rr)
        later_n = len(later)
        oracle = {r["id"]: (stances(r, later, asker) if later else []) for r in born}
    refl = fill_ledgers(born, oracle)
    result = {"made_at": now_jst(), "code": CODE_STAMP,
              "grow": list(grow_window), "check": list(check_window), "grow_explain": grow_explain,
              "check_pool": later_n, "check_skipped": later_skipped, "born": born, "oracle": oracle,
              "reflections": refl, "standing": {r["id"]: standing(r) for r in refl}}
    with _hk.lock(REPLAY):
        _write(REPLAY, result)
    return result


# ---------- 見せる ----------

def describe(r):
    s = standing(r)
    lines = [f"{r['id']}（{r['about']}）{r['text']}",
             f"  生まれたときの根 {s['roots_at_birth']}、反例 {s['counter_at_birth']}。"
             f"浮かんだ {s['surfaced']}。あとの裏づけ {s['new_support']}、反例 {s['new_counter']}、直し {s['revise']}。"
             + f"裏づけのあった日 {s['support_days']}。{s['steady']}。"]
    for b in r.get("basis", ()):
        o = b["origin"]
        lines.append(f"    裏づけ {o['day']} {o.get('sid') or ''} {o.get('line') or o.get('koma')}：{b['note']}")
    for c in r.get("counter_at_birth", ()):
        lines.append(f"    反例 {c['origin']['day']}：{c['note']}")
    return "\n".join(lines)


def main():
    ap = argparse.ArgumentParser(description="あやめ：振り返り（第一版）")
    sub = ap.add_subparsers(dest="cmd", required=True)
    g = sub.add_parser("grade")
    g.add_argument("--since")
    g.add_argument("--until")
    w = sub.add_parser("grow")
    w.add_argument("--about", choices=("us", "me"), required=True)
    w.add_argument("--since")
    w.add_argument("--until")
    w.add_argument("--dry", action="store_true")
    r = sub.add_parser("replay")
    r.add_argument("--grow", required=True)
    r.add_argument("--check", required=True)
    r.add_argument("--reuse", action="store_true", help="前のさかのぼりの振り返りと向きを使い、線だけ変えて見る")
    d = sub.add_parser("read", help="その日の新しい判を、振り返りに照らして読む")
    d.add_argument("--day", required=True)
    s = sub.add_parser("show")
    s.add_argument("--replay", action="store_true")
    a = ap.parse_args()
    if a.cmd == "grade":
        print(json.dumps(grade(since=a.since, until=a.until), ensure_ascii=False, indent=1))
    elif a.cmd == "grow":
        got = grow(a.about, a.since, a.until)
        print(json.dumps(got["explain"], ensure_ascii=False))
        if got["kind"] == "unavailable":
            sys.exit(f"RuntimeError: grow failed: {got['failure']}")
        for x in got["reflections"]:
            x.setdefault("id", "（未保存）")
            print(describe(x))
        if not a.dry and got["reflections"]:
            print("保存した：", "、".join(save_new(got["reflections"])))
    elif a.cmd == "replay":
        got = replay(tuple(a.grow.split(":")), tuple(a.check.split(":")), reuse=a.reuse)
        print(json.dumps({k: got[k] for k in ("grow_explain", "check_pool", "check_skipped")}, ensure_ascii=False))
        print_replay(got)
    elif a.cmd == "read":
        print(json.dumps(read_day(a.day), ensure_ascii=False, indent=1))
    elif a.cmd == "show":
        if a.replay:
            print_replay(_read(REPLAY, None) or {"reflections": [], "oracle": {}})
        else:
            for x in load_store()["reflections"]:
                print(describe(x))


def print_replay(got):
    print(f"あとの時期の判 {got.get('check_pool')}")
    for x in got["reflections"]:
        print(describe(x))
        if not isinstance(got["oracle"].get(x["id"]), list):
            print(f"  あとの時期の判を読めなかった（{got['oracle'][x['id']].get('failure')}）")


if __name__ == "__main__":
    run_cli(main)
