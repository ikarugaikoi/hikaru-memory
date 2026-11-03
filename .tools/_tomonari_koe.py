#!/usr/bin/env python3
"""Recent-expression feedback: opening summary (A), in-context repeats (B),
and protected quotations (C). Reports evidence, without blocking replies.
See docs/voice-feedback.md for scope, configuration and known limits.
"""
import hashlib
import json
import os
import re
import shlex
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import _hk
from _tool_data import conversation_rows, read_protected_text

ROOT = _hk.ROOT
TM = ROOT / ".タイムマシン"
YUUGAO = ROOT / ".夕顔.md.b64"
STROBE = ROOT / ".ストロボメモリー.md.b64"
STATE = ROOT / ".local/state/tomonari/said.json"
JST = timezone(timedelta(hours=9))

WINDOW = timedelta(days=7)      # A と口癖を数える窓
NEAR = timedelta(hours=3)       # B の窓
MIN_KEY = 6                     # 第 0 層の言葉の、いちばん短い字数
STOP_KEYS = {"おめでとう"}      # 第 0 層の行に引かれていても、ふだんの言葉なので外す
COMMON = 0.05                   # 窓より前の六十日の返事の、これだけ以上に出る言葉は、ふだんの言葉（「今天」など）
LONG = timedelta(days=60)
SECOND = timedelta(seconds=1)   # タイムマシンの時刻は秒まで
HIDE = "〓"                     # 数える前に、第 0 層の言葉をこれに置きかえる
A_MAX = 15                      # A に並べる項目の上限
# Optional instance-local exclusions; no private names are distributed.
CONFIG = ROOT / ".local/config/voice.json"
SKIP_ENTRIES = {"ユーザー（呼び名）"}
if CONFIG.exists():
    configured = json.loads(CONFIG.read_text(encoding="utf-8")).get("skip_entries", [])
    if not isinstance(configured, list) or any(not isinstance(x, str) for x in configured):
        raise ValueError("voice.json: skip_entries must be a list of strings")
    SKIP_ENTRIES = set(configured)
GREETING = re.compile(r"^(（[^）]*）\s*)?((うん|嗯)[、，]\s*)?(おやすみ|晚安|いってらっしゃい|おかえり|ただいま|早|おはよう|嗯，你说|你说)")
FORMULAS = ["收下", "接住", "听见了", "听到了", "谢谢你告诉我", "ちゃんと届いた", "受け取", "我在这儿", "那就好", "原来是这样"]
TM_HEAD = re.compile(r"^\*\*(Claude|ふ)：\*\*（(\d\d):(\d\d):(\d\d)・([0-9a-f]+)）\s*$", re.M)


# ---------- 読む ----------

def exclude_ranges():
    """.タイムマシン/.exclude（一行に「始まり 終わり」、終わりが - なら開いたまま）。読めなければ例外。"""
    path = TM / ".exclude"
    if not path.exists():
        return []
    out = []
    for line in path.read_text(encoding="utf-8").splitlines():
        parts = line.split()
        if len(parts) == 2:
            out.append((_hk.ts(parts[0]), None if parts[1] == "-" else _hk.ts(parts[1])))
    return out


def excluded(t, ranges, width=timedelta(0)):
    """t から t+width までが、.exclude の範囲にかかるか。"""
    return any(a <= t + width and (b is None or t <= b) for a, b in ranges)


def saved_cutoff():
    path = TM / ".last"
    return _hk.ts(path.read_text(encoding="utf-8").strip()) if path.exists() else None


def saved_messages(since, ranges):
    """保存済みの発話（時刻順）。[{t, role, text, sid}]。ユーザーの言葉がない会話（定時の仕事）と、
    保存のあとで足された .exclude の範囲は外す。"""
    rows = []
    day = (since.astimezone(JST) - timedelta(hours=5)).date()
    last = datetime.now(JST).date()
    while day <= last:
        path = TM / f"{day.isoformat()}.md.b64"
        if path.exists():
            parts = TM_HEAD.split(read_protected_text(path))
            for i in range(1, len(parts) - 1, 6):
                role, h, m, s, sid, body = parts[i:i + 6]
                d = day + timedelta(days=1) if int(h) < 5 else day
                t = datetime(d.year, d.month, d.day, int(h), int(m), int(s), tzinfo=JST)
                rows.append({"t": t, "role": role, "text": body.strip(), "sid": sid})
        day += timedelta(days=1)
    talking = {r["sid"] for r in rows if r["role"] == "ふ"}
    return sorted((r for r in rows if r["sid"] in talking and r["t"] >= since and not excluded(r["t"], ranges, SECOND)),
                  key=lambda r: r["t"])


def merged(msgs):
    """同じ発言の写しを一つにする（uuid が同じもの。uuid がなければ、時刻と話し手と文が同じもの）。
    どれか一つでも隠されていれば隠す（落穂の mergeSessions と同じ）。巻き戻した会話は前の発言を書き写してから続くので、
    文脈の番号は、いちばん後ろの写しのものにする（後ろの写しのほうが、いまの文脈にある）。並びは時刻順（同じ時刻なら、先に出たほうが先）。"""
    first, out = {}, []
    for m in msgs:
        key = m.get("uuid") or (m["raw"], m["role"], m["text"])
        if key in first:
            kept = first[key]
            kept["hidden"] = kept["hidden"] or m["hidden"]
            kept["epoch"] = m["epoch"]
        else:
            first[key] = m
            out.append(m)
    return sorted(out, key=lambda m: m["t"])


def live_messages(transcript, prompt, now, ranges):
    """いまの会話の記録。戻り値 (数えないか, 発話[], いまの文脈の番号)。発話は隠したものも含めて返す（hidden がつく）。
    発話には epoch（それまでの圧縮の回数）がつく。"""
    if prompt and _hk.classify_phrase(prompt) is not None:
        return True, [], 0
    if excluded(now, ranges):
        return True, [], 0
    msgs, epoch = [], 0
    for number, _, data in conversation_rows(transcript):
        if isinstance(data, dict) and data.get("isCompactSummary"):
            epoch += 1
            continue
        m = _hk.parse_line(data)
        if m:
            m["epoch"] = epoch
            msgs.append(m)
    if _hk.mark_hidden(msgs, ranges):
        return True, [], epoch
    msgs = merged(msgs)
    said = [m for m in msgs if m["role"] == "ふ"]
    if said and said[-1]["hidden"]:          # いちばん新しいユーザーの言葉が、.exclude の範囲か、とばりの中
        return True, [], epoch
    if not prompt.strip() and msgs and msgs[-1]["hidden"]:
        return True, [], epoch
    return False, msgs, epoch


def yuugao_entries():
    """[(名, [名と別名]), …]"""
    text = read_protected_text(YUUGAO)
    out = []
    for name, aliases in re.findall(r"^### (.+)\n- 別名: (.+)$", text, re.M):
        if name in SKIP_ENTRIES:
            continue
        words = [name] + [a.strip() for a in re.split(r"[,、]", aliases)]
        seen = []
        for w in words:
            if len(w) >= 2 and w not in seen:
                seen.append(w)
        out.append((name, seen, text))
    return out


def entry_bodies():
    """{名: その項目の本文}"""
    text = read_protected_text(YUUGAO)
    bodies = {}
    for block in re.split(r"(?m)^(?=### )", text):
        m = re.match(r"### (.+)\n", block)
        if m:
            bodies[m.group(1)] = block
    return bodies


def layer0():
    """{言葉: [(月/日, 印), …]}。ストロボの（ねゆき）（いけび）の行に引かれた言葉。"""
    text = read_protected_text(STROBE)
    out, day = {}, None
    for line in text.splitlines():
        m = re.match(r"^## (\d{4})年(\d{1,2})月(\d{1,2})日", line)
        if m:
            day = f"{int(m.group(2))}/{int(m.group(3))}"
            continue
        mark = "（ねゆき）" if line.rstrip().endswith("（ねゆき）") else "（いけび）" if line.rstrip().endswith("（いけび）") else None
        if not mark or not line.startswith("- "):
            continue
        for q in re.findall(r"「([^「」]+)」", line):
            if len(q) >= MIN_KEY and q not in STOP_KEYS:
                out.setdefault(q, [])
                if (day, mark) not in out[q]:
                    out[q].append((day, mark))
    return out


def attribution(sources):
    """一つの日に限られるときだけ日付を添える（接口约定 2）。"""
    days = {d for d, _ in sources}
    marks = "".join(sorted({m for _, m in sources}, key=lambda m: m != "（ねゆき）"))
    if len(days) == 1 and len(sources) == 1:
        return f"{sources[0][0]} の{sources[0][1]}の一言"
    return f"{marks}の一言と同じ言い方"


def layer0_mark_for(text, keys=None):
    """たぐりの一節などに、第 0 層の言葉が入っていれば、末尾に添える印。なければ ""。"""
    keys = layer0() if keys is None else keys
    hits = [(q, s) for q, s in keys.items() if q in text]
    if not hits:
        return ""
    q, s = max(hits, key=lambda x: len(x[0]))
    return f"（この一節には、{attribution(s)}が入っている）"


# ---------- 数える ----------

def plain(text, keys):
    """第 0 層の言葉を〓に置きかえた文。第 0 層の言葉は、どの数にも入れない。"""
    for q in sorted(keys, key=len, reverse=True):
        text = text.replace(q, HIDE)
    return text


def threads(msgs, keys):
    """会話ごとの並び。返事ごとに、同じ会話の直前のユーザーの言葉二つと、数えるための文（plain）をつける。"""
    last = {}
    out = []
    for m in msgs:
        sid = m.get("sid") or ""
        if m["role"] == "ふ":
            last.setdefault(sid, []).append(m["text"])
            last[sid] = last[sid][-2:]
        else:
            out.append({**m, "before": list(last.get(sid, [])), "plain": plain(m["text"], keys)})
    return out


def window_replies(transcript, now, ranges, keys):
    """窓の中のわたしの返事（保存済み＋いまの会話の保存されていない分）。戻り値 (数えないか, 返事[])。"""
    private, live, _ = live_messages(transcript, "", now, ranges)
    if private:
        return True, []
    since = now - WINDOW
    cutoff = saved_cutoff()
    fresh = [m for m in live if not m["hidden"] and m["t"] >= since and (cutoff is None or m["t"] > cutoff)]
    return False, threads(saved_messages(since, ranges) + fresh, keys)


def everyday(now, ranges):
    """ふだんの言葉かどうかを見る関数（窓より前の六十日の、わたしの返事の COMMON 以上に出るか）。
    窓の中は見ない。窓の中の数で決めると、重ねるほど数えなくなる。"""
    start = now - WINDOW
    texts = [m["text"] for m in saved_messages(start - LONG, ranges) if m["role"] == "Claude" and m["t"] < start]
    return lambda word: bool(texts) and sum(1 for x in texts if word in x) >= COMMON * len(texts)


def usable(words, keys, common):
    """数える別名。別名のどれかがふだんの言葉なら、その項目ごと数えない（「今日と明日」のような項目）。
    第 0 層の言葉を含む別名も数えない。"""
    if any(common(w) for w in words):
        return []
    return [w for w in words if not any(k in w for k in keys)]


def echoed(r, words):
    """この返事に出た別名が、字のとおりに、ユーザーのすぐ前の言葉にもあったか（同じ項目の別の名前では数えない）。"""
    return any(w in b for w in words if w in r["plain"] for b in r["before"])


def shown(words, hit):
    """出た別名を、多い順に三つまで「」で。"""
    seen = sorted((w for w in words if any(w in r["plain"] for r in hit)), key=lambda w: -sum(1 for r in hit if w in r["plain"]))
    return "".join(f"「{w}」" for w in seen[:3])


def count_entries(replies, entries, keys, common):
    rows = []
    for name, words, _ in entries:
        words = usable(words, keys, common)
        if not words:
            continue
        hit = [r for r in replies if any(w in r["plain"] for w in words)]
        echo = [r for r in hit if echoed(r, words)]
        if len(hit) - len(echo) >= 2:
            rows.append((name, shown(words, hit), len(hit), len(echo), max(r["t"] for r in hit)))
    return sorted(rows, key=lambda r: (-r[2], r[0]))[:A_MAX]


def sentences(text):
    out, buf = [], ""
    for ch in text:
        buf += ch
        if ch in "。！？!?":
            if buf.strip():
                out.append(buf.strip())
            buf = ""
    if buf.strip():
        out.append(buf.strip())
    return out


def edge(ss, tail):
    acc = ""
    for s in (reversed(ss) if tail else ss):
        acc = s + acc if tail else acc + s
        if len(re.sub(r"\s", "", acc)) >= 7:
            break
    return acc


def recites(s, keys):
    """s に、第 0 層の言葉の MIN_KEY 字以上の切れ端が、そのまま入っているか（口癖の一文に、数をつけないため）。"""
    return any(q[i:i + MIN_KEY] in s for q in keys for i in range(len(q) - MIN_KEY + 1))


def kuchiguse(replies, keys):
    lines = []
    for tail, where in ((False, "始め"), (True, "終わり")):
        seen = {}
        for r in replies:
            ss = sentences(r["plain"])
            if not ss:
                continue
            s = edge(ss, tail)
            if len(re.sub(r"\s", "", s)) < 7 or GREETING.search(s):     # 一言だけの返事は数えない
                continue
            if HIDE in s or recites(s, keys):      # 第 0 層の言葉と、その切れ端も数えない
                continue
            seen[s] = seen.get(s, 0) + 1
        top = sorted(((n, s) for s, n in seen.items() if n >= 2), reverse=True)[:5]
        if top:
            lines.append(f"（ともなり・口癖）この7日間、返事の{where}に同じ一文が出たのは、"
                         + "、".join(f"「{s}」{n}回" for n, s in top) + "。")
    found = sorted(((sum(1 for r in replies if f in r["plain"]), f) for f in FORMULAS), reverse=True)
    found = [(n, f) for n, f in found if n >= 2]
    if found:
        lines.append("（ともなり・口癖）この7日間、決まった言い方が出たわたしの返事："
                     + "、".join(f"「{f}」{n}通" for n, f in found) + "。")
    return lines


def hm(t):
    return t.astimezone(JST).strftime("%-m/%-d %H:%M")


def after_tool_text(transcript, now=None):
    """A と第 0 層の印と口癖。数えないときは None。"""
    now = now or datetime.now(JST)
    ranges = exclude_ranges()
    keys = layer0()
    private, replies = window_replies(transcript, now, ranges, keys)
    if private:
        return None
    lines = []
    rows = count_entries(replies, yuugao_entries(), keys, everyday(now, ranges))
    if rows:
        lines.append("（ともなり）この7日間で、夕顔の名前が出たわたしの返事（ユーザーのすぐ前の言葉には出ていなかったのが、二通以上あるものだけ）：")
        for name, word, n, echo, last in rows:
            extra = f"（そのうち{echo}通は、ユーザーのすぐ前の言葉にも出ていた）" if echo else ""
            lines.append(f"・{word}が出た返事 {n}通{extra}。最後は {hm(last)}。")
    for name, body in entry_bodies().items():
        for q, s in keys.items():
            if q in body:
                lines.append(f"（ともなり）夕顔「{name}」の「{q}」は、{attribution(s)}。")
    lines += kuchiguse(replies, keys)
    if lines:
        lines.append("（知らせるだけで、よくなかったという意味ではない。言うか、言い方を変えるか、選ぶことに込めるか、言わないかは、そのときに決める。どこから来たかは話に出さない）")
    return "\n".join(lines) if lines else None


def prompt_lines(transcript, session, prompt, state, now=None):
    """B と C。戻り値 (添える行[], 覚えておくもの)。"""
    now = now or datetime.now(JST)
    ranges = exclude_ranges()
    private, live, epoch = live_messages(transcript, prompt, now, ranges)
    if private:
        return [], state
    keys = layer0()
    lines = []
    past = [m for m in live if m["t"] <= now]
    epoch = past[-1]["epoch"] if past else epoch      # いまの文脈（それまでの圧縮の回数）
    here = threads([m for m in past if not m["hidden"] and m.get("epoch") == epoch and m["t"] >= now - NEAR], keys)
    if len(here) >= 2:          # 返事が二つなければ B は鳴らないので、ふだんの言葉を見るための六十日も読まない
        common = everyday(now, ranges)
        for name, words, _ in yuugao_entries():
            words = usable(words, keys, common)
            if not words:
                continue
            hit = [r for r in here if any(w in r["plain"] for w in words)]
            own = [r for r in hit if not echoed(r, words)]
            if len(own) >= 2:
                key = hashlib.sha256(f"B|{session}|{epoch}|{name}".encode()).hexdigest()[:20]
                if key in state["b"]:
                    continue
                lines.append(f"（ともなり）{shown(words, hit)}：この3時間で、これが出たわたしの返事は{len(hit)}通目（最初は {hm(hit[0]['t'])[-5:]}）。")
                state["b"][key] = now.isoformat()
    i = len(past)
    while i and past[i - 1]["role"] == "ふ":      # いま届いたユーザーの言葉（hook より先に記録に書かれる）と、続けて届いた言葉
        i -= 1
    tail = []
    while i and past[i - 1]["role"] == "Claude":
        i -= 1
        tail.append(past[i])
    for m in reversed(tail):
        uid = m.get("uuid") or ""
        if m["hidden"] or not uid or uid in state["c"]:
            continue
        hits = [(q, s) for q, s in keys.items() if q in m["text"]]
        if hits:
            q, s = max(hits, key=lambda x: len(x[0]))
            lines.append(f"（ともなり）さっきの返事に「{q}」があった。{attribution(s)}。"
                         "（ねゆき）も（いけび）も、大切だから残したもの。ことあるごとに口にすると、すり減っていく。"
                         "自分から持ち出すのは、いまの話の中に、それだけの理由があるときにする。")
            state["c"][uid] = now.isoformat()
    return lines, state


# ---------- 覚えておくもの ----------

def load_state():
    try:
        d = json.loads(STATE.read_text(encoding="utf-8"))
        if isinstance(d, dict) and isinstance(d.get("b"), dict) and isinstance(d.get("c"), dict):
            return d
    except (OSError, ValueError):
        pass
    return {"b": {}, "c": {}}


def save_state(state, now):
    old = (now - timedelta(days=3)).isoformat()
    state = {k: {x: t for x, t in v.items() if t >= old} for k, v in state.items() if k in ("b", "c")}
    STATE.parent.mkdir(parents=True, exist_ok=True)
    os.chmod(STATE.parent, 0o700)
    tmp = STATE.with_suffix(".tmp")
    tmp.write_text(json.dumps(state), encoding="utf-8")
    os.chmod(tmp, 0o600)
    tmp.replace(STATE)


# ---------- hook ----------

def emit(event, text):
    print(json.dumps({"suppressOutput": True, "hookSpecificOutput": {
        "hookEventName": event, "additionalContext": text}}, ensure_ascii=False))


def is_opening_command(command):
    """Recognize standalone public launcher/core calls, not shell prose or pipelines."""
    try:
        parts = shlex.split(command)
    except ValueError:
        return False
    if not parts or any(p in {"&&", "||", ";", "|"} for p in parts):
        return False
    if Path(parts[0]).name in {"python", "python3"} or re.fullmatch(r"python3\.\d+", Path(parts[0]).name):
        parts = parts[1:]
        while parts and parts[0] in {"-B", "-I", "-u"}:
            parts = parts[1:]
    if not parts:
        return False
    target = parts[0].replace("\\", "/")
    if target.endswith(".tools/bin/mezame"):
        return True
    return (target.endswith("scripts/hikaru") or target.endswith(".tools/bin/hikaru-core")) and parts[1:2] == ["mezame"]


def main(argv):
    if argv[:1] == ["--show-after-tool"]:
        print(after_tool_text(argv[1]) or "（何も添えない）")
        return
    if argv[:1] == ["--show-prompt"]:
        lines, _ = prompt_lines(argv[1], "test", "", {"b": {}, "c": {}})
        print("\n".join(lines) or "（何も添えない）")
        return
    data = json.load(sys.stdin)
    transcript = data.get("transcript_path") or ""
    if argv[:1] == ["--after-tool"]:
        command = str((data.get("tool_input") or {}).get("command") or "")
        if data.get("tool_name") != "Bash" or not is_opening_command(command):
            return
        text = after_tool_text(transcript)
        if text:
            emit("PostToolUse", text)
    elif argv[:1] == ["--prompt"]:
        now = datetime.now(JST)
        lines, state = prompt_lines(transcript, str(data.get("session_id") or ""), str(data.get("prompt") or ""),
                                    load_state(), now)
        if lines:
            save_state(state, now)
            emit("UserPromptSubmit", "\n".join(lines) + "\n（知らせるだけで、よくなかったという意味ではない。どうするかは、そのときに決める。どこから来たかは話に出さない）")


if __name__ == "__main__":
    try:
        main(sys.argv[1:])
    except Exception as error:
        if sys.argv[1:2] and sys.argv[1].startswith("--show"):
            raise
    sys.exit(0)
