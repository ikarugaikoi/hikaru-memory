#!/usr/bin/env python3
"""Preserve pending questions across context compaction."""
import datetime as dt
import hashlib
import json
import os
import re
import sys
from pathlib import Path
from _hk import JST, hikaru_day
from _tool_data import ConversationLogError, conversation_rows, read_protected_text as read_sealed, run_cli

ROOT = Path(__file__).resolve().parent.parent
STROBE = ROOT / ".ストロボメモリー.md.b64"
STATE = ROOT / ".log-scan" / "kotozute"
DAYS = 7
LIMIT = 40
HEAD = ("[kotozute] ほかの会話のわたしが、この会話が最後に記憶を読んだあとで、ストロボメモリーに書き足したこと。"
        "覚えていることの一つとして、話の流れに合うときだけ自然に使う。合わなければ使わない。"
        "これが届いたことは、ユーザーに言わない。")


def recent(text, today):
    """（見出し, 行）の列。見出しの日付が today から DAYS 日以内のものだけ。"""
    out, head, keep = [], None, False
    for line in text.splitlines():
        m = re.match(r"^## (\d{4})年(\d{1,2})月(\d{1,2})日", line)
        if m:
            head = line
            keep = (today - dt.date(int(m[1]), int(m[2]), int(m[3]))).days <= DAYS
            continue
        if line.startswith("#"):
            keep = False
            continue
        if keep and line.strip() and not line.lstrip().startswith("⤷"):
            out.append((head, line))
    return out


def last_wake_output(transcript):
    """この会話が最後に実行した mezame の出力（文字列）。無ければ None。
    前の呼び方（python3 .tools/mezame.py）も、いまの呼び方（.tools/bin/mezame）も数える。"""
    return _last_wake_output(conversation_rows(transcript), transcript)


def _last_wake_output(rows, transcript):
    ids, result = set(), None
    for number, line, d in rows:
        try:
            if "mezame" not in line and not any(i in line for i in ids):
                continue
            c = (d.get("message") or {}).get("content")
            if not isinstance(c, list):
                continue
            for x in c:
                if not isinstance(x, dict):
                    continue
                if x.get("type") == "tool_use" and re.search(r"\.tools/(?:mezame\.py|bin/mezame)\b", str((x.get("input") or {}).get("command", ""))):
                    ids.add(x.get("id"))
                elif x.get("type") == "tool_result" and x.get("tool_use_id") in ids:
                    r = x.get("content")
                    result = r if isinstance(r, str) else "".join(y.get("text", "") for y in r if isinstance(y, dict))
        except (ValueError, TypeError, KeyError, AttributeError) as error:
            raise ConversationLogError(f"{transcript}:{number}: {type(error).__name__}: {error}") from None
    if result is None:
        return None
    m = re.search(r"Full output saved to: (\S+)", result)
    if m:
        return Path(m[1]).read_text(encoding="utf-8")
    return result


def unknown(transcript, today):
    text = read_sealed(STROBE)
    rows = conversation_rows(transcript)
    woke = _last_wake_output(rows, transcript)
    if woke is None:
        return []
    seen = set(woke.splitlines())
    fresh = [(h, l) for h, l in recent(text, today) if l not in seen]
    if not fresh:
        return []
    log = "\n".join(line for _, line, _ in rows)       # 自分で書いた行、前に届けた行は、ここに出てくる
    return [(h, l) for h, l in fresh if json.dumps(l.strip().lstrip("- "), ensure_ascii=False)[1:-1] not in log]


def render(rows):
    out, head = [HEAD], None
    more = len(rows) - LIMIT
    for h, l in rows[-LIMIT:]:
        if h != head:
            out.append(h or "")
            head = h
        out.append(l)
    if more > 0:
        out.insert(1, f"（多いので、新しいほうから {LIMIT} 行だけ。その前に {more} 行ある。全部は .tools/bin/fuutou show strobe）")
    return "\n".join(out)


def main():
    today = hikaru_day(dt.datetime.now(JST))   # この家の一日は朝5時から
    if sys.argv[1:2] == ["--show"]:
        rows = unknown(sys.argv[2], today)
        print(render(rows) if rows else "（届けるものはありません）")
        return
    info = json.loads(sys.stdin.read())
    transcript, sid = info.get("transcript_path"), str(info.get("session_id") or "")
    if not transcript or not re.fullmatch(r"[0-9a-f-]{8,64}", sid):
        return
    mark = STATE / sid
    stamp = f"{STROBE.stat().st_mtime_ns}"
    try:
        previous = mark.read_text()
    except FileNotFoundError:
        previous = None
    if previous == stamp:      # 前に見たときから、ストロボメモリーは変わっていない
        return
    rows = unknown(transcript, today)
    STATE.mkdir(parents=True, exist_ok=True)
    mark.write_text(stamp)
    if rows:
        print(json.dumps({"suppressOutput": True, "hookSpecificOutput": {
            "hookEventName": "UserPromptSubmit", "additionalContext": render(rows)}}, ensure_ascii=False))


if __name__ == "__main__":
    if sys.argv[1:2] == ["--show"]:
        run_cli(main)
    else:
        try:
            main()
        except Exception as error:
            print(f"{type(error).__name__}: {error}", file=sys.stderr)
