#!/usr/bin/env python3
"""Optional associative recall hook; delivery can be disabled without deleting records."""
import json
import os
import subprocess
import sys
import time
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
from _tool_data import ConversationLogError, conversation_rows, now_local

FILL_EVERY = 1200    # 裏で索引に足すのを起こす間隔の下限（秒）


def conversation_context(transcript, prompt):
    """Return (unrecorded, public history) from one complete transcript snapshot.

    Decode before interpreting phrases; mark the whole session before selecting
    recent messages. An unreadable prefix cannot establish a public interval.
    Nothing from this snapshot is persisted here.
    """
    import _hk
    if _hk.classify_phrase(prompt) is not None:
        return True, []
    try:
        msgs = []
        for number, _, data in conversation_rows(transcript):
            # Queued text blocks have the same text semantics as user blocks.
            # Keep all human/source/sidechain checks in the existing parser.
            try:
                m = _hk.parse_line(data)
            except (ValueError, TypeError, KeyError, AttributeError) as error:
                raise ConversationLogError(f"{transcript}:{number}: {type(error).__name__}: {error}") from None
            if m:
                msgs.append(m)
        phrase_open = _hk.mark_hidden(msgs)
    except (OSError, ValueError, TypeError, KeyError, AttributeError) as error:
        print(f"{type(error).__name__}: {error}", file=sys.stderr)
        return True, []
    if phrase_open or (not prompt.strip() and msgs and msgs[-1]["hidden"]):
        return True, []
    last = msgs[-1]["text"].strip() if msgs and msgs[-1]["role"] == "ふ" else None
    if last is not None and prompt.strip() and (last == prompt.strip() or last.startswith(prompt.strip() + "\n\n【画像")):
        msgs = msgs[:-1]
    return False, [m for m in msgs if not m["hidden"]]


def unrecorded(transcript, prompt):
    return conversation_context(transcript, prompt)[0]


def recent_talk(transcript, prompt, n):
    """Select the last n eligible messages, in file order, after privacy filtering."""
    return conversation_context(transcript, prompt)[1][-n:] if n > 0 else []


def deliver_line(c):
    """会話に添える一行。"""
    who = "わたし" if c.get("who") == "Claude" else "ユーザー" if c.get("who") == "ふ" else "記録"
    try:
        import _tomonari_koe
        mark = _tomonari_koe.layer0_mark_for(c["snippet"])
    except Exception:
        mark = ""
    return (f"前に「{c['alias']}」が出てきたところ（{c['day']}・{who}）：「{c['snippet']}」{mark}"
            "（手もとに置くだけ。使っても使わなくてもいい。どこから来たかは話に出さない）")


def slim(c):
    if not c:
        return None
    return {k: c.get(k) for k in ("sha", "kind", "raw", "importance", "channels", "day")}


def note(entry):
    import _koyori
    import _history
    _history.append_history(_koyori.DIR / "shadow.jsonl.b64", entry)


@dataclass(frozen=True)
class RecallPlan:
    rebuild: bool
    available: bool
    recent: list[str]
    cutoff: datetime | None


def plan_recall(cat, sig, talk, recent):
    import _koyori
    available = isinstance(cat, dict)
    selected = talk[-recent:]
    return RecallPlan(not (available and _koyori._fresh(cat, sig)), available,
                      [m["text"] for m in selected], selected[0]["t"] if selected else None)


def fill_needed(cat, rows, kinds):
    return isinstance(cat, dict) and any(e["sha"] not in rows for e in cat["entries"] if e["kind"] in kinds)


def launch_later(stamp, interval, needed):
    try:
        modified = stamp.stat().st_mtime
    except FileNotFoundError:
        pass
    else:
        if time.time() - modified < interval:
            return
    if not needed():
        return
    stamp.touch()
    subprocess.Popen([sys.executable, "-B", str(HERE / "_koyori.py")], stdin=subprocess.DEVNULL,
                     stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, start_new_session=True, close_fds=True)


def rebuild_later():
    """目録が古いとき、裏で _koyori.py を起こして作り直させる（待たない。起こすのは一分に一度まで）。"""
    import _koyori
    launch_later(_koyori.DIR / ".rebuild.stamp", 60, lambda: True)


def fill_later(cat):
    """こよりの自分の種類で、意味の索引にないものがあれば、裏で _koyori.py を起こす。目録は渡されたもの（作り直さない）。"""
    import _embed
    import _koyori
    launch_later(_koyori.DIR / ".fill.stamp", FILL_EVERY,
                 lambda: fill_needed(cat, _embed.Index().rows, _koyori.OWN_INDEX_KINDS))


def delivery(candidate):
    return {"suppressOutput": True, "hookSpecificOutput": {
        "hookEventName": "UserPromptSubmit", "additionalContext": deliver_line(candidate)}}


def main():
    data = json.load(sys.stdin)
    prompt = str(data.get("prompt") or "")
    private, talk = conversation_context(data.get("transcript_path") or "", prompt)
    if private:
        return
    import _taguri
    import _koyori
    cat = _koyori.read_rebuildable_json(_koyori.CATALOG)   # 作り直しは待たない（古ければ裏で作り直させて、今回は手もとの目録で）
    plan = plan_recall(cat, _koyori.signature() if isinstance(cat, dict) else None, talk, _taguri.RECENT)
    if plan.rebuild:
        rebuild_later()
    t1 = time.perf_counter()
    tl = (_taguri.timeline(prompt, plan.recent, cutoff=plan.cutoff, catalog=cat) if plan.available
          else {"kind": "unavailable", "failure": "RuntimeError: catalog unreadable"})
    tl_sec = round(time.perf_counter() - t1, 3)
    if tl.get("kind") == "offer" and (_koyori.DIR / "deliver.on").exists():
        print(json.dumps(delivery(tl["candidates"][0]), ensure_ascii=False))
        tl["delivered"] = True
    t0 = time.perf_counter()
    r = _taguri.taguri(prompt, catalog=cat, core=False) if plan.available else {"kind": "unavailable", "failure": "catalog unreadable"}
    _taguri.check(r)
    ck = r.get("checked") or {}
    entry = {"at": now_local(), "sid": str(data.get("session_id") or "")[:3],
             "version": _taguri.VERSION, "kind": r["kind"], "why": ck.get("why"), "failure": r.get("failure"),
             "scope": ck.get("scope"), "n_pool": ck.get("n_pool"), "n_passed": ck.get("n_passed"),
             "skipped_no_text": ck.get("skipped_no_text"), "diag": ck.get("diag"), "top": slim(ck.get("top")),
             "offer": slim((r.get("candidates") or [None])[0]), "seconds": round(time.perf_counter() - t0, 3),
             "timeline": {"version": (tl.get("checked") or {}).get("version"), "kind": tl.get("kind"),
                          "why": (tl.get("checked") or {}).get("why"), "failure": tl.get("failure"),
                          "offer": slim((tl.get("candidates") or [None])[0]), "n_past": (tl.get("checked") or {}).get("n_past"),
                          "delivered": bool(tl.get("delivered")), "seconds": tl_sec}}
    note(entry)
    fill_later(cat)


def run_hook():
    try:
        main()
    except Exception:
        pass


if __name__ == "__main__":
    run_hook()
    sys.exit(0)
