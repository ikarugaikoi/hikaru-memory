"""Additional synthetic checks; invoked only by the sandbox harness."""
import base64
import contextlib
from datetime import datetime, timedelta, timezone
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / ".tools"))
print("ISOLATED_CHILD_STARTED")
namespace = {"__name__": "synthetic_cases", "__file__": str(ROOT / "tests/voice_cases.py")}
try:
    exec(compile((ROOT / "tests/voice_cases.py").read_text(), "voice_cases.py", "exec"), namespace)
except SystemExit as error:
    assert error.code == 0, error.code

m = namespace["NEW"]
setup, U, A, at, jsonl = (namespace[k] for k in ("setup", "U", "A", "at", "jsonl"))
NOW, KEY = namespace["NOW"], namespace["KEY"]
checks = []


def check(name, success):
    assert success, name
    checks.append(name)


setup(m)
rows = [U(at(12, 0), "嗯", "u0"), A(at(12, 10), "星灯一。", "a1"),
        U(at(12, 20), "继续", "u1"), A(at(12, 30), "星灯二。", "a2")]
state = {"b": {}, "c": {}}
lines, state = m.prompt_lines(jsonl(rows), "synthetic", "继续", state, NOW)
again, state = m.prompt_lines(jsonl(rows), "synthetic", "继续", state, NOW)
check("B once per context", len(lines) == 1 and not again)
rows += [{"isCompactSummary": True}, U(at(13, 0), "嗯", "u2"), A(at(13, 10), "星灯三。", "a3"),
         U(at(13, 20), "继续", "u3"), A(at(13, 30), "星灯四。", "a4")]
lines, state = m.prompt_lines(jsonl(rows), "synthetic", "继续", state, NOW)
check("B resumes after compaction", len(lines) == 1)
setup(m)
rows = [U(at(13, 0), "嗯", "u0"), A(at(13, 10), KEY, "a1"), U(at(13, 20), "看看", "u1", images=1)]
lines, state = m.prompt_lines(jsonl(rows), "synthetic", "看看", {"b": {}, "c": {}}, NOW)
rows.append(U(at(13, 30), "继续", "u2"))
again, _ = m.prompt_lines(jsonl(rows), "synthetic", "继续", state, NOW)
check("C once per UUID", len(lines) == 1 and not again)
q = "我在这儿陪合成角色看测试星图"
check("protected formula is not counted", not m.kuchiguse([{"plain": m.plain(q, {q: []})}] * 2, {q: []}))

for command in ('python3 scripts/hikaru mezame', 'python3 -B "/tmp/path with spaces/scripts/hikaru" mezame',
                './.tools/bin/mezame', './.tools/bin/hikaru-core mezame'):
    check("opening command: " + command, m.is_opening_command(command))
for command in ('echo "python3 scripts/hikaru mezame"', 'python3 scripts/hikaru omoide mezame',
                'python3 scripts/hikaru mezame && echo done', 'python3 "unterminated'):
    check("other command ignored: " + command, not m.is_opening_command(command))

setup(m)
path = jsonl([U(at(12, 0), "嗯", "u0"), A(at(12, 10), "星灯一。", "a1"),
              U(at(12, 20), "继续", "u1"), A(at(12, 30), "星灯二。", "a2")])
payload = {"transcript_path": path, "tool_name": "Bash", "tool_input": {"command": "python3 scripts/hikaru mezame"}}
out = io.StringIO()
old_stdin = sys.stdin
try:
    sys.stdin = io.StringIO(json.dumps(payload))
    with contextlib.redirect_stdout(out):
        m.main(["--after-tool"])
finally:
    sys.stdin = old_stdin
hook = json.loads(out.getvalue())
check("opening hook JSON", hook["hookSpecificOutput"]["hookEventName"] == "PostToolUse")
import taguri_hook
old = sys.modules.get("_tomonari_koe")
sys.modules["_tomonari_koe"] = m
try:
    line = taguri_hook.deliver_line({"who": "Claude", "alias": "灯台", "day": "2026-11-01", "snippet": KEY})
    check("recall attaches protection marker", "いけび" in line)
finally:
    if old is None:
        sys.modules.pop("_tomonari_koe", None)
    else:
        sys.modules["_tomonari_koe"] = old

data = ROOT / ".local/evaluation-fixture"
(data / ".タイムマシン").mkdir(parents=True)
cues = data / "cues.json"
cues.write_text(json.dumps({"names": ["星灯", "青いノート"]}, ensure_ascii=False))


def baseline(days, watermark="2026-11-04T00:00:00Z", since="2026-11-03", until="2026-11-03"):
    tm = data / ".タイムマシン"
    for p in tm.iterdir():
        p.unlink()
    for day, rows in days.items():
        text = "\n\n".join(f"**{who}：**（{time}・{sid}）\n{body}" for who, time, sid, body in rows)
        (tm / (day + ".md.b64")).write_bytes(base64.b64encode(text.encode()))
    if watermark is not None:
        (tm / ".last").write_text(watermark)
    env = dict(os.environ, HIKARU_DATA_ROOT=str(data))
    result = subprocess.run([sys.executable, "-I", "-B", str(ROOT / "scripts/voice_baseline.py"),
                             "--since", since, "--until", until, "--cues", str(cues)],
                            env=env, capture_output=True, text=True)
    return result.stdout + result.stderr


text = baseline({"2026-11-02": [("ふ", "04:00:00", "abc", "嗯"), ("Claude", "04:30:00", "abc", "星灯一。")],
                 "2026-11-03": [("ふ", "05:00:00", "abc", "继续"), ("Claude", "05:30:00", "abc", "星灯二。")]})
check("baseline cross-boundary repeat", "| names | 1（1通） | 0 | 1（100.00）" in text)
text = baseline({"2026-11-03": [("ふ", "06:00:00", "abc", "嗯"), ("Claude", "06:01:00", "abc", "好。"), ("Claude", "06:02:00", "abc", "好。")]})
check("baseline short sentences", text.count("七日以内）：0") == 2)
text = baseline({"2026-11-03": [("ふ", "06:00:00", "abc", "嗯"), ("Claude", "06:01:00", "abc", "星灯和青いノート。")]})
check("baseline counts versus distinct replies", "| names | 2（1通）" in text and "| 全部 | 2（1通）" in text)
greetings = ["嗯，晚安，今天辛苦了。"] * 2 + ["你说说看，我还在听着呢。"] * 2
text = baseline({"2026-11-03": [("ふ", "06:00:00", "abc", "嗯")] + [("Claude", f"06:0{i+1}:00", "abc", s) for i, s in enumerate(greetings)]})
check("baseline greetings", text.count("七日以内）：0") == 2)
check("no watermark means unknown", "確かめられない" in baseline({}, None))
check("incomplete watermark means unknown", "確かめられない" in baseline({}, "2026-11-03T19:59:59Z"))
check("watermark at boundary confirms processed period", "確かめられない" not in baseline({}, "2026-11-03T20:00:00Z"))
check("complete empty period remains empty", "数える返事がない" in baseline({}))
text = baseline({"2027-01-02": [("ふ", "06:00:00", "abc", "嗯"), ("Claude", "06:01:00", "abc", "星灯。")]},
                "2027-01-03T00:00:00Z", "2027-01-02", "2027-01-02")
check("public evaluator is not pinned to 2026", "| names | 1（1通）" in text)

print(json.dumps({"author_cases": 17, "additional_checks": len(checks), "checks": checks,
                  "fixtures": "synthetic only", "network": "denied"}, ensure_ascii=False))
print("VOICE_CHECKS_PASSED")
