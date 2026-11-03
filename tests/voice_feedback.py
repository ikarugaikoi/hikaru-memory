#!/usr/bin/env python3
"""Run Python-only voice checks in a relocated macOS sandbox, without real data."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

SOURCE = Path(__file__).resolve().parents[1]


def main():
    if sys.platform != "darwin":
        sys.exit("This isolation harness requires macOS sandbox-exec.")
    with tempfile.TemporaryDirectory(prefix="hikaru-voice-test-") as directory:
        work = Path(directory).resolve()
        root = work / "relocated installation"
        for name in (".tools", "scripts", "tests", ".local"):
            (root / name).mkdir(parents=True, exist_ok=True)
        files = [".tools/_hk.py", ".tools/_tool_data.py", ".tools/_tomonari_koe.py",
                 ".tools/taguri_hook.py", "scripts/hikaru", "scripts/voice_baseline.py",
                 "tests/voice_cases.py", "tests/voice_extra.py"]
        for name in files:
            shutil.copyfile(SOURCE / name, root / name)
        phrases = [["SYNTHETIC PRIVATE START", "SYNTHETIC PRIVATE END"]]
        privacy = json.dumps({"privacy_phrases": phrases}).encode()
        (root / ".local/privacy.json").write_bytes(privacy)
        # Python-only fixture fingerprint: no real settings or Haskell binary is used.
        (root / ".local/compiled-settings.sha256").write_text(hashlib.sha256(privacy).hexdigest())
        (root / "settings.local.json").write_text(json.dumps({"privacy_phrases": phrases}))
        quote = lambda p: json.dumps(str(p), ensure_ascii=False)
        policy = "(version 1)(allow default)(deny network*)"
        policy += f"(deny file-write* (require-not (subpath {quote(work)})))"
        policy += (f"(deny file-read* (require-all (subpath {quote(Path.home())}) "
                   f"(require-not (subpath {quote(work)})) "
                   f"(require-not (subpath {quote(Path(sys.base_prefix).resolve())}))))")
        (work / "isolation.sb").write_text(policy)
        env = {k: v for k, v in os.environ.items()
               if not k.startswith(("HIKARU_", "CLAUDE_CODE_", "MIZUKAGAMI_"))}
        env.update(PYTHONDONTWRITEBYTECODE="1", TMPDIR=str(work))
        result = subprocess.run(["/usr/bin/sandbox-exec", "-f", str(work / "isolation.sb"),
                                 sys.executable, "-I", "-B", str(root / "tests/voice_extra.py")],
                                cwd=root, env=env, capture_output=True, text=True)
        print(result.stdout, end="")
        if result.returncode:
            print(result.stderr, file=sys.stderr, end="")
            sys.exit(result.returncode)
        if "ISOLATED_CHILD_STARTED" not in result.stdout or "VOICE_CHECKS_PASSED" not in result.stdout:
            sys.exit("The isolated child did not complete its checks.")


if __name__ == "__main__":
    main()
