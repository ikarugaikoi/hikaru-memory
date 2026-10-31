#!/usr/bin/env python3
"""Optional restic backup of explicitly configured source directories."""
import json
import os
import subprocess
import sys
from pathlib import Path

import shutil
ROOT = Path(__file__).resolve().parent.parent
SETTINGS = ROOT / "settings.local.json"
config = json.loads(SETTINGS.read_text()).get("backup", {})
RESTIC = config.get("executable") or shutil.which("restic") or "restic"
REPO = Path(config.get("repository") or ROOT / ".local" / "unconfigured-backup")
PASSWORD_COMMAND = config.get("password_command", "")
SOURCES = config.get("sources", [])


def restic(*args):
    env = dict(os.environ, RESTIC_REPOSITORY=str(REPO), RESTIC_PASSWORD_COMMAND=PASSWORD_COMMAND)
    return subprocess.run([RESTIC, *args], env=env, capture_output=True, text=True)


def fail(what, p=None):
    last = (p.stderr.strip().splitlines() or [""])[-1] if p else ""
    sys.exit(f"RuntimeError: restic backup failed: {what}" + (f": {last}" if last else ""))


def human(n):
    for unit in ("B", "KB", "MB", "GB"):
        if n < 1024 or unit == "GB":
            return f"{n:.0f} {unit}" if unit == "B" else f"{n:.1f} {unit}"
        n /= 1024


def main():
    if not SOURCES or not config.get("repository") or not PASSWORD_COMMAND:
        fail("configure backup.repository, password_command and sources first")
    if not Path(RESTIC).exists():
        fail(f"executable not found: {RESTIC}")
    if not REPO.is_dir():
        fail(f"repository directory not found: {REPO}")
    for s in SOURCES:
        if not Path(s).is_dir():
            fail(f"source directory not found: {s}")
    if "--list" in sys.argv[1:]:
        p = restic("snapshots", "--compact")
        if p.returncode:
            fail(f"cannot open repository {REPO}; exit {p.returncode}", p)
        print(p.stdout.rstrip())
        return
    p = restic("backup", "--json", "--tag", "hikaru", "--exclude", ".DS_Store", "--exclude", "__pycache__",
               "--exclude", str(ROOT / ".build"), *SOURCES)   # 作り直せるビルドの中間物（1 GB 余り）は写さない
    summary = None
    for line in p.stdout.splitlines():
        try:
            d = json.loads(line)
        except ValueError:
            continue
        if d.get("message_type") == "summary":
            summary = d
    if p.returncode or not summary:
        fail(f"backup to {REPO} did not complete; exit {p.returncode}", p)
    c = restic("check")
    if c.returncode:
        fail(f"snapshot {summary['snapshot_id'][:8]} was saved, but repository check failed; exit {c.returncode}", c)
    print(f"琥珀：写し {summary['snapshot_id'][:8]}（{summary['total_files_processed']} ファイル。"
          f"新しい {summary['files_new']}、変わった {summary['files_changed']}、"
          f"倉に足したのは {human(summary['data_added_packed'])}）。倉の検査も済みました。")


if __name__ == "__main__":
    main()
