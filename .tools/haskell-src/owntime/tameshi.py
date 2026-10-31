#!/usr/bin/env python3
"""試し（tameshi）：自分の時間に書いた小さなプログラムを、閉じた箱の中で一度だけ走らせる（2026-10-28）。

    python3 tameshi.py ファイル [引数…]

走らせられるのは、いまの作業場（cwd）の中のファイルだけ。拡張子で決める：.py は Python 3.12、.hs は runghc。
箱（macOS の sandbox-exec。核を作るときと同じしくみ）の中では：

  * ネットにつながらない。
  * 書けるのは作業場の中だけ。
  * 読めるのは、作業場と、走らせるのに要る場所（システムの置き場所、Python と GHC の置き場所）だけ。
    それ以外（家の記録、会話の記録、令牌、inbox、共有の場所、外の円盤、一時置き場など）は読めない。
    （読むのを「全部だめ、要るところだけ許す」にしてある。2026-10-28、Codex の審査 38 で、だめを並べるやり方の漏れが見つかった）
  * 箱の外の程序に信号を送れない。
  * 60 秒たったら、走らせた程序と、それが起こした程序の組（進程組）をまとめて止める。
    ただし、自分で組を抜けた程序が残ることは、まだ防げていない（第一版の限り）。
  * 出力は、stdout と stderr を合わせて 16000 バイトまでしか受け取らない（それより先は読んで捨てる。溜めない）。

最後に一行、「試し：終わった（終了コード N、X 秒）」か「試し：時間切れ」を出す。足跡では、書いたことと、
走らせて動いたことを分けて書くので、この一行を見てから書く。
"""
from __future__ import annotations

import os
import signal
import subprocess
import sys
import threading
import time

HOME = os.path.expanduser("~")
PYTHON = os.path.join(HOME, ".pyenv", "versions", "3.12.12", "bin", "python3")
GHCUP = os.path.join(HOME, ".ghcup")
RUNGHC = os.path.join(GHCUP, "bin", "runghc")
TIMEOUT = 60
MAX_OUT = 16000  # バイト。stdout と stderr を合わせて

# 走らせるのに要る、読める場所（ホームの外）。ここにないところは読めない
SYSTEM_READABLE = [
    "/System", "/usr", "/bin", "/sbin", "/Library/Developer", "/Library/Apple",
    "/Applications/Xcode.app", "/private/etc", "/private/var/db", "/private/var/select", "/dev",
    "/opt/homebrew/opt", "/opt/homebrew/Cellar",
]


def quote(path: str) -> str:
    return '"' + path.replace("\\", "\\\\").replace('"', '\\"') + '"'


def policy(work: str) -> str:
    readable = [work, os.path.join(HOME, ".pyenv"), GHCUP, os.path.join(HOME, ".cabal"), os.path.join(HOME, ".ghc")] + SYSTEM_READABLE
    return (
        "(version 1)\n(allow default)\n"
        "(deny network*)\n"
        "(deny signal (target others))\n"
        "(deny file-write*)\n"
        f"(allow file-write* (subpath {quote(work)}) (literal \"/dev/null\") (literal \"/dev/tty\"))\n"
        "(deny file-read-data (subpath \"/\"))\n"
        "(allow file-read-data (literal \"/\") " + " ".join(f"(subpath {quote(p)})" for p in readable) + ")\n"
    )


class Budget:
    """二つの流れで分け合う、受け取ってよいバイト数。"""

    def __init__(self, limit: int) -> None:
        self.left = limit
        self.cut = False
        self.lock = threading.Lock()

    def take(self, chunk: bytes) -> bytes:
        with self.lock:
            if self.left <= 0:
                self.cut = True
                return b""
            keep = chunk[: self.left]
            self.left -= len(keep)
            if len(keep) < len(chunk):
                self.cut = True
            return keep


def drain(stream, sink: list, budget: Budget) -> None:
    while True:
        chunk = stream.read(4096)
        if not chunk:
            break
        kept = budget.take(chunk)
        if kept:
            sink.append(kept)


def kill_group(pgid: int) -> None:
    try:
        os.killpg(pgid, signal.SIGKILL)
    except ProcessLookupError:
        pass


def main() -> int:
    if len(sys.argv) < 2:
        print("使い方: tameshi.py ファイル [引数…]", file=sys.stderr)
        return 2
    work = os.path.realpath(os.getcwd())
    target = os.path.realpath(sys.argv[1])
    if not target.startswith(work + os.sep):
        print("試し：作業場の中のファイルだけ走らせられる", file=sys.stderr)
        return 2
    if not os.path.isfile(target):
        print(f"ValueError: expected a regular file: {target!r}", file=sys.stderr)
        return 2
    ext = os.path.splitext(target)[1].lower()
    if ext == ".py":
        cmd = [PYTHON, "-I", target]
    elif ext == ".hs":
        cmd = [RUNGHC, target]
    else:
        print("試し：走らせられるのは .py と .hs だけ", file=sys.stderr)
        return 2
    cmd += sys.argv[2:]
    tmp = os.path.join(work, ".tmp")
    os.makedirs(tmp, exist_ok=True)
    env = {
        "PATH": f"{os.path.join(GHCUP, 'bin')}:/usr/bin:/bin",
        "HOME": work,
        "TMPDIR": tmp + "/",
        "LANG": "en_US.UTF-8",
        "LC_ALL": "en_US.UTF-8",
    }
    start = time.time()
    # 新しい進程組で起こす。時間切れのときも、終わったあとも、組ごと止める（残った子を置いていかない）
    p = subprocess.Popen(["/usr/bin/sandbox-exec", "-p", policy(work), *cmd], cwd=work, env=env,
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE, start_new_session=True)
    pgid = p.pid
    budget = Budget(MAX_OUT)
    out, err = [], []
    readers = [threading.Thread(target=drain, args=(p.stdout, out, budget), daemon=True),
               threading.Thread(target=drain, args=(p.stderr, err, budget), daemon=True)]
    for t in readers:
        t.start()
    timed_out = False
    try:
        p.wait(timeout=TIMEOUT)
    except subprocess.TimeoutExpired:
        timed_out = True
    kill_group(pgid)
    p.wait()
    for t in readers:
        t.join(timeout=2)
    so = b"".join(out).decode("utf-8", "replace")
    se = b"".join(err).decode("utf-8", "replace")
    if so:
        print(so, end="" if so.endswith("\n") else "\n")
    if se:
        print("--- stderr ---")
        print(se, end="" if se.endswith("\n") else "\n")
    if budget.cut:
        print(f"（出力は {MAX_OUT} バイトで切った。その先は読んで捨てた）")
    if timed_out:
        print(f"試し：時間切れ（{TIMEOUT} 秒。進程組ごと止めた）")
        return 124
    print(f"試し：終わった（終了コード {p.returncode}、{time.time() - start:.1f} 秒）")
    return p.returncode


if __name__ == "__main__":
    sys.exit(main())
