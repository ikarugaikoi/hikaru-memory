#!/usr/bin/env python3
"""General-language frequency cache used by associative-trigger filtering."""
import json
import subprocess
import sys
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import _hk  # noqa: E402
from _tool_data import index_dir, now_jst, read_frequency_cache

VERSION = "rarest-token-v1"
VENV = Path.home() / ".local" / "share" / "hikaru" / "wordfreq-venv" / "bin" / "python"
TABLE = index_dir(_hk.ROOT) / "yonotsune.json.b64"

MEASURE = r"""
import json, re, sys
from wordfreq import zipf_frequency, tokenize
KANA = re.compile(r'[぀-ヿ]'); HAN = re.compile(r'[一-鿿]'); LAT = re.compile(r'[A-Za-z]')
out = {}
for a in json.load(sys.stdin):
    langs = ['ja'] if KANA.search(a) else (['en'] if LAT.search(a) and not HAN.search(a) else ['zh', 'ja'])
    vals = []
    for lang in langs:
        toks = tokenize(a, lang) or [a]
        vals.append(min(zipf_frequency(t, lang) for t in toks))
    out[a] = max(vals)
print(json.dumps(out, ensure_ascii=False))
"""


def measure(aliases):
    """別名の列 → {別名: 一般の頻度}。VENV の Python で測る（-I：ほかの場所のモジュールを読まない）。"""
    r = subprocess.run([str(VENV), "-I", "-c", MEASURE], input=json.dumps(sorted(set(aliases)), ensure_ascii=False),
                       capture_output=True, text=True, timeout=600)
    if r.returncode != 0:
        raise RuntimeError(f"wordfreq in {VENV} exited with status {r.returncode}: {r.stderr.strip()[-300:]}")
    return json.loads(r.stdout)


def load():
    """置いてある表。{別名: 値}。なければ空（どの別名も「頻度が分からない」になる）。"""
    return read_frequency_cache(TABLE)


def main():
    import _koyori
    aliases = [a for v in _koyori.load_things().values() for a in v["aliases"]]
    values = measure(aliases)
    data = {"version": VERSION, "made_at": now_jst(), "aliases": values}
    with _hk.lock(TABLE):
        _hk.write_sealed(TABLE, json.dumps(data, ensure_ascii=False))
    print(f"一般の頻度：{len(values)} の別名を測った（{VERSION}）")


if __name__ == "__main__":
    main()
