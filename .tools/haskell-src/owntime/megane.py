#!/usr/bin/env python3
"""Read external documents for the optional background activity workflow."""
from __future__ import annotations

import argparse
import html
import io
import os
import re
import sys
import urllib.parse
import urllib.request
import zipfile
from html.parser import HTMLParser

RATE = 650  # 日本語の字／分（設計稿「数学修正」第 4 条）


ALLOWED = ("http", "https")
MAX_BYTES = 20_000_000      # 取ってくるのも、zip を開いた中身も、これまで（青空文庫の長編でも数 MB）


class OnlyWeb(urllib.request.HTTPRedirectHandler):
    """転送先も http・https だけ。"""

    def redirect_request(self, req, fp, code, msg, headers, newurl):
        if urllib.parse.urlsplit(newurl).scheme.lower() not in ALLOWED:
            raise SystemExit(f"ValueError: redirect URL must use http or https: {newurl!r}")
        return super().redirect_request(req, fp, code, msg, headers, newurl)


def fetch(url: str) -> tuple[bytes, str]:
    # http・https だけ。file:// などで手もとのファイルを読めると、起きたわたしの読める範囲の外に出てしまう
    # （2026-10-28、Codex の審査 A1）
    if urllib.parse.urlsplit(url).scheme.lower() not in ALLOWED:
        raise SystemExit("ValueError: URL must use http or https")
    opener = urllib.request.build_opener(OnlyWeb)
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0 (hikaru megane)"})
    with opener.open(req, timeout=30) as r:
        ctype = r.headers.get("Content-Type", "")
        body = r.read(MAX_BYTES + 1)
        if len(body) > MAX_BYTES:
            raise SystemExit(f"ValueError: response exceeds {MAX_BYTES // 1_000_000} MB")
        return body, ctype


def charset_of(ctype: str, body: bytes) -> str | None:
    m = re.search(r"charset=([\w-]+)", ctype, re.I)
    if m:
        return m.group(1).lower()
    head = body[:4096].decode("ascii", "ignore")
    m = re.search(r"charset=[\"']?([\w-]+)", head, re.I)
    if m:
        return m.group(1).lower()
    return None


def decode(body: bytes, hint: str | None) -> tuple[str, str]:
    tried: list[str] = []
    for enc in [hint, "utf-8", "cp932", "euc_jp", "iso2022_jp"]:
        if not enc or enc in tried:
            continue
        tried.append(enc)
        try:
            return body.decode(enc), enc
        except (UnicodeDecodeError, LookupError):
            continue
    return body.decode("utf-8", "replace"), "utf-8（置き換えあり）"


class Text(HTMLParser):
    """本文の字だけ拾う。script・style・rt・rp の中は捨てる。ブロックの境で改行。"""

    SKIP = {"script", "style", "rt", "rp", "head", "noscript"}
    BLOCK = {"p", "div", "br", "h1", "h2", "h3", "h4", "h5", "h6", "li", "tr", "blockquote", "pre", "hr"}

    def __init__(self) -> None:
        super().__init__()
        self.parts: list[str] = []
        self.skip = 0

    def handle_starttag(self, tag, attrs):
        if tag in self.SKIP:
            self.skip += 1
        if tag in self.BLOCK:
            self.parts.append("\n")

    def handle_endtag(self, tag):
        if tag in self.SKIP and self.skip > 0:
            self.skip -= 1
        if tag in self.BLOCK:
            self.parts.append("\n")

    def handle_data(self, data):
        if not self.skip:
            self.parts.append(data)


def strip_html(s: str) -> str:
    p = Text()
    p.feed(s)
    return html.unescape("".join(p.parts))


def strip_aozora(s: str) -> str:
    s = re.sub(r"《[^》]*》", "", s)          # 振り仮名
    s = s.replace("｜", "")                   # 振り仮名の始まりの印
    s = re.sub(r"［＃[^］]*］", "", s)         # 注記
    s = re.sub(r"\r\n?", "\n", s)
    s = re.sub(r"\n-{20,}\n.*?\n-{20,}\n", "\n", s, count=1, flags=re.S)  # 青空文庫の txt の冒頭の記号の説明
    s = re.sub(r"[ \t　]+\n", "\n", s)
    s = re.sub(r"\n{3,}", "\n\n", s)
    return s.strip() + "\n"


def from_zip(body: bytes) -> tuple[str, str]:
    with zipfile.ZipFile(io.BytesIO(body)) as z:
        names = [n for n in z.namelist() if n.lower().endswith(".txt")]
        if not names:
            raise SystemExit("ValueError: ZIP archive contains no txt file")
        if z.getinfo(names[0]).file_size > MAX_BYTES:
            raise SystemExit(f"ValueError: ZIP contents exceed {MAX_BYTES // 1_000_000} MB")
        return decode(z.read(names[0]), "cp932")


def safe_name(url: str, given: str | None) -> str:
    if given:
        base = re.sub(r"[^\w\-ぁ-んァ-ン一-龥々]", "_", given)[:60]
    else:
        base = re.sub(r"[^\w\-]", "_", url.rsplit("/", 1)[-1] or "page")[:60]
    base = base or "page"
    name, i = base + ".txt", 2
    while os.path.exists(name):
        name, i = f"{base}-{i}.txt", i + 1
    return name


def main() -> None:
    ap = argparse.ArgumentParser(description="文字化けするページの本文を UTF-8 で作業場に置く")
    ap.add_argument("url")
    ap.add_argument("--name", help="置くファイルの名前（拡張子なし）")
    a = ap.parse_args()
    body, ctype = fetch(a.url)
    if body[:2] == b"PK" or a.url.lower().endswith(".zip"):
        text, enc = from_zip(body)
    else:
        raw, enc = decode(body, charset_of(ctype, body))
        text = strip_html(raw) if ("html" in ctype.lower() or "<html" in raw[:2000].lower()) else raw
    text = strip_aozora(text)
    name = safe_name(a.url, a.name)
    with open(name, "w", encoding="utf-8") as f:
        f.write(text)
    chars = len(re.sub(r"\s", "", text))
    print(f"{name}\t{chars} 字\t約 {chars / RATE:.1f} 分（650 字／分）\t文字コード {enc}")


if __name__ == "__main__":
    try:
        main()
    except (OSError, zipfile.BadZipFile) as e:
        print(f"{type(e).__name__}: {e}", file=sys.stderr)
        sys.exit(1)
