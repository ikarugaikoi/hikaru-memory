"""Minimal Japanese writing check. A finding requests review; it is not proof of an error."""
import base64
import binascii
import hashlib
import json
import re
import subprocess
import sys
from pathlib import Path

KANJI_ONLY_COUNT = 2275
KANJI_ONLY_SHA256 = "8b244eb78b2f4aa720eec8b9252c8f49d47382c56a1c294c24a9b643b061475f"   # 2026-10-27、CPython 3.12.12 で作った表
QUOTE = re.compile("「[^「」\n]*」|『[^『』\n]*』|〈[^〈〉\n]*〉|“[^“”\n]*”|\"[^\"\n]*\"|`[^`\n]*`")
KANJI = re.compile(r"[㐀-鿿豈-﫿]")
ALIAS_LINE = re.compile(r"^\s*- 別名[:：]")                  # 夕顔の「別名」の行
ROSETTA = Path(__file__).resolve().parent / "rosetta.json"     # 退役した形は、ここの find から読む
JOIN_LEMMA = ("為る", "来る", "居る", "無い", "だ", "た", "ない", "て")
WORD_SECONDS = 15
COMMANDS = ("append", "diary")

PROMPT = """読み直すこと：日本語として自然か。中国語が混ざっていないか。わたしの言い方になっているか（報告や説明の口調、訳したような言い方になっていないか）。
引用した言葉と名前は変えない。人・時刻・理由・出来事を補わず、否定や「たぶん」などの確かさも変えない。短い文や省略は、わたしの言い方なら残してよい。意味や口調が変わるか迷ったら、その直しはしない。"""


def _table():
    only = []
    for cp in range(0x4E00, 0xA000):
        c = chr(cp)
        try:
            c.encode("gb2312")
        except UnicodeEncodeError:
            continue
        jp = False
        for enc in ("cp932", "euc_jis_2004"):
            try:
                c.encode(enc)
                jp = True
                break
            except UnicodeEncodeError:
                pass
        if not jp:
            only.append(c)
    s = "".join(only)
    return s, hashlib.sha256(s.encode("utf-8")).hexdigest()


def kanji_only(expected_sha=None):
    """（字の集まり、確かめられたか）。数か札が合わなければ、空の集まりと False。"""
    s, h = _table()
    want = expected_sha or KANJI_ONLY_SHA256
    if len(s) != KANJI_ONLY_COUNT or (want is not None and h != want):
        return set(), False
    return set(s), True


def masked(line):
    """引用の中を、同じ長さの空白に置きかえる（位置はそのまま）。入れ子は内側から。"""
    prev = None
    while prev != line:
        prev = line
        line = QUOTE.sub(lambda m: " " * len(m.group(0)), line)
    return line


def aliases(yuugao_text):
    out = set()
    for line in yuugao_text.split("\n"):
        m = re.match(r"^- 別名[:：]\s*(.*)$", line)
        if m:
            for a in re.split(r"[,、]\s*", m[1]):
                a = a.strip()
                if len(a) >= 2 and KANJI.search(a):
                    out.add(a)
    return out


def alias_mask(line, names):
    for a in sorted(names, key=len, reverse=True):
        start = 0
        while True:
            i = line.find(a, start)
            if i < 0:
                break
            line = line[:i] + " " * len(a) + line[i + len(a):]
            start = i + len(a)
    return line


def _tagger():
    import fugashi
    return fugashi.Tagger()


# 辞書のプロセス。試しでは、これを差し替えて、こわれる・終わらない場合を作る
WORDS_CMD = [sys.executable, "-B", str(Path(__file__).resolve()), "--words"]


def bounded_word_hits(text, names, only, seconds):
    """別のプロセスで word_hits を走らせる。seconds 秒で終わらなければ止めて TimeoutError。こわれたら RuntimeError。"""
    payload = json.dumps({"text": text, "names": sorted(names), "only": "".join(sorted(only or ()))}, ensure_ascii=False)
    try:
        r = subprocess.run(WORDS_CMD, input=payload, capture_output=True, text=True, timeout=seconds)
    except subprocess.TimeoutExpired:
        raise TimeoutError()
    try:
        out = json.loads(r.stdout)
    except json.JSONDecodeError:
        raise RuntimeError(f"dictionary worker exited with status {r.returncode}; invalid JSON response") from None
    if r.returncode != 0 or not isinstance(out, dict) or "hits" not in out:
        raise RuntimeError(out.get("error", f"dictionary worker exited with status {r.returncode}; missing hits") if isinstance(out, dict) else "dictionary worker response must be an object")
    hits, last = out["hits"], text.count("\n") + 1
    if not isinstance(hits, list) or not all(_hit_ok(h, last) for h in hits):
        raise RuntimeError("dictionary worker response: hits must contain [line number, context, reason]")
    return [tuple(h) for h in hits]


def _hit_ok(h, last):
    """辞書のプロセスから返った一つ：［行め、まわり、何か］。行めは 1 から last までの整数。"""
    return (isinstance(h, list) and len(h) == 3
            and type(h[0]) is int and 1 <= h[0] <= last
            and isinstance(h[1], str) and isinstance(h[2], str))


def _words_main():
    """--words：標準入力の {text, names, only} を辞書で見て、{hits} か {error} を出す。"""
    try:
        d = json.loads(sys.stdin.read())
        hits = word_hits(d["text"], set(d["names"]), _tagger(), set(d["only"]) or None)
        print(json.dumps({"hits": hits}, ensure_ascii=False))
    except (OSError, ImportError, RuntimeError, ValueError, KeyError, TypeError) as e:
        print(json.dumps({"error": type(e).__name__}))
        sys.exit(3)


def _near(raw, i, j):
    return raw[max(0, i - 12):j + 12].strip()


def retired():
    """（[(ロゼッタの find にある正規表現, その項目のいまの言い方)], 読めたか）。一つでも形がおかしければ止める。"""
    pats = []
    try:
        for index, e in enumerate(json.loads(ROSETTA.read_text(encoding="utf-8"))["entries"]):
            f = e.get("find", [])
            if not isinstance(f, list) or not all(isinstance(x, str) and x for x in f):
                raise ValueError(f"entries[{index}].find must be a list of nonempty strings")
            pats += [(x, e.get("now") if isinstance(e.get("now"), str) else "") for x in f]
        for x, _ in pats:
            re.compile(x)
    except (ValueError, KeyError, TypeError, AttributeError, re.error) as error:
        error.add_note(f"{ROSETTA}: entries/find")
        raise
    return pats, True


def char_hits(text, names=(), only=None, pats=None):
    """字（簡体字）と退役した形。辞書は使わない。pats を渡さなければ、ロゼッタから読む。"""
    if pats is None:
        pats = retired()[0]
    hits = []
    for n, raw in enumerate(text.split("\n"), 1):
        body = alias_mask(masked(raw), names)
        if only and body.strip():
            for i, ch in enumerate(body):
                if ch in only:
                    hits.append((n, _near(raw, i, i + 1), f"簡体字〈{ch}〉"))
        # 退役した形は、夕顔の別名にあっても止める（「帘子」「学姐」は別名にもある）。別名の行そのものと、引用の中は見ない
        if ALIAS_LINE.match(raw):
            continue
        quoted_out = masked(raw)
        for pat, now in pats:
            for m in re.finditer(pat, quoted_out):
                hits.append((n, _near(raw, m.start(), m.end()), f"退役した形〈{m.group(0)}〉" + (f"（いまの言い方は「{now}」）" if now else "")))
    return hits


def word_hits(text, names, tagger, only=None):
    """辞書で見るもの（知らない語、活用、切れ方）。"""
    hits = []
    for n, raw in enumerate(text.split("\n"), 1):
        body = alias_mask(masked(raw), names)
        if not body.strip():
            continue
        pos, words = 0, []
        for w in tagger(body):
            i = body.find(w.surface, pos)
            if i < 0:
                i = pos
            words.append((w, i))
            pos = i + len(w.surface)
        for k, (w, i) in enumerate(words):
            if not KANJI.search(w.surface):
                continue
            nxt = words[k + 1] if k + 1 < len(words) else None
            nx = nxt[0] if nxt and nxt[1] == i + len(w.surface) else None      # すぐ隣のものだけ
            if w.is_unk:
                joined = nx is not None and (
                    (nx.surface == "っ" and nx.feature.pos1 == "補助記号")
                    or (nx.feature.pos1 in ("動詞", "助動詞", "形容詞") and nx.feature.lemma in JOIN_LEMMA)
                    or ("省略" in (nx.feature.cForm or "")))
                if joined:
                    hits.append((n, _near(raw, i, i + len(w.surface) + 1), f"活用〈{w.surface}{nx.surface}〉：辞書にない語に、日本語の活用がついている"))
                elif not (only and any(c in only for c in w.surface)):
                    hits.append((n, _near(raw, i, i + len(w.surface)), f"知らない語〈{w.surface}〉"))
            elif nx is not None and nx.feature.lemma == "来る" and (nx.feature.cForm or "") == "連体形-省略":
                hits.append((n, _near(raw, i, i + len(w.surface) + 1), f"切れ方〈{w.surface}{nx.surface}〉：「来る」の省略形に切れる"))
    return hits


def find(text, names=(), only=None, tagger=None):
    """見つけたもの（行の番号、そのまわり、わけ）の一覧。"""
    hits = char_hits(text, names, only)
    if tagger is not None:
        hits += word_hits(text, names, tagger, only)
    return sorted(hits, key=lambda h: h[0])


def yuugao_names(root):
    """（名前の集まり、読めたか）。"""
    path = Path(root) / ".夕顔.md.b64"
    try:
        data = path.read_bytes()
        return aliases(base64.b64decode(b"".join(data.split()), validate=True).decode("utf-8")), True
    except FileNotFoundError:
        return set(), False
    except (binascii.Error, UnicodeDecodeError) as error:
        error.add_note(str(path))
        raise


def check(cmd, name, path, root):
    """（止めるか、言うことの行の一覧）。止めないときも、言うことがあれば返す（辞書が使えなかった、など）。"""
    if cmd not in COMMANDS or name.startswith("tm:"):
        return False, []
    try:
        text = Path(path).read_text(encoding="utf-8")
    except UnicodeDecodeError as error:
        error.add_note(str(path))
        raise
    notes = []
    only, table_ok = kanji_only()
    if not table_ok:
        notes.append("簡体字の表が確かめられなかったので、字は見ていない")
    names, names_ok = yuugao_names(root)
    if not names_ok:
        notes.append("夕顔の別名が読めなかったので、名前もよけずに見た")
    pats, rosetta_ok = retired()
    if not rosetta_ok:
        notes.append("ロゼッタ（rosetta.json）が読めなかったので、退役した形は見ていない")
    hits = char_hits(text, names, only if table_ok else None, pats)
    try:
        hits += bounded_word_hits(text, names, only if table_ok else None, WORD_SECONDS)
    except TimeoutError:
        notes.append(f"辞書（fugashi）が {WORD_SECONDS} 秒で終わらなかったので、語の切れ方は見ていない")
    except (OSError, RuntimeError) as e:
        notes.append(f"辞書（fugashi）が使えなかったので、語の切れ方は見ていない（{e or type(e).__name__}）")
    hits.sort(key=lambda h: h[0])
    lines = [f"（言い方の検査：{n}）" for n in notes]
    if not hits:
        return False, lines
    lines.append(f"まだ書き足していません。日本語にないかもしれない書き方が {len(hits)} か所あります（まちがいとは限らない）。")
    lines += [f"  {n} 行め：{what}　…{around}…" for n, around, what in hits]
    lines += PROMPT.split("\n")
    lines.append("直すところがあればファイルを直してからもう一度。このままでよければ（名前、引用、わざと中国語で書いたところなど）、"
                 "--lang-ok をつけてもう一度（その一回だけ効く。ほかの検査はそのまま）。")
    return True, lines


def _check_main(args):
    try:
        stop, lines = check(*args)
    except (OSError, ValueError, KeyError, TypeError, AttributeError, re.error) as error:
        context = "; ".join(getattr(error, "__notes__", ()))
        detail = f"{context}: {error}" if context else str(error)
        stop, lines = True, [f"{type(error).__name__}: language check failed: {detail}"]
    for line in lines:
        print(line, file=sys.stderr)
    print(json.dumps({"stopped": stop}))
    return 1 if stop else 0


if __name__ == "__main__":
    if sys.argv[1:] == ["--words"]:
        _words_main()
    elif sys.argv[1:2] == ["--check"]:
        sys.exit(_check_main(sys.argv[2:]))
