#!/usr/bin/env python3
"""Prepare, review and apply bounded wording repairs to memory records."""
import argparse
import datetime as dt
import difflib
import fcntl
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import unicodedata
import uuid
from pathlib import Path

HIKARU = Path(os.environ.get("MIZUKAGAMI_HIKARU", str(Path(__file__).resolve().parent.parent)))
sys.path.insert(0, str(HIKARU / ".tools"))
from _tool_data import unseal_protected as unseal
STATE = Path(os.environ.get("MIZUKAGAMI_STATE", str(HIKARU / ".local" / "repairs")))
LINE_FILE = Path(os.environ.get("MIZUKAGAMI_LINE_FILE", str(HIKARU / ".local" / "config" / "repair-line.json")))
THIRD = Path(os.environ.get("MIZUKAGAMI_THIRD", str(HIKARU / ".tools/直し/日本語点検_全部_第三版.md.b64")))
WINDOW = 30
RUN_NOTE = 50

FIXED = {"yuugao": ".夕顔.md.b64", "strobe": ".ストロボメモリー.md.b64",
         "shiori": ".しおり.md.b64", "biblia": ".ビブリア.md.b64"}
REF_NAME = {"yuugao": "夕顔.md", "strobe": "ストロボメモリー.md", "shiori": "しおり.md", "biblia": "ビブリア.md"}
STEM = {"yuugao": "yuugao", "strobe": "strobe", "shiori": "shiori", "biblia": "biblia"}

# 核の場所・記録の場所を、親から受け継いだ環境で変えさせない（核は自分の場所から家の根を決める）。
# 作業フォルダ（HIKARU_WORK）は、書き込みごとに、こちらで決めて渡す
SEAL_ENV_DROP = ("HIKARU_ROOT", "HIKARU_DATA_ROOT", "HIKARU_CODE_ROOT", "HIKARU_CORE", "HIKARU_RUNTIME_ROOT",
                 "HIKARU_LOCK_ROOT", "HIKARU_BACKUP_ROOT", "HIKARU_TM_ROOT", "HIKARU_LOG_ROOT",
                 "HIKARU_PROJECT_IDENTITY", "HIKARU_PROJECT_SLUG", "HIKARU_PICTURE_ROOT", "HIKARU_WORK",
                 "HIKARU_SCRATCH_ROOT")

PEOPLE_FIXED = ["ユーザー", "わたし", "クロード"]
# 家の名前、使わない言葉、薄めない印は、ひかるのロゼッタ（.tools/rosetta.json）から読む（2026-10-28 から。ここには書き写さない）
ROSETTA = HIKARU / ".tools/rosetta.json"


def _rosetta():
    """(家の名前：count_fixed, 使わない言葉の正規表現：find, ストロボメモリーの薄めない印：keep_mark)。
    読めない、形がおかしい、どれかが空、のときは止める（守りが抜けたまま直しを書かないように）。"""
    try:
        house, retired, keep = [], [], []
        for e in json.loads(ROSETTA.read_text(encoding="utf-8"))["entries"]:
            c, f = e.get("count_fixed"), e.get("find", [])
            if c is True:
                house.append(e["now"])
            elif c:
                house.append(c)
            if not isinstance(f, list):
                raise ValueError(f"entry {e.get('id')!r}: find must be a list")
            retired += f
            if e.get("keep_mark"):
                keep.append(e["now"])
        if not all(isinstance(w, str) and w for w in house + retired + keep):
            raise ValueError("count_fixed/find/keep_mark must contain nonempty strings")
        for r in retired:
            re.compile(r)
        if not (house and retired and keep):
            raise ValueError("count_fixed, find and keep_mark must each contain at least one entry")
        return house, retired, tuple(keep)
    except (OSError, ValueError, KeyError, TypeError, AttributeError, re.error) as e:
        sys.exit(f"{type(e).__name__}: {ROSETTA}: {e}")


HOUSE, RETIRED, KEEP_MARKS = _rosetta()
KEEP_NAMES = tuple(m.strip("（）()") for m in KEEP_MARKS)


def keep_mark(line):
    """行の終わりにある、薄めない印の名前（なければ None）。核の Memory/Strobe.hs の markOf と同じ見分け方：
    行末の空白を除いて、最後が「）」か「)」、その前が印の名前、その前が「（」か「(」。行の途中にあるものは、印にしない（2026-10-29 から。前は行のどこにあっても、全角だけを見ていた）。"""
    s = line.rstrip()
    if not s or s[-1] not in ")）":
        return None
    body = s[:-1]
    for name in KEEP_NAMES:
        if body.endswith(name) and body[:-len(name)][-1:] in ("(", "（"):
            return name
    return None
# 清单の案に混ざっていた、点検の作業のメモの形（これで全部とは言えない。見つかった形だけ）
MEMO = re.compile(r"（[^）]*(行\s*\d|案のまま|元のまま|同じ：|同じ段落)")

OPEN = {"「": "」", "『": "』", "（": "）"}
CLOSE = {v: k for k, v in OPEN.items()}
STOPS = "。！？"


class Refuse(Exception):
    """動かない（入力が合わない）。何も書かない。"""


class Halt(Exception):
    """書き込みの途中で、ほかから書き換えられた、片づけが残った、などが分かった。どの記録への書き込みも、そこで止める。"""


# ---------- 札と封 ----------

def sha(data):
    return hashlib.sha256(data).hexdigest()


def plain_sha(text):
    return sha(text.encode("utf-8"))


def digest_of(obj):
    return sha(json.dumps(obj, ensure_ascii=False, sort_keys=True, separators=(",", ":")).encode("utf-8"))


def now_iso():
    return dt.datetime.now().isoformat(timespec="seconds")


def nfc(s):
    return unicodedata.normalize("NFC", s)


# ---------- 記録の名前 ----------

def week_start(day):
    return day - dt.timedelta(days=day.weekday())


def norm_name(name):
    """核の normName と同じ：ichifure:日 を、その週の月曜日の名前に。"""
    name = name.strip()
    m = re.match(r"^ichifure:.*?(\d{4}-\d{2}-\d{2})", name)
    if not m:
        return name
    try:
        return f"ichifure:{week_start(dt.date.fromisoformat(m[1])).isoformat()}"
    except ValueError:
        return name


def record_path(name):
    """決まった六つの名前だけ。ichifure は週の月曜日の名前で呼ぶ。"""
    if name in FIXED:
        return HIKARU / FIXED[name]
    m = re.fullmatch(r"ariake:(\d{4}-\d{2}-\d{2})", name)
    if m:
        dt.date.fromisoformat(m[1])
        return HIKARU / ".ありあけ" / f"{m[1]}.read.md.b64"
    m = re.fullmatch(r"ichifure:(\d{4}-\d{2}-\d{2})", name)
    if m:
        day = dt.date.fromisoformat(m[1])
        if week_start(day) != day:
            raise Refuse(f"日記は週の月曜日の名前で呼ぶ: {name}")
        for p in sorted((HIKARU / ".一週間フレンズ").glob(f"{m[1]}〜*.md.b64")):
            return p
        raise Refuse(f"その週の日記がない: {name}")
    raise Refuse(f"受けつけない記録の名前: {name}")


def kind_of(name):
    return name.split(":", 1)[0]


def stem_of(name):
    if name in STEM:
        return STEM[name]
    k, d = name.split(":", 1)
    return {"ichifure": f"diaryweek-{d}", "ariake": f"ariake-{d}"}[k]


def ref_path(refs, version, name):
    if not re.fullmatch(r"[a-z0-9]+", str(version)):
        raise Refuse(f"版の名前がおかしい: {version}")
    base = Path(refs) / version
    if name in REF_NAME:
        p = base / REF_NAME[name]
    elif name.startswith("ariake:"):
        p = base / f"{name[7:]}.read.md"
    else:
        hits = sorted(base.glob(f"日記_{name[9:]}〜*.md"))
        p = hits[0] if len(hits) == 1 else base / "（ない）"
    if not p.is_file():
        raise Refuse(f"版がない: {version} {name}")
    return p


def order_key(name):
    rank = {"yuugao": 0, "strobe": 1, "shiori": 2, "ichifure": 3, "ariake": 4, "biblia": 5}[kind_of(name)]
    return (rank, name)


# ---------- 行と文 ----------

def lines_with_offsets(text):
    out, pos = [], 0
    for line in text.split("\n"):
        out.append((pos, line))
        pos += len(line) + 1
    return out


def heading_at(text, pos):
    best = ""
    for start, line in lines_with_offsets(text):
        if start > pos:
            break
        if line.startswith("#"):
            best = line
    return best


def find_all(text, needle):
    out, i = [], text.find(needle)
    while needle and i >= 0:
        out.append(i)
        i = text.find(needle, i + 1)
    return out


def quotes(text):
    """「」『』の中身を、開いた順に。"""
    stack, out = [], []
    for i, ch in enumerate(text):
        if ch in "「『":
            stack.append((ch, i))
        elif ch in "」』" and stack and stack[-1][0] == CLOSE[ch]:
            _, s = stack.pop()
            out.append((s, text[s + 1:i]))
    return [q for _, q in sorted(out)]


def misekechi_blocks(text):
    """みせけちの欄（始めの行、終わりの行）。読み方は correction_index（核と同じ手順）。"""
    return correction_index(text)[0]


def protected(text, kind):
    """守る範囲（始め、終わり）の一覧。"""
    spans = []
    lines = lines_with_offsets(text)

    def whole(i):
        s, line = lines[i]
        spans.append((s, s + len(line)))

    first_section = next((i for i, (_, l) in enumerate(lines) if l.startswith("## ")), len(lines))
    if kind == "yuugao":
        first_section = next((i for i, (_, l) in enumerate(lines) if l.startswith("## 人")), first_section)
    if kind in ("yuugao", "shiori", "strobe", "ichifure", "ariake"):
        for i in range(first_section):
            whole(i)
    if kind == "yuugao":
        for b, e in misekechi_blocks(text):
            for i in range(b, e):
                whole(i)
    block = False
    for i, (s, line) in enumerate(lines):
        if line.startswith("#"):
            whole(i); block = False; continue
        if block:
            if line.startswith(" ") or line.startswith("\t"):
                whole(i); continue
            block = False
        if kind == "yuugao":
            if re.match(r"^- (別名|つながり|日付)[:：]", line):
                whole(i)
            else:
                m = re.match(r"^- (説明|ユーザーの言葉|わたしの見立て)[:：] ?", line)
                if m:
                    spans.append((s, s + m.end()))
        elif kind == "shiori":
            m = re.match(r"^- \d{1,2}/\d{1,2} ", line)
            if m:
                spans.append((s, s + m.end()))
            m = re.search(r" ?→ (済|入れた|丘の上|入れない)（", line)
            if m:
                spans.append((s + m.start(), s + len(line)))
        elif kind == "strobe":
            if keep_mark(line):
                whole(i)                         # 行の終わりに（ねゆき）か（いけび）のある行は、行ごと守る（計画のとおり。いけびは 2026-10-28 から）
        elif kind == "ichifure":
            if re.match(r"^- (問いたいこと|候補|答えが出たこと)", line):
                whole(i); block = True
            else:
                m = re.match(r"^- (一行で|話したこと|わたしが思ったこと)：", line)
                if m:
                    spans.append((s, s + m.end()))
        elif kind == "ariake":
            # 道具（Fuutou/Record.hs の rereadBlocks・rereadReport）が判と再読の扱いに使うのは、札と並び、
            # 判が「まだ分からない」で始まるかどうか。抽き選びと、読み直しが済んだかどうかは、ほかの言い回しに寄らない
            m = re.match(r"^[ \t\-・*　]*(判|再読)[：:][ 　]*(まだ分からない)?", line)
            if m:
                spans.append((s, s + m.end()))
            elif re.match(r"^[ \t\-・*　]*(しるし|二択)[：:]", line):
                whole(i)
    return sorted(spans)


def protected_by_line(text, kind):
    """行の番号ごとに、守る部分の文字列（突き合わせに使う）。"""
    lines = lines_with_offsets(text)
    out = {}
    for s, e in protected(text, kind):
        for i, (ls, line) in enumerate(lines):
            le = ls + len(line)
            if e < ls or s > le:
                continue
            a, b = max(s, ls), min(e, le)
            out.setdefault(i, []).append(text[a:b])
    return out


def overlaps(a, b, spans):
    return any(s < b and a < e for s, e in spans)


def sentences(text, spans):
    """控えの全文の上で、文の範囲を先に決める。かっこの外の「。！？」と行の終わりで切る。
    行の頭の字下げと箇条書きの印、行の頭や終わりの守る部分（欄の名前など）は、文の端から外す。"""
    out = []
    for ls, line in lines_with_offsets(text):
        depth, start = 0, ls
        for j, ch in enumerate(line):
            i = ls + j
            if ch in OPEN:
                depth += 1
            elif ch in CLOSE and depth:
                depth -= 1
            elif ch in STOPS and depth == 0:
                out.append((start, i + 1)); start = i + 1
        if start < ls + len(line):
            out.append((start, ls + len(line)))
    trimmed = []
    for s, e in out:
        lead = re.match(r"[ \t　]*(?:- |・|\* )?", text[s:e])
        s += lead.end() if lead else 0
        changed = True
        while changed and s < e:
            changed = False
            for ps, pe in spans:
                if ps <= s < pe:
                    s, changed = min(pe, e), True
                if ps < e <= pe:
                    e, changed = max(ps, s), True
        if s < e:
            trimmed.append((s, e))
    return trimmed


def unit_span(sents, s, e):
    """[s, e) にかかる文を全部合わせた範囲（元が文の外の印から始まっていれば、そこまで広げる）。"""
    hit = [(a, b) for a, b in sents if a < e and s < b]
    if not hit:
        return None
    return (min([a for a, _ in hit] + [s]), max([b for _, b in hit] + [e]))


def change_ratio(before, after):
    """同じでないところごとに、前と後の長いほうを足し、前と後の長いほうの文字数で割る。"""
    m = difflib.SequenceMatcher(None, before, after, autojunk=False)
    changed = sum(max(i2 - i1, j2 - j1) for tag, i1, i2, j1, j2 in m.get_opcodes() if tag != "equal")
    return changed / max(len(before), len(after), 1)


# ---------- 引用（みせけちの「残っているところ」） ----------

def parse_trace(line):
    """核の trace と同じ：名前｜位置｜だれ「手がかり」。区切りは｜でも|でもよく、位置はなくてもよい。"""
    body = line.strip()
    if body.startswith("- "):
        body = body[2:]
    if "「" not in body:
        return None
    prefix, rest = body.split("「", 1)
    if not rest.rstrip().endswith("」"):
        return None
    clue = rest.rstrip()[:-1]
    parts = [p.strip() for p in re.split(r"[｜|]", prefix)]
    return (parts[0] if parts else ""), clue


KINDS = ("読み違い", "変わった", "食い違い", "場合による")
FIELDS = ("前の直し", "前", "いま", "いつから", "一方", "もう一方", "ふだん", "このとき", "なぜ", "よりどころ", "範囲",
          "残っているところ")


def _entry_head(line):
    """核の entryHeading：「### 」のあとに一字でもあれば、項目の見出し（空白だけでも）。"""
    return line.startswith("### ") and len(line) > 4


def _block_head(line):
    """核の blockHeading：「- みせけち」のすぐあとに : か ：。そのあとの文字（なければ空）。"""
    if line.startswith("- みせけち") and line[6:7] in (":", "："):
        return line[7:].strip()
    return None


def _after_spaces(line):
    rest = line.lstrip(" ")
    return rest if len(rest) < len(line) else None


def _rev_head(line):
    """核の revHeading：半角の空白で始まり、「- 〔直 」、番号、「〕」、種類。"""
    rest = _after_spaces(line)
    if rest is None or not rest.startswith("- 〔直 ") or "〕" not in rest[5:]:
        return None
    rid, kind = rest[5:].split("〕", 1)
    return rid.strip(), kind.strip()


def _field_head(line):
    """核の fieldHeading：半角の空白で始まり、「- 」、名前（:：「｜| の手前まで、空でない）、: か ：、値。"""
    rest = _after_spaces(line)
    if rest is None or not rest.startswith("- "):
        return None
    m = re.match(r"([^:：「｜|]*)([:：「｜|])", rest[2:])
    if not m or not m[1] or m[2] not in ":：":
        return None
    return m[1].strip(), rest[2 + m.end():].strip()


def _valid_id(rid):
    """核の validId：2026-10-04a の形。"""
    return (len(rid) == 11 and rid[:4].isdecimal() and rid[4] == "-" and rid[5:7].isdecimal() and rid[7] == "-"
            and rid[8:10].isdecimal() and "a" <= rid[10] <= "z")


def correction_index(text):
    """（みせけちの欄の一覧、引用の一覧（名前、手がかり）、形の問題の一覧（行、わけ））。
    核の Memory/Correction/Parse.hs の parse と parseBlock を、同じ順で写したもの。そこで核が「止める」問題も「止めない」問題も、
    ここでは同じに数える（使う側は、一つでもあれば動かない）。Correction.Check の中身の検査は、写していない。
    番号の数字は Python の isdecimal で見る（核は ICU。食い違えば、ここが断る側に寄る）。"""
    ls = text.split("\n")
    blocks, found, problems, ids = [], [], [], set()

    def prob(i, why):
        problems.append((i, why))

    def stray(a, b):
        for i in range(a, b):
            if ls[i].lstrip().startswith("- 〔直 "):
                prob(i, "直しが「- みせけち:」の下にない")

    for start in [i for i, l in enumerate(ls) if _entry_head(l)]:
        end = next((i for i in range(start + 1, len(ls)) if ls[i].startswith(("# ", "## ", "### "))), len(ls))
        heads = [i for i in range(start + 1, end) if _block_head(ls[i]) is not None]
        if not heads:
            stray(start + 1, end)
            continue
        b = heads[0]
        e = next((i for i in range(b + 1, end) if ls[i].strip() and not ls[i].startswith((" ", "\t"))), end)
        while e > b + 1 and not ls[e - 1].strip():
            e -= 1
        if len(heads) > 1:
            prob(heads[1], "「- みせけち:」が二つある")
        if _block_head(ls[b]):
            prob(b, "「- みせけち:」の行に、何かが続いている")
        _correction_block(ls, b, e, found, prob, ids)
        stray(start + 1, b)
        stray(e, end)
        blocks.append((b, e))
    return blocks, found, problems


def _correction_block(ls, begin, end, found, prob, ids):
    cur, fld, fld_ind = None, None, 0
    for i in range(begin + 1, end):
        line = ls[i]
        if not line.strip():
            continue
        lead = line[:len(line) - len(line.lstrip())]
        ind = len(lead)
        if "\t" in lead:
            prob(i, "字下げにタブがある"); continue
        rh = _rev_head(line)
        if rh:
            rid, kind = rh
            if not _valid_id(rid):
                prob(i, f"直しの番号の形が違う（{rid}）")
            elif rid in ids:
                prob(i, f"直しの番号が重なっている（{rid}）")
            else:
                ids.add(rid)
            if kind not in KINDS:
                prob(i, f"直しの種類が違う（{kind}）")
            cur, fld, fld_ind = {"ind": ind, "keys": []}, None, 0
            continue
        if cur is None:
            prob(i, "直しの頭より前に行がある"); continue
        if fld == "残っているところ" and ind > fld_ind:
            tr = parse_trace(line)
            if tr is None:
                prob(i, "残っているところの一件が読めない")
            else:
                found.append([norm_name(tr[0]), tr[1]])
            continue
        fh = _field_head(line)
        if fh and ind > cur["ind"]:
            key, val = fh
            if key in cur["keys"]:
                prob(i, f"「{key}」が二つある")
            else:
                cur["keys"].append(key)
                if key in ("tm", "ariake", "ichifure"):
                    prob(i, "引用が、「残っているところ」より一段深くない")
                elif key not in FIELDS:
                    prob(i, f"知らない欄（{key}）")
                if key == "前の直し" and not re.search(r"〔直 [^〕]*〕", val):
                    prob(i, "「前の直し」に番号がない")
                if key == "残っているところ" and val:
                    prob(i, "残っているところが、一行に並んでいる")
            fld, fld_ind = key, ind
            continue
        if fld is not None and fld != "残っているところ" and ind > fld_ind and not line.lstrip().startswith("- "):
            continue                                  # 欄の続きの行
        prob(i, "読めない")


def traces(yuugao_text):
    """（名前、手がかり）。核と同じ手順で読む（correction_index）。核が問題として数えるものが一つでもあれば、動かない
    （少なく守るより、断るほうをとる）。名前は核の normName でそろえる。"""
    _, found, problems = correction_index(yuugao_text)
    if problems:
        i, why = problems[0]
        more = f"。ほかに {len(problems) - 1} か所" if len(problems) > 1 else ""
        raise Refuse(f"夕顔のみせけちに、読めない行がある（{i + 1} 行め：{why}{more}）")
    return found


def clues_for(record, all_traces):
    return [c for name, c in all_traces if name == record and c]


def clue_lines(text, clue):
    """核の walk と hits と同じく、記録も手がかりも NFC にそろえ、一行ずつ探す。（行の番号、その行で当たった数）。"""
    c = nfc(clue)
    return [(i, nfc(line).count(c)) for i, line in enumerate(text.split("\n")) if c and c in nfc(line)]


def clue_spans_of(base, all_traces):
    """手がかりが当たる行は、行ごと守る（NFC の上の位置を、元の文字の位置に戻さずに、広めに守る）。"""
    lines = lines_with_offsets(base.text)
    out = []
    for c in clues_for(base.name, all_traces):
        for i, _ in clue_lines(base.text, c):
            ls, line = lines[i]
            out.append((ls, ls + len(line)))
    return out


# ---------- 人の名前と、清单の第三版 ----------

def people(yuugao_text):
    names, inside = set(PEOPLE_FIXED), False
    for line in yuugao_text.split("\n"):
        if line.startswith("## "):
            inside = line.startswith("## 人")
            continue
        if not inside:
            continue
        if line.startswith("### "):
            names.add(line[4:].strip())
        m = re.match(r"^- 別名[:：] ?(.*)$", line)
        if m:
            for a in re.split(r", |、", m[1]):
                if a.strip():
                    names.add(a.strip())
    return sorted(names)


def verified_items():
    """封をした清单の第三版から、「原文で確かめた：案どおり」の項目を、見出しの行の番号（L123）ごとに（元、案）。"""
    try:
        data = THIRD.read_bytes()
    except FileNotFoundError:
        return {}
    text = unseal(data)
    out, cur, head = {}, {}, None

    def flush():
        if head and cur.get("原文で確かめた", "").startswith("案どおり") and "元" in cur and "**案**" in cur:
            out[f"L{head}"] = (cur["元"], cur["**案**"])

    for n, line in enumerate(text.split("\n"), 1):
        if line.startswith("#"):
            flush(); cur = {}
            head = n if re.match(r"^### \d+\.", line) else None
            continue
        m = re.match(r"^- (元|\*\*案\*\*|原文で確かめた)：(.*)$", line)
        if m and m[1] not in cur:
            cur[m[1]] = m[2]
    flush()
    return out


def policy_of(yuugao_text):
    """守りのよりどころ（夕顔の引用と人の名前、清单の第三版）の札。束に結び、書く直前にもう一度確かめる。"""
    try:
        third = sha(THIRD.read_bytes())
    except FileNotFoundError:
        third = ""
    return digest_of({"traces": traces(yuugao_text), "people": people(yuugao_text), "third": third})


# ---------- 束 ----------

class Base:
    """一つの記録の、控えの全文と、それから計算したもの。"""
    def __init__(self, name, text, sealed_sha256):
        self.name, self.text, self.kind = name, text, kind_of(name)
        self.sealed_sha256, self.plain_sha256 = sealed_sha256, plain_sha(text)
        self.spans = protected(text, self.kind)
        self.sents = sentences(text, self.spans)


def load_current(name):
    data = record_path(name).read_bytes()
    return Base(name, unseal(data), sha(data))


def base_from_refs(refs, version, name, rec):
    """apply は、記録の今ではなく、prepare のときの控え（版のフォルダ）を土台にする。"""
    text = ref_path(refs, version, name).read_text(encoding="utf-8")
    if plain_sha(text) != rec["plain_sha256"]:
        raise Refuse(f"控えの版の中身の札が、束と合わない: {name}")
    return Base(name, text, rec["sealed_sha256"])


def item_refusal(it, base, refs, clue_spans):
    """この項目を入れないわけ（入れてよければ None）と、元の位置。"""
    moto, an = it.get("moto", ""), it.get("an", "")
    if not moto or not an:
        return "元か案がない", None
    if an.startswith("元のまま") or an.startswith("（元のまま"):
        return "元のまま", None
    if "原文で確かめる" in an:
        return "原文で確かめる（まだ確かめていない）", None
    if an == moto:
        return "元と案が同じ", None
    hits = find_all(base.text, moto)
    if len(hits) != 1:
        return f"控えの記録の中で、元がちょうど一回ではない（{'ない' if not hits else '二回以上'}）", None
    try:
        ref = ref_path(refs, it["source"], it["record"]).read_text(encoding="utf-8")
    except Refuse as e:
        return str(e), None
    if "source_sha256" in it and plain_sha(ref) != it["source_sha256"]:
        return "出どころの版の札が合わない", None
    rhits = find_all(ref, moto)
    if len(rhits) != 1:
        return f"出どころの版の中で、元がちょうど一回ではない（{'ない' if not rhits else '二回以上'}）", None
    s = hits[0]
    if heading_at(base.text, s) != heading_at(ref, rhits[0]):
        return "控えと出どころの版で、元の見出しが違う", None
    if overlaps(s, s + len(moto), base.spans):
        return "守る行にかかる", None
    span = unit_span(base.sents, s, s + len(moto))
    if span is None:
        return "文の中にない", None
    if overlaps(span[0], span[1], clue_spans):
        return "みせけちに引かれている文", None
    return None, s


def item_record(it, refs):
    p = ref_path(refs, it["source"], it["record"])
    return {"id": it["id"], "source": it["source"], "source_sha256": plain_sha(p.read_text(encoding="utf-8")),
            "moto": it["moto"], "an": it["an"]}


def group_units(base, placed):
    """（元の位置、項目）の並びから、文ごとのまとまり。範囲が重なる項目のある文は、まとめて外す。"""
    groups = []
    for s, it in sorted(placed, key=lambda x: x[0]):
        a, b = unit_span(base.sents, s, s + len(it["moto"]))
        if groups and a < groups[-1]["end"]:
            g = groups[-1]
            g["start"], g["end"] = min(g["start"], a), max(g["end"], b)
            g["items"].append((s, it))
        else:
            groups.append({"start": a, "end": b, "items": [(s, it)]})
    good, bad = [], []
    for g in groups:
        its = g["items"]
        if any(its[k][0] < its[k - 1][0] + len(its[k - 1][1]["moto"]) for k in range(1, len(its))):
            bad.append(g)
        else:
            good.append(g)
    return good, bad


def compose_unit(base, g):
    before = base.text[g["start"]:g["end"]]
    after = before
    for s, it in sorted(g["items"], key=lambda x: x[0], reverse=True):
        after = after[:s - g["start"]] + it["an"] + after[s + len(it["moto"]) - g["start"]:]
    return before, after


def unit_digest(unit, rec):
    return digest_of({
        "record": unit["record"], "sealed_sha256": rec["sealed_sha256"], "plain_sha256": rec["plain_sha256"],
        "start": unit["start"], "end": unit["end"], "before": unit["before"], "after": unit["after"],
        "items": unit["items"], "round": unit["round"], "prev": unit["prev"],
    })


def batch_id(batch):
    d = {"base": batch.get("base"), "sources": batch.get("sources"), "policy": batch.get("policy"),
         "units": sorted(u["digest"] for u in batch["units"])}
    if batch.get("one_off"):
        d["one_off"] = True                     # 一回きりの束は、同じ直しでも、ふだんの束とは別の番号になる
    return digest_of(d)


def build_units(cands, refs, base_version, sources, extra_skipped=()):
    """項目から、文ごとの直しを作る。入れないものは skipped に、わけをつけて。"""
    skipped = [dict(s, why=f"取り出し：{s.get('why', '')}") for s in extra_skipped]
    by_record, seen = {}, set()
    for it in cands:
        if it.get("id") in seen:
            raise Refuse(f"項目の番号が重なっている: {it.get('id')}")
        seen.add(it.get("id"))
        if it.get("source") not in sources:
            skipped.append({"id": it.get("id"), "record": it.get("record"), "why": f"出どころの版が決めたものでない: {it.get('source')}"})
            continue
        try:
            record_path(it["record"])
        except (Refuse, ValueError, KeyError) as e:
            skipped.append({"id": it.get("id"), "record": it.get("record"), "why": f"記録の名前: {e}"}); continue
        by_record.setdefault(it["record"], []).append(it)
    yuugao = load_current("yuugao")
    if plain_sha(ref_path(refs, base_version, "yuugao").read_text(encoding="utf-8")) != yuugao.plain_sha256:
        raise Refuse(f"控えの版（{base_version}）の夕顔が、夕顔の今と違う")
    all_traces = traces(yuugao.text)
    units, records = [], {}
    for name in sorted(by_record, key=order_key):
        base = load_current(name)
        if plain_sha(ref_path(refs, base_version, name).read_text(encoding="utf-8")) != base.plain_sha256:
            raise Refuse(f"控えの版（{base_version}）が、記録の今と違う: {name}")
        records[name] = {"sealed_sha256": base.sealed_sha256, "plain_sha256": base.plain_sha256}
        cspans = clue_spans_of(base, all_traces)
        placed = []
        for it in by_record[name]:
            why, s = item_refusal(it, base, refs, cspans)
            if why:
                skipped.append({"id": it["id"], "record": name, "why": why}); continue
            same = next((x for x in placed if x[0] == s and x[1]["moto"] == it["moto"] and x[1]["an"] == it["an"]), None)
            if same:
                skipped.append({"id": it["id"], "record": name, "why": f"同じ直しが、ほかの項目にもある（{same[1]['id']} に入れた）"})
                continue
            placed.append((s, it))
        good, bad = group_units(base, placed)
        for g in bad:
            for _, it in g["items"]:
                skipped.append({"id": it["id"], "record": name, "why": "同じ文の中で、直しの範囲が重なっている"})
        for g in good:
            before, after = compose_unit(base, g)
            unit = {"record": name, "start": g["start"], "end": g["end"], "before": before, "after": after,
                    "items": [item_record(it, refs) for _, it in g["items"]], "round": 1, "prev": ""}
            unit["digest"] = unit_digest(unit, records[name])
            units.append(unit)
    for n, u in enumerate(units, 1):
        u["unit"] = f"U{n:04d}"
    return {"round": 1, "base": base_version, "sources": sorted(sources), "policy": policy_of(yuugao.text),
            "records": records, "units": units, "skipped": skipped}


def write_packets(batch, outdir, bases):
    outdir = Path(outdir)
    outdir.mkdir(parents=True, exist_ok=True)
    groups = {}
    for u in batch["units"]:
        text = bases[u["record"]].text
        ls = lines_with_offsets(text)
        first, last = text.count("\n", 0, u["start"]), text.count("\n", 0, u["end"])
        ctx = "\n".join(l for _, l in ls[max(0, first - 2):last + 3])
        groups.setdefault(u["record"], []).append({
            "unit": u["unit"], "digest": u["digest"], "round": u["round"],
            "kind": kind_of(u["record"]), "heading": heading_at(text, u["start"]),
            "context": ctx, "before": u["before"], "after": u["after"],
        })
    for name, units in groups.items():
        (outdir / f"{name.replace(':', '_')}.json").write_text(json.dumps(units, ensure_ascii=False, indent=1), encoding="utf-8")


# ---------- 判断と束の確かめ ----------

def load_verdicts(path, units):
    """判断のファイルを全部読み、直し一つに判断一つが、摘要まで合っているか確かめる。"""
    want = {u["unit"]: u["digest"] for u in units}
    got = {}
    files = sorted(Path(path).glob("*.json")) if Path(path).is_dir() else [Path(path)]
    for f in files:
        for v in json.loads(f.read_text(encoding="utf-8")):
            uid = v.get("unit")
            if uid not in want:
                raise Refuse(f"判断に、束にない直しがある: {uid}（{f.name}）")
            if uid in got:
                raise Refuse(f"一つの直しに、判断が二つある: {uid}")
            if v.get("digest") != want[uid]:
                raise Refuse(f"摘要が合わない: {uid}")
            if v.get("verdict") not in ("いい", "よくない"):
                raise Refuse(f"判断が「いい」か「よくない」ではない: {uid}")
            got[uid] = v["verdict"]
    missing = [u for u in want if u not in got]
    if missing:
        raise Refuse(f"判断がない直しがある: {', '.join(missing[:5])}{' ほか' if len(missing) > 5 else ''}")
    return got


def check_batch(batch, refs, bases, all_traces):
    """一度目の束の中身を、控えと版から計算し直して確かめる（作った側を信じない）。"""
    ids = [u["unit"] for u in batch["units"]]
    if len(ids) != len(set(ids)):
        raise Refuse("束の中で、直しの番号が重なっている")
    item_ids = [it["id"] for u in batch["units"] for it in u["items"]]
    if len(item_ids) != len(set(item_ids)):
        raise Refuse("束の中で、項目の番号が重なっている")
    sources = set(batch.get("sources") or [])
    if batch.get("base") not in sources:
        raise Refuse("束の控えの版が、出どころの版のひとつでない")
    by_record = {}
    for u in batch["units"]:
        name = u["record"]
        record_path(name)
        rec = batch["records"].get(name)
        if not rec:
            raise Refuse(f"束に記録の札がない: {name}")
        if u["round"] != 1 or u["prev"] != "":
            raise Refuse(f"一度目の束に、一度目でない直しがある: {u['unit']}")
        if u["digest"] != unit_digest(u, rec):
            raise Refuse(f"摘要を計算し直すと合わない: {u['unit']}")
        if bases[name].text[u["start"]:u["end"]] != u["before"]:
            raise Refuse(f"前の文が、控えのその位置の文と違う: {u['unit']}")
        for it in u["items"]:
            if it.get("source") not in sources:
                raise Refuse(f"出どころの版が決めたものでない: {it.get('id')}")
            if not re.fullmatch(r"[0-9a-f]{64}", str(it.get("source_sha256", ""))):
                raise Refuse(f"出どころの版の札がない: {it.get('id')}")
        by_record.setdefault(name, []).append(u)
    for name, units in by_record.items():
        base = bases[name]
        cspans = clue_spans_of(base, all_traces)
        placed = []
        for u in units:
            for it in u["items"]:
                why, s = item_refusal(dict(it, record=name), base, refs, cspans)
                if why:
                    raise Refuse(f"項目を確かめ直すと、入れられない: {it['id']}（{why}）")
                placed.append((s, it))
        # その記録の項目を全部まとめて、文ごとに組み直し、束の直しと一つずつ合うか確かめる
        good, bad = group_units(base, placed)
        if bad:
            raise Refuse(f"同じ文の中で、直しの範囲が重なっている: {name}")
        want = {(g["start"], g["end"]): (sorted(it["id"] for _, it in g["items"]), compose_unit(base, g)) for g in good}
        got = {(u["start"], u["end"]): (sorted(it["id"] for it in u["items"]), (u["before"], u["after"])) for u in units}
        if len(got) != len(units) or want != got:
            raise Refuse(f"文ごとのまとまりを組み直すと、束の直しと合わない: {name}")


SAME_IN_BOTH = ("record", "start", "end", "before", "items")


def check_round2(b1, b2, v1):
    """二度目は、後の文と、何度目か・前の摘要・摘要だけが変わってよい。ほかは一度目とまったく同じ。"""
    by1 = {u["unit"]: u for u in b1["units"]}
    if any(b2.get(k) != b1.get(k) for k in ("base", "sources", "policy")):
        raise Refuse("二度目の束の、控えの版か出どころの版か守りのよりどころが、一度目と違う")
    ids = [u["unit"] for u in b2["units"]]
    if len(ids) != len(set(ids)):
        raise Refuse("二度目の束の中で、直しの番号が重なっている")
    out = {}
    for u2 in b2["units"]:
        u1 = by1.get(u2["unit"])
        if not u1:
            raise Refuse(f"二度目の直しが、一度目の束にない: {u2['unit']}")
        if any(u2.get(k) != u1.get(k) for k in SAME_IN_BOTH):
            raise Refuse(f"二度目の直しが、一度目と違う記録か範囲を指している: {u2['unit']}")
        if b2["records"].get(u2["record"]) != b1["records"].get(u1["record"]):
            raise Refuse(f"二度目の束の札が、一度目と違う: {u2['unit']}")
        if u2["round"] != 2 or u2["prev"] != u1["digest"] or v1[u1["unit"]] != "よくない":
            raise Refuse(f"二度目の直しが、一度目とつながらない: {u2['unit']}")
        if u2["digest"] != unit_digest(u2, b1["records"][u1["record"]]):
            raise Refuse(f"二度目の摘要を計算し直すと合わない: {u2['unit']}")
        out[u2["unit"]] = u2
    return out


# ---------- 関所 ----------

def guard(unit, after, base, names, verified, clue_spans):
    """当たった関所の名前（なければ None）。"""
    before = unit["before"]
    if not before.strip() or not after.strip():
        return "空の文"
    if base.text[unit["start"]:unit["end"]] != before:
        return "前の文が、控えのその位置の文と違う"
    if before.count("\n") != after.count("\n"):
        return "行の数が変わる"
    if quotes(before) != quotes(after):
        return "「」『』の中が変わる"
    for it in unit["items"]:
        s = base.text.find(it["moto"])
        if overlaps(s, s + len(it["moto"]), base.spans):
            return "守る行にかかる"
    for s, e in base.spans:
        if s < unit["end"] and unit["start"] < e and base.text[max(s, unit["start"]):min(e, unit["end"])] not in after:
            return "守る行にかかる"
    if change_ratio(before, after) > 0.5:
        return "半分より多くが書き換わる"
    # 人を足す例外：清单の第三版で、同じ見出しの番号の項目が「原文で確かめた：案どおり」で、元と案が同じものだけ。
    # 認めるのは、その項目が足した分だけ（同じ文のほかの項目が足した分は認めない）
    allowed = {}
    if unit["round"] == 1 and after == unit["after"]:
        for it in unit["items"]:
            if verified.get(it["id"]) == (it["moto"], it["an"]):
                for n in names:
                    allowed[n] = allowed.get(n, 0) + max(0, it["an"].count(n) - it["moto"].count(n))
    for n in names:
        if after.count(n) - before.count(n) > allowed.get(n, 0):
            return "人の名前や呼び名が増える"
    for h in HOUSE:
        if after.count(h) != before.count(h):
            return "家の名前の数が変わる"
    for r in RETIRED:
        if len(re.findall(r, after)) > len(re.findall(r, before)):
            return "使わない言葉が増える"
    if overlaps(unit["start"], unit["end"], clue_spans):
        return "みせけちに引かれている文"
    if len(MEMO.findall(after)) > len(MEMO.findall(before)):
        return "後の文に、点検の作業のメモが混ざっている"
    return None


# ---------- 線と記録 ----------

def read_line():
    try:
        v = float(json.loads(LINE_FILE.read_text(encoding="utf-8"))["line"])
    except (OSError, ValueError, KeyError, TypeError) as error:
        raise Refuse(f"{type(error).__name__}: {LINE_FILE}: line: {error}") from None
    if not 0 < v <= 1:
        raise Refuse(f"ValueError: {LINE_FILE}: line must be greater than 0 and at most 1")
    return v


def windows_pass(prior, mine, line):
    """prior：それまでの通った/通らなかった。mine：この記録の分（順に）。30 ごとと最後で、いちばん新しい 30 を見る。"""
    if not mine:
        return True
    seq = prior + mine
    for k in range(1, len(mine) + 1):
        if k % WINDOW == 0 or k == len(mine):
            w = seq[:len(prior) + k][-WINDOW:]
            if sum(w) / len(w) < line:
                return False
    return True


SHAPES = {
    "書き込み.jsonl": {"tx": str, "phase": str, "record": str, "workdir": str, "stem": str,
                    "plain_sha256_before": str, "plain_sha256_expected": str},
    "判断.jsonl": {"digest": str, "pass": bool},
    "門.jsonl": {"record": str, "digests": str, "passed": bool},
    "知らせ.jsonl": {"digest": str, "date": str},
}


def read_jsonl(path):
    """一行に一つの記録。字が UTF-8 として読めない、最後の行に改行がない（書きかけで切れた）、
    一行が JSON として読めない、一行の形が違う（決まった欄がない、型が違う）、のどれでも止める（直さずに、そのまま残す）。"""
    try:
        data = path.read_bytes()
    except FileNotFoundError:
        return []
    if not data:
        return []
    try:
        text = data.decode("utf-8")
    except UnicodeDecodeError as e:
        raise Halt(f"UnicodeDecodeError: {path}: {e}") from None
    if not text.endswith("\n"):
        raise Halt(f"{path.name} の最後の行に改行がない（書きかけで切れている？）。直さずに、確かめてから")
    rows = []
    for n, l in enumerate(text.split("\n")[:-1], 1):
        if not l.strip():
            continue
        try:
            row = json.loads(l)
        except json.JSONDecodeError as error:
            raise Halt(f"JSONDecodeError: {path}:{n}: {error}") from None
        shape = SHAPES.get(path.name, {})
        if not isinstance(row, dict) or any(not isinstance(row.get(k), t) for k, t in shape.items()):
            raise Halt(f"{path.name} の {n} 行めの形が違う（決まった欄がないか、型が違う）。直さずに、確かめてから")
        rows.append(row)
    return rows


def append_jsonl(path, rows):
    """書き足して、ディスクまで届ける。書けなければ止める（書きかけの行が残っていれば、次に読むところで止まる）。"""
    if not rows:
        return
    try:
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open("a", encoding="utf-8") as f:
            for r in rows:
                f.write(json.dumps(r, ensure_ascii=False) + "\n")
            f.flush()
            os.fsync(f.fileno())
    except OSError as e:
        raise Halt(f"{type(e).__name__}: cannot append to {path}: {e}") from None


def record_gate(history, mine_units, firsts, line):
    """この記録の窓。前に判断したことのある記録は、そのときの並び（それより前の判断）と、そのときの一度目の判断を使う。"""
    digests = [u["digest"] for u in mine_units]
    dset = set(digests)
    pos = [i for i, h in enumerate(history) if h["digest"] in dset]
    if pos:
        prior = [h["pass"] for h in history[:min(pos)]]
        known = {h["digest"]: h["pass"] for h in history if h["digest"] in dset}
        firsts = [known.get(d, f) for d, f in zip(digests, firsts)]
    else:
        prior = [h["pass"] for h in history]
    return windows_pass(prior, firsts, line), firsts


def note_text(ev):
    return f"{ev['date']} 二度目の目が、続けて全部「いい」だった（{ev['digest'][:12]} まで）。甘くなっていないか、見直してほしい。"


def run_milestones(history):
    """判断の記録の全体から、続けて「通った」がちょうど 50 になったところ（その摘要）。いつ数えても同じになる。"""
    run, out = 0, []
    for h in history:
        run = run + 1 if h["pass"] else 0
        if run == RUN_NOTE:
            out.append(h["digest"])
    return out


# ---------- 書き込み ----------

def seal(*args, work=None):
    env = {k: v for k, v in os.environ.items() if k not in SEAL_ENV_DROP}
    if work is not None:
        env["HIKARU_WORK"] = str(work)
    # 子（.tools/bin/fuutou、つまり核）にも .lock の鍵を渡す。apply が途中で殺されても、鍵を持つ最後のものが閉じるか終わるまで
    # 鍵は外れないので、次の apply も ack も、子が作業フォルダに書き終えるまで始まらない（片づけたあとに、子が平文を作り直さない）
    fds = (Lock.fd,) if Lock.fd is not None else ()
    r = subprocess.run([str(HIKARU / ".tools/bin/fuutou"), *args], capture_output=True, text=True, env=env,
                       pass_fds=fds)
    return r.returncode, (r.stdout + r.stderr).strip()


KNOWN = (".md", ".md.tmp", ".base.md", ".sha1", ".mine.md")


def _clear(top, stem, mine):
    """書き込みの作業フォルダ（作業/書き込みの番号/）を片づける。消すのは hikaru/ の中の、決まった名前のふつうのファイルで、
    mine が自分のものと認めたものだけ。そのほかのもの（hikaru/ の外に置かれたものも）は消さずに、残りに数える。
    全部消せたときだけ、空になったフォルダを一つずつ消す（中身ごと消すことはしない）。
    （残ったものの名前、そのうち自分のものと分からなかったもの）。"""
    top = Path(top)
    wd = top / "hikaru"
    left, strange = [], []

    def keep(name):
        left.append(name); strange.append(name)

    if top.is_dir():
        for f in sorted(top.iterdir()):
            if f.name != "hikaru" or f.is_symlink() or not f.is_dir():
                keep(f.name)
    if wd.is_dir() and not wd.is_symlink():
        for f in sorted(wd.iterdir()):
            suf = next((x for x in KNOWN if f.name == stem + x), None)
            if suf is None or f.is_symlink() or not f.is_file() or not mine(suf, f):
                keep("hikaru/" + f.name); continue
            try:
                f.unlink()
            except OSError:
                left.append("hikaru/" + f.name)
    if not left:
        for d in (wd, top):
            try:
                if d.is_dir() and not d.is_symlink():
                    d.rmdir()
            except OSError:
                pass
            if d.exists() or d.is_symlink():
                left.append(d.name + "/")
                break
    return left, strange


def owned_leftovers(row, after_ack=False):
    """その書き込みの作業フォルダ（記録に残した場所）を片づける。自分が置いたものか（中身で）確かめてから消す。
    人が確かめた（ack）あとは、決まった名前のものは中身を問わず消す。決まった名前でないものは、消さずに残りに数える。"""
    before, expected = row["plain_sha256_before"], row["plain_sha256_expected"]

    def mine(suf, f):
        if after_ack or suf == ".md.tmp":
            return True
        try:
            if suf == ".md":
                return plain_sha(f.read_text(encoding="utf-8")) in (before, expected)
            if suf == ".base.md":
                return plain_sha(f.read_text(encoding="utf-8")) == before
            if suf == ".sha1":
                got = f.read_text(encoding="utf-8").strip()
                return got == row["sha1"] if row.get("sha1") else bool(re.fullmatch(r"[0-9a-f]{40}", got))
        except (OSError, UnicodeDecodeError):
            return False
        return False
    return _clear(Path(row["workdir"]).parent, row["stem"], mine)


def tidy_private(work, stem):
    """書き込みの最中に、この書き込みだけの作業フォルダを片づける（さっき自分で作ったフォルダなので、決まった名前のものは
    自分のもの）。消せなかったもの、決まった名前でないものを返す。"""
    return _clear(work, stem, lambda suf, f: True)[0]


def policy_now():
    """今の夕顔と清单の第三版から作った、守りのよりどころの札。読めなければ None。"""
    try:
        return policy_of(load_current("yuugao").text)
    except (Refuse, OSError, ValueError):
        return None


def same_policy(kept):
    """残しておいた札と、今の札が同じか。どちらかが分からない（ない、読めない、文字でない）ときは、同じとしない。"""
    now = policy_now()
    return isinstance(kept, str) and isinstance(now, str) and kept == now


def current_plain(name):
    return plain_sha(unseal(record_path(name).read_bytes()))


def current_both(name):
    data = record_path(name).read_bytes()
    return sha(data), plain_sha(unseal(data))


PENDING = ("用意", "開いた", "始め", "片づけ残り", "確かめた")


def observed(name):
    """記録の今の札。解けなければ、封の札だけ（中身の札は「解けない」）。"""
    data = record_path(name).read_bytes()
    try:
        return sha(data), plain_sha(unseal(data)), None
    except ValueError as e:
        return sha(data), None, str(e)


def recover(write_path):
    """前の本番が途中で止まっていたら、書き込みの記録から、どこまで済んだかを確かめてから始める。
    記録にない作業フォルダが残っていても、止める。"""
    last, began, was_committed, was_unwritten = {}, set(), set(), set()
    for r in read_jsonl(write_path):
        last[r["tx"]] = r
        if r["phase"] == "始め":
            began.add(r["tx"])               # close を呼ぶと残した。あとで「片づけ残り」になっても、これは消えない
        if r.get("committed") is True:
            was_committed.add(r["tx"])       # 一度「書けた」と残した
        if r.get("committed") is False:
            was_unwritten.add(r["tx"])       # 一度「書かれていない」と見た（close のあとか、close を呼ぶ前に）
    open_dirs = {str(Path(r["workdir"]).parent) for r in last.values() if r["phase"] in PENDING and r.get("workdir")}
    work_root = STATE / "作業"
    if work_root.is_dir():
        stray = [d.name for d in work_root.iterdir() if str(d) not in open_dirs]
        if stray:
            raise Halt(f"書き込みの記録にない作業フォルダが残っている: {', '.join(stray)}（中を確かめてから）")
    out = []
    for tx, r in last.items():
        if r["phase"] == "どちらでもない":
            raise Halt(f"前の書き込み（{r['record']}、{tx}）で、記録が前とも当てたものとも違うか、解けないと分かっている。"
                       f"何があったかを確かめてから `mizukagami.py ack --tx {tx} --note …` で印をつける")
        if r["phase"] not in PENDING:
            continue
        if not r.get("workdir"):
            raise Halt(f"前の書き込み（{r['record']}、{tx}）の作業フォルダが分からない")
        acked = r["phase"] == "確かめた" or r.get("acked")
        if acked:
            left, _ = owned_leftovers(r, after_ack=True)
            if left:
                append_jsonl(write_path, [dict(r, phase="片づけ残り", acked=True, left=left, at=now_iso())])
                raise Halt(f"印をつけた書き込み（{r['record']}）の作業フォルダに、片づけられないものがある: {', '.join(left)}")
            row = dict(r, phase="片づけた", acked=True, left=[], at=now_iso())
            append_jsonl(write_path, [row]); out.append({"tx": tx, "record": r["record"], "phase": row["phase"]})
            continue
        sealed_now, now, err = observed(r["record"])
        seen = {"sealed_sha256_after": sealed_now, "plain_sha256_after": now if now else "（解けない）"}
        closed = tx in began                 # 「始め」がなければ、close はまだ呼んでいない（片づけ残りでも同じ）
        if now is None:
            append_jsonl(write_path, [dict(r, phase="どちらでもない", why=f"解けない: {err}", **seen, at=now_iso())])
            raise Halt(f"前の書き込み（{r['record']}）のあと、記録が解けない。確かめてから ack で印をつける")
        if closed and now == r["plain_sha256_expected"] and tx not in was_unwritten:
            committed = True
        elif now == r["plain_sha256_before"] and tx not in was_committed:
            committed = False
        else:
            why = ("前に書けたと残したのに、今は前のまま" if now == r["plain_sha256_before"] else
                   "close を呼ぶ前に止まったのに、当てたものになっている" if now == r["plain_sha256_expected"] and not closed else
                   "書かれていないと残したのに、当てたものになっている" if now == r["plain_sha256_expected"] else
                   "前とも当てたものとも違う")
            append_jsonl(write_path, [dict(r, phase="どちらでもない", why=why, **seen, at=now_iso())])
            raise Halt(f"前の書き込み（{r['record']}）のあと、{why}。確かめてから ack で印をつける")
        if committed and not same_policy(r.get("policy")):
            why = ("書いたときと今とで、守りのよりどころ（夕顔の引用か人の名前、清单の第三版）が同じだと確かめられない"
                   "（違う、今のが読めない、書いたときのが残っていない。書いているあいだに変わったかは、分からない）")
            append_jsonl(write_path, [dict(r, phase="どちらでもない", committed=True, why=why, **seen, at=now_iso())])
            raise Halt(f"前の書き込み（{r['record']}）は書けていたが、{why}。引用が守れたか確かめてから ack で印をつける")
        left, strange = owned_leftovers(r)
        if left:
            append_jsonl(write_path, [dict(r, phase="片づけ残り", committed=committed, left=left, **seen, at=now_iso())])
            raise Halt(f"前の書き込み（{r['record']}）の作業フォルダに、片づけられないもの"
                       f"{'（自分のものと分からないもの）' if strange else ''}がある: {', '.join(left)}")
        row = dict(r, phase="済" if committed else "書かれていない", committed=committed, recovered=True, left=[],
                   **seen, at=now_iso())
        append_jsonl(write_path, [row])
        out.append({"tx": tx, "record": r["record"], "phase": row["phase"]})
    return out


def write_record(base, expected, adopted, not_applied, write_path, bid, policy, one_off=False):
    """一つの記録に書き込む。書けたら True、書かなかったら（わけ）。止めるべきときは Halt。
    簿付けの順：用意（作業フォルダを作る前）→ 開いた（.sha1 の中身）→ 始め（close の前）→ 済・書かれていない・片づけ残り・どちらでもない。"""
    name = base.name
    path = record_path(name)
    if sha(path.read_bytes()) != base.sealed_sha256:
        return "控えのあとに書き換えられていた"
    tx = uuid.uuid4().hex
    work = STATE / "作業" / tx                 # この書き込みだけの作業フォルダ（ほかの会話と同じ場所を使わない）
    wd, stem = work / "hikaru", stem_of(name)
    row = {"tx": tx, "batch": bid, "record": name, "workdir": str(wd), "stem": stem,
           "sealed_sha256_before": base.sealed_sha256, "plain_sha256_before": base.plain_sha256,
           "plain_sha256_expected": plain_sha(expected), "adopted": adopted,
           "applied_units": len(adopted), "applied_items": sum(a["items"] for a in adopted),
           "not_applied_units": not_applied, "policy": policy}
    if one_off:
        row["one_off"] = True
    append_jsonl(write_path, [dict(row, phase="用意", at=now_iso())])   # 平文ができる前に、場所を残す
    work.mkdir(parents=True, mode=0o700)

    def stop_unwritten(why):
        left = tidy_private(work, stem)
        if left:
            append_jsonl(write_path, [dict(row, phase="片づけ残り", committed=False, left=left, at=now_iso())])
            raise Halt(f"{name} は書いていないが、作業フォルダの片づけが残った: {', '.join(left)}")
        append_jsonl(write_path, [dict(row, phase="書かれていない", committed=False, left=[], why=why, at=now_iso())])
        return why

    rc, out = seal("open", name, work=work)
    opened = wd / f"{stem}.md"
    if (wd / f"{stem}.sha1").is_file():
        row["sha1"] = (wd / f"{stem}.sha1").read_text(encoding="utf-8").strip()
    append_jsonl(write_path, [dict(row, phase="開いた", open_rc=rc, at=now_iso())])
    if rc != 0 or not opened.is_file():
        return stop_unwritten(f"open できなかった: {out.splitlines()[0] if out else rc}")
    if plain_sha(opened.read_text(encoding="utf-8")) != base.plain_sha256:
        return stop_unwritten("開いた中身が控えと違った")
    tmp = wd / f"{stem}.md.tmp"
    tmp.write_text(expected, encoding="utf-8")
    os.replace(tmp, opened)                    # .md は、開いたときの中身か、あるべき全文の、どちらか
    append_jsonl(write_path, [dict(row, phase="始め", at=now_iso())])     # close の前に、何をするかを残す
    rc, out = seal(*(["close", name] + (["--force"] if name == "strobe" else [])), work=work)
    sealed_now, now, err = observed(name)
    seen = {"sealed_sha256_after": sealed_now, "plain_sha256_after": now if now else "（解けない）"}
    if now is None:
        append_jsonl(write_path, [dict(row, phase="どちらでもない", close_rc=rc, why=f"解けない: {err}", **seen, at=now_iso())])
        raise Halt(f"{name} が解けない: {err}")
    if now not in (row["plain_sha256_expected"], base.plain_sha256):
        append_jsonl(write_path, [dict(row, phase="どちらでもない", close_rc=rc, why="前とも当てたものとも違う", **seen, at=now_iso())])
        raise Halt(f"{name} が、前とも、当てたものとも違う。ほかから書き換えられた")
    committed = now == row["plain_sha256_expected"]
    left = tidy_private(work, stem)
    if left:
        append_jsonl(write_path, [dict(row, phase="片づけ残り", committed=committed, close_rc=rc, left=left, **seen, at=now_iso())])
        raise Halt(f"{name} は{'書けた' if committed else '書けなかった'}が、作業フォルダの片づけが残った: {', '.join(left)}")
    if committed and not same_policy(policy):
        # 書いているあいだに、守りのよりどころが変わった（ほかの会話や仕事が、夕顔か清单を書き換えた）
        append_jsonl(write_path, [dict(row, phase="どちらでもない", committed=True, close_rc=rc, left=[],
                                       why="書いているあいだに、守りのよりどころ（夕顔か清单の第三版）が変わった", **seen, at=now_iso())])
        raise Halt(f"{name} を書いているあいだに、夕顔か清单の第三版が変わった。引用が守れたか、確かめてから ack で印をつける")
    append_jsonl(write_path, [dict(row, phase="済" if committed else "書かれていない", committed=committed, close_rc=rc,
                                   left=[], **seen, at=now_iso())])
    if committed:
        return True
    return f"close で書かれなかった: {out.splitlines()[0] if out else rc}"


def compose(text, chosen):
    """控えの全文に、通った直しだけを当てた「あるべき全文」。"""
    out, last = text, None
    for u, after in sorted(chosen, key=lambda x: x[0]["start"], reverse=True):
        if last is not None and u["end"] > last:
            raise Refuse(f"直しの範囲が重なっている: {u['unit']}")
        if out[u["start"]:u["end"]] != u["before"]:
            raise Refuse(f"前の文が控えと合わない: {u['unit']}")
        out = out[:u["start"]] + after + out[u["end"]:]
        last = u["start"]
    return out


def record_checks(base, expected, clues):
    """close の前の確かめ。通らなければ、わけ。"""
    text, kind = base.text, base.kind
    if text.count("\n") != expected.count("\n"):
        return "行の数が変わった"
    if quotes(text) != quotes(expected):
        return "「」『』の並びが変わった"
    if protected_by_line(text, kind) != protected_by_line(expected, kind):
        return "守る行が変わった"
    for c in clues:
        if clue_lines(text, c) != clue_lines(expected, c):
            return "みせけちの手がかりの位置が変わった"
    if kind == "ariake":
        mada = lambda t: [(i, bool(re.match(r"^[ \t\-・*　]*判[：:][ 　]*まだ分からない", l)))
                          for i, l in enumerate(t.split("\n")) if re.match(r"^[ \t\-・*　]*(判|再読)[：:]", l)]
        if mada(text) != mada(expected):
            return "判と再読の並びか、「まだ分からない」が変わった"
    if kind == "strobe":
        hl = lambda t: [l for l in t.split("\n") if l.startswith("#")]
        nl = lambda t: [(i, l) for i, l in enumerate(t.split("\n")) if keep_mark(l)]
        if hl(text) != hl(expected) or nl(text) != nl(expected):
            return "見出しか（ねゆき）・（いけび）の行が変わった"
    return None


# ---------- コマンド ----------

class Lock:
    """apply と ack は、同じ時に一つだけ。鍵は seal の子にも渡す（seal を見る）。"""
    fd = None

    def __enter__(self):
        STATE.mkdir(parents=True, exist_ok=True)
        self.f = (STATE / ".lock").open("a")
        try:
            fcntl.flock(self.f, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            self.f.close()
            raise Refuse("ほかの apply か ack が動いている（前に止めた apply の、seal の子がまだ動いていることもある。終わるのを待つ）")
        except OSError:
            self.f.close()
            raise
        Lock.fd = self.f.fileno()
        return self

    def __exit__(self, *exc):
        # LOCK_UN はしない（同じ鍵を持つ子が残っていれば、その子が終わるまで外れないように。閉じるだけ）
        Lock.fd = None
        self.f.close()


def load_bases(batch, refs):
    bases = {n: base_from_refs(refs, batch["base"], n, batch["records"][n]) for n in batch["records"]}
    if "yuugao" not in bases:
        bases["yuugao"] = Base("yuugao", ref_path(refs, batch["base"], "yuugao").read_text(encoding="utf-8"), "")
    if policy_of(bases["yuugao"].text) != batch.get("policy"):
        raise Refuse("守りのよりどころ（控えの夕顔と清单の第三版）が、束と合わない")
    return bases


def cmd_prepare(a):
    data = json.loads(Path(a.cands).read_text(encoding="utf-8"))
    sources = [s for s in a.sources.split(",") if s]
    if a.base not in sources:
        raise Refuse("控えの版は、出どころの版のひとつでなければならない")
    batch = build_units(data["items"], a.refs, a.base, sources, data.get("skipped", []))
    if a.one_off:
        batch["one_off"] = True
    Path(a.out).parent.mkdir(parents=True, exist_ok=True)
    Path(a.out).write_text(json.dumps(batch, ensure_ascii=False, indent=1), encoding="utf-8")
    write_packets(batch, a.packets, load_bases(batch, a.refs))
    print(f"直し {len(batch['units'])}、入れないもの {len(batch['skipped'])}")
    if a.one_off:
        print(f"一回きりの束の番号: {batch_id(batch)}（apply のときに --one-off でこの番号を渡す）")


def cmd_round2(a):
    b1 = json.loads(Path(a.units).read_text(encoding="utf-8"))
    bases = load_bases(b1, a.refs)
    check_batch(b1, a.refs, bases, traces(bases["yuugao"].text))
    v1 = load_verdicts(a.verdicts, b1["units"])
    by = {u["unit"]: u for u in b1["units"]}
    revs = []
    files = sorted(Path(a.revisions).glob("*.json")) if Path(a.revisions).is_dir() else [Path(a.revisions)]
    for f in files:
        revs += json.loads(f.read_text(encoding="utf-8"))
    units, seen = [], set()
    for r in revs:
        u = by.get(r.get("unit"))
        if not u or r["unit"] in seen:
            raise Refuse(f"考え直しが、束にないか、二つある: {r.get('unit')}")
        if v1[u["unit"]] != "よくない":
            raise Refuse(f"「いい」だった直しを考え直している: {u['unit']}")
        if r.get("prev") != u["digest"]:
            raise Refuse(f"考え直しの、前の摘要が合わない: {u['unit']}")
        seen.add(r["unit"])
        n = dict(u, after=r["after"], round=2, prev=u["digest"])
        n["digest"] = unit_digest(n, b1["records"][u["record"]])
        units.append(n)
    b2 = {"round": 2, "base": b1["base"], "sources": b1["sources"], "policy": b1["policy"], "records": b1["records"],
          "units": units, "skipped": []}
    Path(a.out).parent.mkdir(parents=True, exist_ok=True)
    Path(a.out).write_text(json.dumps(b2, ensure_ascii=False, indent=1), encoding="utf-8")
    write_packets(b2, a.packets, bases)
    print(f"考え直し {len(units)}")


def cmd_apply(a):
    with Lock():
        return apply_locked(a)


def apply_locked(a):
    Path(a.report).parent.mkdir(parents=True, exist_ok=True)
    report = {"dry_run": bool(a.dry_run), "batch": None, "records": [], "units": [], "skipped": [], "notes": []}

    def save_report():
        Path(a.report).write_text(json.dumps(report, ensure_ascii=False, indent=1), encoding="utf-8")

    hist_path, write_path, gate_path, note_path = (STATE / "判断.jsonl", STATE / "書き込み.jsonl",
                                                   STATE / "門.jsonl", STATE / "知らせ.jsonl")
    if not a.dry_run:
        # 前の書き込みの後始末は、今回の束によらない（束が古くなって読めなくても、先に済ませる）
        try:
            report["recovered"] = recover(write_path)
        except Halt as e:
            report["records"].append({"record": "（前の書き込み）", "result": "止めた", "why": str(e)})
            print(f"止めた（{e}）"); save_report()
            return 3
    b1 = json.loads(Path(a.units).read_text(encoding="utf-8"))
    one_off = bool(b1.get("one_off"))
    if one_off != bool(a.one_off):
        raise Refuse("一回きりの束には --one-off（束の番号）をつける。ふだんの束にはつけない")
    if one_off and a.units2:
        raise Refuse("一回きりの束では、考え直しの段は回さない")
    line = None if one_off else read_line()
    bases = load_bases(b1, a.refs)
    all_traces = traces(bases["yuugao"].text)
    check_batch(b1, a.refs, bases, all_traces)
    v1 = load_verdicts(a.verdicts, b1["units"])
    b2u, v2 = {}, {}
    if a.units2:
        b2 = json.loads(Path(a.units2).read_text(encoding="utf-8"))
        b2u = check_round2(b1, b2, v1)
        v2 = load_verdicts(a.verdicts2, b2["units"])
    bid = batch_id(b1)
    if one_off and a.one_off != bid:
        raise Refuse("--one-off の番号が、この束の番号と違う（prepare が出した番号を、そのまま渡す）")
    verified = verified_items()
    names = people(bases["yuugao"].text)
    if one_off:
        history, gates, noted = [], [], set()   # 一回きりの束は、ふだんの判断・門・知らせの記録を読まず、書かない
    else:
        history, gates, noted = read_jsonl(hist_path), read_jsonl(gate_path), {r["digest"] for r in read_jsonl(note_path)}
    report.update(batch=bid, skipped=b1.get("skipped", []), one_off=one_off)
    status = 0

    def deliver_notes():
        """続けて「通った」が 50 に届いたところのうち、まだ知らせていないものを、出来事として記録してから、
        頼み.md を出来事の記録から作り直す（途中で止まっても、漏れず、二重にならない）。"""
        if one_off:
            return
        events = read_jsonl(note_path)
        for d in run_milestones(history):
            if d in noted:
                continue
            ev = {"digest": d, "date": dt.date.today().isoformat(), "at": now_iso()}
            report["notes"].append(note_text(ev))
            noted.add(d)
            events.append(ev)
            if not a.dry_run:
                append_jsonl(note_path, [ev])
        if not a.dry_run and events:
            view = "".join(f"- {note_text(ev)}\n" for ev in events)
            target = STATE / "頼み.md"
            if not target.exists() or target.read_text(encoding="utf-8") != view:
                tmp = STATE / "頼み.md.tmp"
                tmp.write_text(view, encoding="utf-8")
                os.replace(tmp, target)

    deliver_notes()                               # 前の回で数えたまま、知らせそびれたものがあれば
    done = {r["plain_sha256_expected"] for r in read_jsonl(write_path) if r.get("phase") == "済"}
    for name in sorted(b1["records"], key=order_key):
        base = bases[name]
        mine = sorted([u for u in b1["units"] if u["record"] == name], key=lambda u: u["start"])
        if not mine:
            continue
        cspans = clue_spans_of(base, all_traces)
        firsts, chosen = [], []
        for u in mine:
            g1 = guard(u, u["after"], base, names, verified, cspans)
            ok1 = v1[u["unit"]] == "いい" and g1 is None
            firsts.append(ok1)
            if ok1:
                chosen.append((u, u["after"])); outcome = "入れる（一度目）"
            elif v1[u["unit"]] == "いい":
                outcome = f"関所：{g1}"                   # 「いい」でも関所に当たったものは、考え直しに回さない
            elif u["unit"] in b2u:
                u2 = b2u[u["unit"]]
                g2 = guard(u2, u2["after"], base, names, verified, cspans)
                if v2[u["unit"]] == "いい" and g2 is None:
                    chosen.append((u2, u2["after"])); outcome = "入れる（考え直し）"
                else:
                    outcome = "考え直しも通らなかった" if g2 is None else f"考え直しが関所：{g2}"
            else:
                outcome = "判断がよくない"
            report["units"].append({"unit": u["unit"], "record": name, "items": [i["id"] for i in u["items"]], "outcome": outcome})
        if one_off:
            passed = True                                # 線は使わない。関所と、一つずつの判断はそのまま
        else:
            dkey = digest_of(sorted(u["digest"] for u in mine))
            prior_gate = next((g for g in gates if g["record"] == name and g["digests"] == dkey), None)
            passed, firsts = record_gate(history, mine, firsts, line)
            if prior_gate is not None:
                passed = prior_gate["passed"]            # 一度決めた門は、やり直しても変えない
            known = {h["digest"] for h in history}
            new_rows = [{"digest": u["digest"], "record": name, "unit": u["unit"], "batch": bid, "pass": p, "at": now_iso()}
                        for u, p in zip(mine, firsts) if u["digest"] not in known]
            history = history + new_rows
            if not a.dry_run:
                append_jsonl(hist_path, new_rows)
                if prior_gate is None:
                    row = {"record": name, "digests": dkey, "batch": bid, "passed": passed, "at": now_iso()}
                    append_jsonl(gate_path, [row])
                    gates.append(row)
            deliver_notes()

        def finish(result, why):
            report["records"].append({"record": name, "result": result, "why": why})
            print(f"{name}：{result}（{why}）")

        if not passed:
            for r in report["units"]:
                if r["record"] == name and r["outcome"].startswith("入れる"):
                    r["outcome"] = "線に届かず、この記録は入れない"
            finish("通らなかった", "この記録は、今回は入れなかった"); status = max(status, 1); continue
        if not chosen:
            finish("通った", "入れる直しはなかった"); continue
        try:
            expected = compose(base.text, chosen)
        except Refuse as e:
            finish("通らなかった", str(e)); status = max(status, 1); continue
        why = record_checks(base, expected, clues_for(name, all_traces))
        if why:
            finish("通らなかった", f"close の前の確かめ：{why}"); status = max(status, 1); continue
        if a.dry_run:
            finish("通った", "dry-run"); continue
        try:
            if not same_policy(b1["policy"]):
                raise Halt("守りのよりどころ（夕顔の引用か人の名前、清单の第三版）が、束を作ったあとで変わったか、読めない。確かめて、束を作り直す")
            if plain_sha(expected) in done and current_plain(name) == plain_sha(expected):
                finish("通った", "もう書いてある"); continue
            adopted = [{"unit": u["unit"], "digest": u["digest"], "round": u["round"], "items": len(u["items"])} for u, _ in chosen]
            res = write_record(base, expected, adopted, len(mine) - len(adopted), write_path, bid, b1["policy"], one_off)
        except Halt as e:
            finish("止めた", str(e)); save_report()
            return 3
        if res is True:
            finish("通った", "書いた")
        else:
            finish("通らなかった", res); status = max(status, 1)
    save_report()
    return status


def cmd_ack(a):
    """「どちらでもない」で止まった書き込みに、人が確かめたという印をつけてから、その書き込みの作業フォルダを片づける。
    記録には触らない。片づけが済まなければ、止まったまま（次の apply も、片づけが済むまで動かない）。"""
    with Lock():
        write_path = STATE / "書き込み.jsonl"
        rows = [r for r in read_jsonl(write_path) if r["tx"] == a.tx]
        if not rows or rows[-1]["phase"] != "どちらでもない":
            raise Refuse("その書き込みは、「どちらでもない」で止まっていない")
        if not a.note.strip():
            raise Refuse("何を確かめたかを --note に書く")
        acked = dict(rows[-1], phase="確かめた", acked=True, note=a.note, at=now_iso())
        append_jsonl(write_path, [acked])
        if acked.get("workdir"):
            left, _ = owned_leftovers(acked, after_ack=True)
            if left:
                append_jsonl(write_path, [dict(acked, phase="片づけ残り", left=left, at=now_iso())])
                raise Halt(f"印はつけたが、作業フォルダに片づけられないものがある: {', '.join(left)}")
        append_jsonl(write_path, [dict(acked, phase="片づけた", left=[], at=now_iso())])
        print("印をつけて、片づけた")


def main(argv=None):
    p = argparse.ArgumentParser(prog="mizukagami.py")
    sub = p.add_subparsers(dest="cmd", required=True)
    s = sub.add_parser("prepare")
    s.add_argument("--cands", required=True); s.add_argument("--refs", required=True)
    s.add_argument("--base", required=True); s.add_argument("--sources", required=True)
    s.add_argument("--out", required=True); s.add_argument("--packets", required=True)
    s.add_argument("--one-off", action="store_true")
    s = sub.add_parser("round2")
    s.add_argument("--units", required=True); s.add_argument("--verdicts", required=True)
    s.add_argument("--revisions", required=True); s.add_argument("--refs", required=True)
    s.add_argument("--out", required=True); s.add_argument("--packets", required=True)
    s = sub.add_parser("apply")
    s.add_argument("--units", required=True); s.add_argument("--verdicts", required=True)
    s.add_argument("--units2"); s.add_argument("--verdicts2")
    s.add_argument("--refs", required=True); s.add_argument("--report", required=True)
    s.add_argument("--dry-run", action="store_true")
    s.add_argument("--one-off", metavar="束の番号")
    s = sub.add_parser("ack")
    s.add_argument("--tx", required=True); s.add_argument("--note", required=True)
    a = p.parse_args(argv)
    if a.cmd == "apply" and bool(a.units2) != bool(a.verdicts2):
        p.error("--units2 と --verdicts2 は一緒に")
    try:
        if a.cmd == "prepare":
            cmd_prepare(a); return 0
        if a.cmd == "round2":
            cmd_round2(a); return 0
        if a.cmd == "ack":
            cmd_ack(a); return 0
        return cmd_apply(a)
    except Refuse as e:
        print(f"Refuse: {e}", file=sys.stderr)
        return 2
    except Halt as e:
        print(f"Halt: {e}", file=sys.stderr)
        return 3
    except OSError as e:
        print(f"{type(e).__name__}: {e}", file=sys.stderr)
        return 3


if __name__ == "__main__":
    sys.exit(main())
