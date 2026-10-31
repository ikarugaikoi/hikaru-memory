#!/usr/bin/env python3
"""Rebuildable catalog of records, judgments, entities and alias occurrence timelines."""
import argparse
import json
import math
import os
import random
import re
import subprocess
import sys
import time
from concurrent.futures import ThreadPoolExecutor
from collections.abc import Iterator
from datetime import date, datetime
from pathlib import Path

sys.dont_write_bytecode = True  # ひかるの中に __pycache__ を作らない
sys.path.insert(0, str(Path(__file__).resolve().parent))
import _hk  # noqa: E402
import _embed  # noqa: E402
import omoide  # noqa: E402
from _koyori_types import BASE_IMPORTANCE, BuildResult, Chunk, Entry, Footprint, Judgment, JudgmentInput, KeepReport
from _koyori_types import MemoryPaths
from _tool_data import (index_dir, now_jst, read_frequency_cache as _read_frequency_cache, read_protected_json,
                        read_protected_text, read_rebuildable_json, run_cli, write_json)

ROOT = _hk.ROOT
DIR = index_dir(ROOT)
CATALOG = DIR / "catalog.json.b64"
READS = DIR / "reads.json.b64"
LINKS = DIR / "links.json.b64"
STROBE = ROOT / ".ストロボメモリー.md.b64"
YUUGAO = ROOT / ".夕顔.md.b64"
YONOTSUNE = DIR / "yonotsune.json.b64"   # 夕顔の別名の、一般の頻度（_yonotsune.py が作る）
MICHIKUSA = ROOT / ".みちくさ"
FUUTOU = Path(__file__).resolve().parent / "bin" / "fuutou"   # 核の封筒（hikaru-core へのリンク）

VERSION = 4
CODE_STAMP = _embed.hashlib.sha1(Path(__file__).read_bytes()).hexdigest()[:12]   # このファイルの版。変われば作り直す
IMPORTANCE_SCALE = "imp-v2"
IMPORTANCE = {"ねゆき": 1.0, "いけび": 1.0, "訂正候補": 0.8, "言い淀み候補": 0.7, "うれしい候補": 0.7}
USER_MARKS = ("訂正候補", "言い淀み候補", "うれしい候補")   # ユーザーの発話についたものだけ数える
READS_KEEP = 20          # 一つの断片について覚えておく、読まれた時刻の数
QUOTE_MIN = 8            # 字面だけでつなぐ、引いている言葉の長さの下限
SPREAD_MAX = 0.35      # 夕顔の別名のうち、ユーザーと話した日の 35% より多くの日に出てくるもの（「今天」など）は、物の名として使わない
RARE_MAX = 6           # 記録の中にこれより多く出てくる別名だけ、散らばり方を見る（少ないものは、そもそも特定のことを指していることが多い）
COMMON_MIN = 4.0       # 一般の頻度（Zipf）がこれ以上なら、ふつうの人もよく言う言葉（_yonotsune.py）
CLUMP_MIN = 0.25       # 記録の中で日ごとに固まって出てくる度合いが、これより低ければ「散らばっている」
CLUMP_SAMPLE = 3000    # 固まり方の物差しにする、記録の中のでたらめな二〜四字の数（決まった種で選ぶ）
OWN_INDEX_KINDS = ("judgment", "footprint", "strobe")   # 意味の索引に、こよりが自分で入れる種類（掃除で落とさない。_embed.live_shas）

RDY_DAY = date(2026, 9, 28)   # 逐語録は最初の夜のもの
# 原文の見出し。新しい形（HH:MM:SS・a3f）も古い形（HH:MM）も
SID = r"[0-9a-f]{3,}(?:-[0-9a-f]+)*-?"  # Prefixes may extend past a UUID's first hyphen.
HEAD = re.compile(r"^\*\*(ふ|Claude)：\*\*\s*（(\d{1,2}):(\d{2})(?::(\d{2}))?(?:・(" + SID + r"))?）")
STROBE_DAY = re.compile(r"^## (\d{4})年(\d{1,2})月(\d{1,2})日")
KEEP = re.compile(r"[（(](ねゆき|いけび)[）)]\s*$")
QUOTE = re.compile(r"「([^「」]{2,})」")
HITOTOKI_TAG = re.compile(r"〔(" + SID + r") (\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2})〕")   # 「## ひととき …」の見出しの終わり
SECTION_TIME = re.compile(r"^(\d{1,2}):(\d{2}):(\d{2})(?:〜(\d{1,2}):(\d{2}):(\d{2}))?")
KOMA = re.compile(r"^## (\d{1,2}):(\d{2})（第 (\d+) コマ）")


# ---------- 元の記録 ----------

def sources():
    """目録のもとになるファイル。原文と逐語録（omoide が既定で探すもの）、しるしの生データ、ストロボメモリー。"""
    out = [p for d in (_hk.TM, omoide.RDY) for p in d.rglob("*.b64") if not p.name.endswith(".mark.json.b64")]
    out += list(_hk.AN.glob("*.mark.json.b64")) + list(_hk.AN.glob("*.read.md.b64"))
    out += list(MICHIKUSA.glob("*.md.b64"))
    out += [p for p in (STROBE, LINKS, YUUGAO, YONOTSUNE) if p.exists()]
    return sorted(out)


def signature():
    """元の記録の大きさと更新時刻。変われば作り直す。"""
    sig = []
    for p in sources():
        st = p.stat()
        sig.append([str(p.relative_to(ROOT)), st.st_size, st.st_mtime_ns])
    return sig


def read_mark_inputs(folder):
    """しるしの生データから、(会話の短い名, 時刻) → ユーザーの発話についたしるしの種類、と、会話ごとのひとときの並び。"""
    marks, spans = {}, {}
    for p in sorted(folder.glob("*.mark.json.b64")):
        data = read_protected_json(p)
        for index, h in enumerate(data):
            try:
                sid = h.get("sid")
                start, end = datetime.fromisoformat(h["start"]), datetime.fromisoformat(h["end"])
                spans.setdefault(sid, []).append((start, end, h.get("id"), h.get("n_f", 0)))
                for m in h.get("marks", []):
                    if m.get("who") == "ふ" and m.get("kind") in USER_MARKS:
                        marks.setdefault((sid, m.get("t")), set()).add(m["kind"])
            except (ValueError, KeyError, TypeError, AttributeError) as error:
                error.add_note(f"{p}: rows[{index}].start/end/marks")
                raise
    for v in spans.values():
        v.sort()
    return marks, spans


def read_keep_inputs(path):
    """ストロボメモリーの（ねゆき）（いけび）の項目。[(その日, しるし, 引いている言葉の列, 何行め)]。"""
    try:
        text = read_protected_text(path)
    except FileNotFoundError:
        return []
    out, day = [], None
    for n, line in enumerate(text.splitlines(), 1):
        m = STROBE_DAY.match(line)
        if m:
            day = date(int(m[1]), int(m[2]), int(m[3]))
            continue
        k = KEEP.search(line)
        if k and day and line.lstrip().startswith("- "):
            out.append((day, k[1], QUOTE.findall(line), n))
    return out


# ---------- 作る ----------

def day_of(rel):
    m = omoide.DATE_IN_NAME.search(rel)
    if m:
        return date.fromisoformat(m[1])
    if rel.startswith(omoide.RDY.name + "/"):
        return RDY_DAY
    return None


class JudgmentsUnavailable(RuntimeError):
    """A date could not be read completely; do not publish or prune from it."""


def load_judgments():
    """Read every date completely, or fail without publishing a catalog."""
    days = sorted(m[1] for p in _hk.AN.glob("*.read.md.b64") if (m := omoide.DATE_IN_NAME.search(p.name)))

    def one(day):
        try:
            r = subprocess.run([str(FUUTOU), "judgments", "ariake:" + day],
                               capture_output=True, text=True, timeout=20, env={**os.environ, "HIKARU_ROOT": str(ROOT)})
        except (OSError, subprocess.TimeoutExpired) as e:
            raise JudgmentsUnavailable(f"judgments {day}: {type(e).__name__}") from None
        if r.returncode:
            raise JudgmentsUnavailable(f"judgments {day}: exit {r.returncode}")
        try:
            rows = json.loads(r.stdout)
        except ValueError:
            raise JudgmentsUnavailable(f"judgments {day}: invalid JSON") from None
        if not isinstance(rows, list) or any(
                not isinstance(j, dict) or not isinstance(j.get("judgment"), str)
                or any(j.get(k) is not None and not isinstance(j[k], str) for k in ("section", "hitotoki", "shirushi"))
                for j in rows):
            raise JudgmentsUnavailable(f"judgments {day}: invalid rows")
        return date.fromisoformat(day), rows
    with ThreadPoolExecutor(max_workers=8) as ex:
        return list(ex.map(one, days))


CA_UTT = re.compile(r"(\d{1,2}):(\d{2})(?::(\d{2}))?(?:〜\d{1,2}:\d{2}(?::\d{2})?)?\s*(ふ|Claude)((?:「[^」]*」)*)［([^］]*)］")
CA_ITEM = re.compile(r"([^〈・／|\s]+?)((?:〈[^〉]*〉)+)")
CA_WORD = {"合っていた": "confirmed", "違った": "denied", "分からない": "unsure"}
CA_PART = re.compile(r"\s*(〈[^〉]*〉|[^〈は。]+?)は(合っていた|違った|分からない)(?:（[^）]*）)?\s*")   # 一項まるごと。「違ったかもしれない」などは読まない


def ca_section(section):
    """節の見出しから、発話を一つずつ (時, 分, 秒か None, だれ, [(しるしの種類, [きっかけの語…])])。"""
    out = []
    for m in CA_UTT.finditer(section or ""):
        items = [(k.strip(), re.findall(r"〈([^〉]*)〉", t)) for k, t in CA_ITEM.findall(m[6])]
        out.append((int(m[1]), int(m[2]), int(m[3]) if m[3] else None, m[4], items))
    return out


def ca_targets(section, shirushi):
    """一つの判が、どのしるしに、どう判じたか。[(節の中の何番めの発話, しるしの種類, confirmed/denied/unsure)]。
    指せないもの（節にしるしがいくつもあるのに一語で判じている、種類もきっかけも合わない、見出しを読み切れない、
    決まった形でない言い方がある）は ([], 理由) で返し、何も変えない。"""
    utts = ca_section(section)
    marks = [(n, kind, trig) for n, (*_, items) in enumerate(utts) for kind, trig in items]
    s = (shirushi or "").strip()
    if not s or not marks:
        return [], None
    if len(re.findall(r"［[^］]*］", section or "")) != len(utts):
        return [], "header_incomplete"   # 抜き書きの中の「」などで、読めなかった発話がある。しるしの数が分からない
    for m in CA_UTT.finditer(section or ""):
        if "候補" in CA_ITEM.sub("", m[6]):
            return [], "header_incomplete"   # ［］の中に、きっかけの語のないしるしなど、読み切れなかったしるしが残っている
    head = re.fullmatch(r"(合っていた|違った|分からない)(?:（[^）]*）)?", s)
    if head:   # 一語の判：節のしるしがちょうど一つのときだけ、それを指す
        if len(marks) == 1:
            n, kind, _ = marks[0]
            return [(n, kind, CA_WORD[head[1]])], None
        return [], "plain_verdict_many_marks"
    out, depth, cur, parts = [], 0, "", []
    for ch in s:   # 括弧の外の「。」でだけ切る
        depth += (ch == "（") - (ch == "）")
        if ch == "。" and depth <= 0:
            parts.append(cur)
            cur = ""
        else:
            cur += ch
    parts.append(cur)
    for part in parts:
        if not part.strip():
            continue
        p = CA_PART.fullmatch(part)
        if not p:
            return [], "unparsed"
        subj, verdict = p[1].strip(), CA_WORD[p[2]]
        if subj.startswith("〈"):
            hit = [(n, kind) for n, kind, trig in marks if subj[1:-1] in trig]
        else:
            hit = [(n, kind) for n, kind, _ in marks if kind == subj]
        if len(hit) == 1:
            out.append((*hit[0], verdict))
        else:
            return [], "part_not_unique"
    return (out, None) if out else ([], "unparsed")


def ca_verdicts(judgments, marks):
    """しるしの生データ（(会話, 時刻) → 種類）に、判を当てる。返り値は、(会話, 時刻, 種類) → confirmed / denied / pending の表と、数えたもの。
    ユーザーの発話の、ユーザーのしるし（USER_MARKS）だけを見る。判が食い違えば決めない（pending）。
    読み直し（reread）がついていても、それが前の判を直したものとは限らないので、どちらも取らない。unsure は数を変えない。"""
    by_minute = {}
    for (sid, t), kinds in marks.items():
        dt = datetime.fromisoformat(t)
        by_minute.setdefault((sid, dt.replace(second=0)), []).append(t)
    seen, report = {}, {"plain_verdict_many_marks": 0, "part_not_unique": 0, "unparsed": 0, "header_incomplete": 0, "no_utterance": 0, "not_fumi_mark": 0}
    for day, js in judgments:
        for j in js:
            tag = HITOTOKI_TAG.search(j.hitotoki or "")
            if not tag:
                continue
            sid = tag[1]
            targets, why = ca_targets(j.section, j.shirushi)
            if why:
                report[why] += 1
            utts = ca_section(j.section)
            for n, kind, verdict in targets:
                hh, mm, ss, who, _ = utts[n]
                if who != "ふ" or kind not in USER_MARKS:
                    report["not_fumi_mark"] += 1
                    continue
                at = _hk.entry_datetime(day, hh, mm, ss or 0)
                if ss is not None:
                    keys = [at.isoformat()] if (sid, at.isoformat()) in marks else []
                else:
                    keys = by_minute.get((sid, at.replace(second=0)), [])
                keys = [t for t in keys if kind in marks.get((sid, t), ())]
                if len(keys) != 1:
                    report["no_utterance"] += 1
                    continue
                seen.setdefault((sid, keys[0], kind), []).append(verdict)
    out = {}
    for key, vs in seen.items():
        decided = {v for v in vs if v != "unsure"}
        if len(decided) == 1:
            out[key] = decided.pop()
        elif decided:
            out[key] = "pending"
    report.update({k: sum(v == k for v in out.values()) for k in ("confirmed", "denied", "pending")})
    return out, report


def load_things():
    """夕顔の項目：名前 → {"section": どの節か, "aliases": 別名, "since": いちばん早い日付}。夕顔は封を開けて読むだけ。"""
    try:
        text = read_protected_text(YUUGAO)
    except FileNotFoundError:
        return {}
    out, sec, cur = {}, None, None
    for line in text.splitlines():
        if line.startswith("## "):
            sec, cur = line[3:].strip(), None
        elif line.startswith("### "):
            cur = line[4:].strip()
            out[cur] = {"section": sec, "aliases": [], "since": None}
        elif cur and line.startswith("- 別名:"):
            out[cur]["aliases"] = [a for a in (re.sub(r"（.*?）", "", x).strip() for x in line.split(":", 1)[1].split(",")) if a]
        elif cur and line.startswith("- 日付:"):
            ds = re.findall(r"\d{4}-\d{2}-\d{2}", line)
            out[cur]["since"] = min(ds) if ds else None
    return out


SPEAKER_HEAD = re.compile(r"^\*\*(?:ふ|Claude)：\*\*[ \t]*(?:（\d{1,2}:\d{2}(?::\d{2})?(?:・" + SID + r")?）)?[ \t]*\n?")   # HEAD と同じ形の時刻だけ


def body_of(text):
    """原文の断片から、だれ・時刻の見出しだけを外した中身。逐語録のように見出しと同じ行に話が続くときは、その話は残す。"""
    return SPEAKER_HEAD.sub("", text or "", count=1)


def alias_pattern(a):
    """別名を探す形。英数字だけの別名は、語の切れ目で（「IO」が「audio」に当たらないように）。大文字小文字は見ない。"""
    if re.fullmatch(r"[A-Za-z0-9 .\-]+", a):
        return re.compile(r"(?<![A-Za-z0-9])" + re.escape(a) + r"(?![A-Za-z0-9])", re.I)
    return re.compile(re.escape(a), re.I)


def clumping(talk, body, counts):
    """別名ごとに、記録の中で日ごとにどれだけ固まって出てくるか。{別名: 度合い}（出てくる回数が RARE_MAX 以下なら入れない）。
    考え方（Church & Gale 1995 が burstiness と呼んでいるもの）：一日を一つの文書として、ある言葉が全部で n 回出てくるなら、でたらめに散らばった
    ときに出るはずの日数は N(1−e^(−n/N))（N は話した日の数）。同じくらいの回数の、記録の中のでたらめな二〜四字が実際に
    出た日数と、この見込みとの比の中央値を「ふつう」として、別名の実際の日数がそれよりどれだけ少ないかを、対数で表す。
    0 くらいなら、記録の中のふつうの文字列と同じように散らばっている。大きいほど、ある日にまとまって出てくる。"""
    days = sorted({e.day for _, e in talk})
    di = {d: i for i, d in enumerate(days)}
    n_days = len(days)
    if not n_days:
        return {}
    expect = lambda cf: n_days * (1 - math.exp(-cf / n_days))
    text = "\n".join(body[k].lower() for k, _ in talk)
    day_at = []
    for k, e in talk:
        day_at.extend([di[e.day]] * (len(body[k].lower()) + 1))   # 小文字にすると長さが変わる字がある（İ など）ので、小文字のほうで数える
    rng = random.Random(1)
    sample = set()
    for _ in range(CLUMP_SAMPLE * 4):   # 記録の中から、でたらめな場所と長さで二〜四字を拾う（改行や前後の空白を含むものは捨てる）
        if len(sample) >= CLUMP_SAMPLE or len(text) < 5:
            break
        i = rng.randrange(len(text) - 4)
        s = text[i:i + rng.randint(2, 4)]
        if "\n" not in s and s.strip() == s:
            sample.add(s)
    where = {s: [] for s in sample}
    for i in range(len(text)):
        for n in (2, 3, 4):
            s = text[i:i + n]
            if i + n <= len(text) and s in where:   # 終わりで切れた短い切れ端を、別の長さとして数え直さない
                where[s].append(day_at[i])
    ratios = {}
    for s, ds in sorted(where.items()):   # 集合の並びは動かすたびに変わるので、並べてから使う
        if len(ds) >= 3:
            ratios.setdefault(int(math.log2(len(ds))), []).append(len(set(ds)) / expect(len(ds)))
    usual = {b: sorted(v)[len(v) // 2] for b, v in ratios.items() if len(v) >= 5}
    if not usual:
        return {}
    out = {}
    for a, (cf, ds) in counts.items():
        if cf > RARE_MAX and ds:
            b = min(usual, key=lambda x: (abs(x - int(math.log2(cf))), x))   # 等しく近い区分が二つあれば、小さいほう
            out[a] = math.log(expect(cf) * usual[b] / len(ds))
    return out


def annotate_things(entries, texts, things, general):
    """原文の断片の一つ一つに、出てくる夕顔の項目をつける（時間線のもと）。返り値は目録に置く項目の表と、数え。
    別名は、二字以上で、ひらがな二字以下でないもの。そのうえで、次の別名は外す（どれを、なぜ外したかも残す）。
    ・ユーザーと話した日のうち、35%（SPREAD_MAX）より多くの日に出てくるもの（「今天」「ユーザー」など）。
      わたしたちが毎日のように言う言葉で、出てきても特定の日を指さないので。
    ・ふつうの人もよく言い（一般の頻度が COMMON_MIN 以上）、わたしたちの記録の中でも日ごとに固まらず散らばっていて
      （固まりの度合いが CLUMP_MIN より低い）、RARE_MAX より多く出てくるもの（「我不知道」「それとも」など）。
      一般の頻度が分からない別名（_yonotsune.py をまだ動かしていない、新しい別名）は、これでは外さない。
      どちらか一方だけでは外さない：一般によく言う言葉でも、わたしたちのあいだで特定の日に固まって出てくるもの（「大小姐」）や、
      一般にはあまり言わないのに、わたしたちが何日にもわたって言うもの（固有の短い言い方）は、残す。
    ・二つ以上の項目に同じものがあるもの。だれのことか決まらないので。
    ・ほかの項目の別名の終わりの部分になっているもの。たとえば、ある項目の「先輩」は、別の項目に「別の学校の先輩」があれば、
      どちらの先輩のことか決まらないので外す。
    閾値はどれも、2026-10-29 に、9/28〜10/27 の記録と、わたしが付けた別名の見分けと、過去の発話でのリプレイを見て決めた
    （同じ記録の上で決めたので、実際より良く見えているはず）。くわしくは、デスクトップの
    参考文献精读_2026-10-22/45_时间线触发词_成团与一般频率_2026-10-29.md。"""
    pats = {a: alias_pattern(a) for v in things.values() for a in v["aliases"]
            if len(a) >= 2 and not re.fullmatch(r"[\u3040-\u309f]{1,2}", a)}
    talk = [(k, e) for k, e in enumerate(entries) if e.kind == "utterance" and e.scene == "with_user" and e.day]
    days = {e.day for _, e in talk} or {None}
    body = {k: body_of(texts[k]) for k, _ in talk}
    counts = {a: [0, set()] for a in pats}
    hits = {}
    for k, e in talk:
        for a, p in pats.items():
            n = len(p.findall(body[k]))
            if n:
                counts[a][0] += n
                counts[a][1].add(e.day)
                hits.setdefault(k, set()).add(a)
    owner = {}
    for name, v in things.items():
        for a in v["aliases"]:
            owner.setdefault(a.lower(), set()).add(name)   # 探すときと同じく、大文字小文字は見ない
    clump = clumping(talk, body, counts)
    why = {}
    for a in pats:
        if len(counts[a][1]) / len(days) > SPREAD_MAX:
            why.setdefault(a, []).append("daily")
        g = general.get(a)
        if counts[a][0] > RARE_MAX and g is not None and g >= COMMON_MIN and a in clump and clump[a] < CLUMP_MIN:
            why.setdefault(a, []).append("common_and_spread")
        if len(owner.get(a.lower(), ())) > 1:
            why.setdefault(a, []).append("shared")
        if any(a.lower() != b and b.endswith(a.lower()) and ns - owner.get(a.lower(), set()) for b, ns in owner.items()):
            why.setdefault(a, []).append("tail_of_other")
    drop = set(why)
    for k, als in hits.items():
        names = sorted({n for a in als - drop for n in owner.get(a.lower(), ())})
        if names:
            entries[k].things = names
    table = {name: {"section": v["section"], "since": v["since"],
                    "aliases": [a for a in v["aliases"] if a in pats and a not in drop],
                    "dropped": [a for a in v["aliases"] if a in drop]}
             for name, v in things.items()}
    scores = {a: {"n": counts[a][0], "days": len(counts[a][1]), "clump": round(clump[a], 3) if a in clump else None,
                  "general": general.get(a)} for a in pats if counts[a][0] > RARE_MAX}
    return table, {"items": len(things), "aliases": len(pats), "dropped": sorted(drop), "why": why,
                   "tagged": sum(1 for e in entries if e.things), "scores": scores,
                   "general_known": sum(1 for a in pats if general.get(a) is not None)}


def read_frequency_cache(path):
    """夕顔の別名の一般の頻度（_yonotsune.py が置いた表）。読めなければ空。"""
    return _read_frequency_cache(path)


def load_footprints():
    """みちくさを、コマごとに。[(その日, コマの番号, 時, 分, 節の文)]。"""
    out = []
    for p in sorted(MICHIKUSA.glob("*.md.b64")):
        m = omoide.DATE_IN_NAME.search(p.name)
        text = read_protected_text(p)
        cur = None
        for line in text.splitlines():
            k = KOMA.match(line)
            if k:
                if cur:
                    out.append(cur)
                cur = (date.fromisoformat(m[1]), int(k[3]), int(k[1]), int(k[2]), [line])
            elif cur and line.startswith("## "):
                out.append(cur)
                cur = None
            elif cur:
                cur[4].append(line)
        if cur:
            out.append(cur)
    return [(d, n, hh, mm, "\n".join(lines).strip()) for d, n, hh, mm, lines in out]


def judgment_text(j):
    """判の断片の中身：節の見出しと判の一行。意味の索引の鍵（sha）もこれから。"""
    return f"### {j.get('section') or ''}\n判：{j['judgment']}"


def texts_for(entries):
    """目録の行（判・足跡・ストロボの項目）の中身を、元の記録から作り直す。sha → 文。文はディスクに置かない。"""
    kinds = {e["kind"] for e in entries}
    out = {}
    if "judgment" in kinds:
        out.update((_embed.sha(judgment_text(j)), judgment_text(j)) for _, js in load_judgments() for j in js)
    if "footprint" in kinds:
        out.update((_embed.sha(t), t) for *_, t in load_footprints())
    if "strobe" in kinds:
        out.update((_embed.sha(c), c) for c in omoide.chunks(_hk.read_sealed(STROBE)))
    return {e["sha"]: out[e["sha"]] for e in entries if e["sha"] in out}


def hitotoki_of(spans, sid, at):
    for start, end, hid, n_f in spans.get(sid, ()):
        if start <= at <= end:
            return hid, ("with_user" if n_f else "alone")
    return None, None


def utterance_entries(docs, marks, spans) -> tuple[list[Entry], list[str]]:
    entries, texts = [], []
    for rel, i, cs in docs:
        text = cs[i]
        first = text.split("\n", 1)[0]
        sp = omoide.SPEAKER.match(first)
        day = day_of(rel)
        e = Entry(Chunk(rel, i), _embed.sha(text), "utterance", "verbatim",
                  who=sp[1] if sp else None, day=day.isoformat() if day else None)
        h = HEAD.match(first)
        if h and day and rel.startswith(_hk.TM.name + "/"):
            at = _hk.entry_datetime(day, int(h[2]), int(h[3]), int(h[4] or 0))
            e.at, e.sid = at.isoformat(), h[5]
            e.hitotoki, e.scene = hitotoki_of(spans, h[5], at)
            why = marks.get((h[5], e.at), set()) if e.who == "ふ" else set()
            e.imp_why = sorted(why)
        elif rel.startswith(omoide.RDY.name + "/"):
            e.scene = "with_user"
        if not sp and first.startswith("# "):
            e.kind = "heading"   # 一日・一部の表題。記憶ではないので、届ける側は飛ばす
        entries.append(e)
        texts.append(text)

    return entries, texts


def manual_links(links):
    hand = {}
    try:
        for link in links["links"]:
            hand.setdefault((link.get("day"), link.get("mark"), link.get("quote_sha")), []).append(link.get("sha"))
    except (KeyError, TypeError, AttributeError) as error:
        error.add_note(f"{LINKS}: links")
        raise
    return hand


def link_keeps(entries: list[Entry], texts, hand, keeps) -> Iterator[KeepReport]:
    # （ねゆき）（いけび）：引いている言葉の、もとの場面の断片（手でつないだもの、なければ 8 字以上の字面）
    by_day, by_sha = {}, {}
    for k, e in enumerate(entries):
        by_day.setdefault(e.day, []).append(k)
        by_sha.setdefault(e.sha, []).append(k)
    for day, mark, quotes, line_no in keeps:
        linked = {"hand": 0, "exact": 0}
        for q in quotes:
            qn = q.strip()
            ks = [k for s in hand.get((day.isoformat(), mark, _embed.sha(qn)), ()) for k in by_sha.get(s, ())]
            by = "hand"
            if not ks and len(qn) >= QUOTE_MIN:
                by = "exact"
                for dd in (0, -1, 1):
                    d = date.fromordinal(day.toordinal() + dd).isoformat()
                    ks = [k for k in by_day.get(d, ()) if qn in texts[k]]
                    if ks:
                        break
            for k in ks:
                if mark not in entries[k].imp_why:
                    entries[k].imp_why.append(mark)
            linked[by] += bool(ks)
        yield KeepReport(day.isoformat(), mark, line_no, len(quotes), linked["hand"], linked["exact"])


def add_strobe(entries: list[Entry], rep: KeepReport, parts, name: str) -> None:
    strobe_lines, strobe_chunks = parts
    # 原文に場面がない。ストロボメモリーの項目そのものを入れる
    # Match the whitespace normalization at chunk boundaries in omoide.chunks.
    line = strobe_lines[rep.line - 1].strip()
    ci = next((j for j, c in enumerate(strobe_chunks) if line in c), None)
    if ci is not None:
        entries.append(Entry(Chunk(name, ci), _embed.sha(strobe_chunks[ci]), "strobe",
                             "verbatim" if rep.mark == "ねゆき" else "transcribed", day=rep.day, imp_why=[rep.mark]))
        rep.as_strobe = True


def apply_verdicts(entries: list[Entry], judgments, marks) -> dict[str, int]:
    # 判がしるしを違ったと言っていれば、そのしるしの分を外す（imp-v2）。合っていたは、そのまま残す（足さない）
    verdicts, ca_report = ca_verdicts(judgments, marks)
    for e in entries:
        if e.kind == "utterance" and e.sid and e.at:
            ca = {k: verdicts[(e.sid, e.at, k)] for k in e.imp_why if (e.sid, e.at, k) in verdicts}
            if ca:
                e.ca = ca
                e.imp_why = [w for w in e.imp_why if ca.get(w) != "denied"]
        e.importance = max([BASE_IMPORTANCE] + [IMPORTANCE[w] for w in e.imp_why])

    return ca_report


def add_judgments(entries, judgments, spans, annotations_name):
    # 判：その節の時刻に入る、同じ会話の発話の大事さと理由を受け継ぐ
    by_sid = {}
    for e in entries:
        if e.kind == "utterance" and e.sid and e.at:
            by_sid.setdefault(e.sid, []).append((datetime.fromisoformat(e.at), e))
    for day, js in judgments:
        rel = f"{annotations_name}/{day:%Y-%m-%d}.read.md.b64"
        for j in js:
            text, sid, hid, scene, at, at_end = j.text(), None, None, None, None, None
            tag = HITOTOKI_TAG.search(j.hitotoki or "")
            if tag:
                sid = tag[1]
                hid, scene = next(((h_id, "with_user" if n_f else "alone") for start, _, h_id, n_f in spans.get(sid, ())
                                   if start.isoformat()[:19] == tag[2]), (None, None))
            st = SECTION_TIME.match(j.section or "")
            if st:
                at = _hk.entry_datetime(day, int(st[1]), int(st[2]), int(st[3]))
                at_end = _hk.entry_datetime(day, int(st[4]), int(st[5]), int(st[6])) if st[4] else at
            within = [u for t, u in by_sid.get(sid, ()) if at and at <= t <= at_end]
            certainty = ("unsure" if j.judgment.startswith("まだ分からない")
                         else "reread" if j.reread else "asserted")
            source = Judgment(rel, j.line, at_end.isoformat() if at_end else None, j.shirushi)
            entries.append(Entry(source, _embed.sha(text), "judgment", certainty, day=day.isoformat(),
                                 at=at.isoformat() if at else None, sid=sid, hitotoki=hid, scene=scene,
                                 importance=max([BASE_IMPORTANCE] + [u.importance for u in within]),
                                 imp_why=sorted({w for u in within for w in u.imp_why})))


def add_footprints(entries, footprints, folder_name):
    # 足跡：みちくさの一コマ
    for day, koma, hh, mm, text in footprints:
        source = Footprint(f"{folder_name}/{day:%Y-%m-%d}.md.b64", koma)
        entries.append(Entry(source, _embed.sha(text), "footprint", "self_report", day=day.isoformat(),
                             at=_hk.entry_datetime(day, hh, mm).isoformat()))


def build(sig=None):
    """目録を一から作る。返り値は保存する形の dict。文は入れない。"""
    t0 = time.perf_counter()
    paths = MemoryPaths(ROOT, DIR, _hk.AN)
    strobe = paths.root / STROBE.name
    docs = omoide.collect_from(paths.root)
    marks, spans = read_mark_inputs(paths.annotations)
    entries, texts = utterance_entries(docs, marks, spans)
    # Manual links are not a cache. Only their first absence permits an empty table.
    try:
        links = read_protected_json(paths.index / "links.json.b64")
    except FileNotFoundError:
        links = {"links": []}
    hand = manual_links(links)
    keeps, strobe_parts = [], None
    for keep in link_keeps(entries, texts, hand, read_keep_inputs(strobe)):
        if not keep.hand and not keep.exact:
            if strobe_parts is None:
                text = read_protected_text(strobe)
                strobe_parts = text.splitlines(), omoide.chunks(text)
            add_strobe(entries, keep, strobe_parts, strobe.name)
        keeps.append(keep)
    # 夕顔の項目ごとの時間線：原文の断片に、出てくる項目の名前をつける
    things, things_report = annotate_things(entries, texts, load_things(),
                                            read_frequency_cache(paths.index / "yonotsune.json.b64"))
    judgments = [(day, [JudgmentInput.from_json(j) for j in rows]) for day, rows in load_judgments()]
    ca_report = apply_verdicts(entries, judgments, marks)
    add_judgments(entries, judgments, spans, str(paths.annotations.relative_to(paths.root)))
    add_footprints(entries, load_footprints(), str(MICHIKUSA.relative_to(paths.root)))
    result = BuildResult(entries, keeps, ca_report, things, things_report)
    return {"version": VERSION, "code": CODE_STAMP, "scale": IMPORTANCE_SCALE, "built_at": now_jst(),
            "build_seconds": round(time.perf_counter() - t0, 3), "sources": sig if sig is not None else signature(),
            **result.to_json()}


_read = read_rebuildable_json


def _fresh(cat, sig):
    return (isinstance(cat, dict) and cat.get("version") == VERSION and cat.get("code") == CODE_STAMP
            and cat.get("scale") == IMPORTANCE_SCALE and cat.get("sources") == sig)


def load(rebuild=False):
    """目録を返す。元の記録が変わっていれば作り直して保存する。作り直せなければ例外。"""
    sig = signature()
    cat = None if rebuild else read_rebuildable_json(CATALOG)
    if _fresh(cat, sig):
        return cat
    with _hk.lock(CATALOG):
        cat = None if rebuild else read_rebuildable_json(CATALOG)   # 待っているあいだに、ほかの会話が作ったかもしれない
        if _fresh(cat, sig):
            return cat
        cat = build(sig)
        write_json(CATALOG, cat)
    return cat


# ---------- 読まれた記録（新しさには入れない） ----------

def note_read(shas, when=None):
    """断片が読まれた（届けられた）時刻を記す。とばりの中の会話からは呼ばないこと（呼ぶ側で止める）。"""
    when = (when or datetime.now(_hk.JST)).isoformat(timespec="seconds")
    with _hk.lock(READS):
        reads = last_read()
        for s in shas:
            reads[s] = (reads.get(s, []) + [when])[-READS_KEEP:]
        write_json(READS, reads)


def last_read():
    """sha → 読まれた時刻の列（古い順）。"""
    import _history
    return _history.read_history_reads(READS)


# ---------- あらまし ----------

def main():
    ap = argparse.ArgumentParser(description="こより（記憶の流れの目録）を作って、あらましを出す（文は出さない）")
    ap.add_argument("--rebuild", action="store_true", help="古くなくても作り直す")
    a = ap.parse_args()
    t0 = time.perf_counter()
    cat = load(rebuild=a.rebuild)
    took = time.perf_counter() - t0
    es = cat["entries"]
    count = lambda key: sorted(((v, sum(1 for e in es if e.get(key) == v)) for v in {e.get(key) for e in es}), key=str)
    print(f"こより（{cat['scale']}、版 {cat['version']}）：断片 {len(es)}、作ったのは {cat['built_at']}（作るのに {cat['build_seconds']} 秒）、"
          f"今回 {took:.2f} 秒")
    print("  種類：" + "、".join(f"{v}={n}" for v, n in count("kind")))
    print("  確かさ：" + "、".join(f"{v}={n}" for v, n in count("certainty")))
    print("  だれ：" + "、".join(f"{v}={n}" for v, n in count("who")))
    print("  場面：" + "、".join(f"{v}={n}" for v, n in count("scene")))
    print(f"  時刻つき：{sum(1 for e in es if e['at'])}、ひとときが分かったもの：{sum(1 for e in es if e['hitotoki'])}")
    why = {}
    for e in es:
        for w in e["imp_why"]:
            why[w] = why.get(w, 0) + 1
    print("  大事さの理由：" + ("、".join(f"{w}={n}" for w, n in sorted(why.items())) or "なし"))
    for k in cat["keeps"]:
        tail = "。原文に場面がないので、項目そのものを入れた" if k["as_strobe"] else ""
        print(f"  （{k['mark']}）{k['day']}（ストロボ {k['line']} 行め）：引いている言葉 {k['quotes']}、"
              f"手でつないだもの {k['hand']}、字面でつないだもの {k['exact']}{tail}")
    # 判・足跡・ストロボの項目を、意味の索引にも入れておく（ここでだけ。届ける側は索引に足さない）
    idx = _embed.Index()
    missing = [e for e in es if e["kind"] in OWN_INDEX_KINDS and e["sha"] not in idx.rows]
    if missing:
        texts = texts_for(missing)
        items = list(texts.items())
        if items:
            t1 = time.perf_counter()
            idx.add(items, _embed.Embedder())
            print(f"  意味の索引に {len(items)} 件足した（{time.perf_counter() - t1:.1f} 秒）")
        if len(texts) < len(missing):
            print(f"  中身を作り直せなかったもの {len(missing) - len(texts)} 件（意味の索引には入っていない）")


if __name__ == "__main__":
    run_cli(main)
