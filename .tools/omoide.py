#!/usr/bin/env python3
"""ひかるの記録（.b64）を、平文をディスクに残さずに検索する。

使い方:
  python3 omoide.py 乌冬 うどん udon            # 字でも意味でも探して、関連度順に
  python3 omoide.py 鸡翅 --context 2 --top 5
  python3 omoide.py 九弁 --only タイムマシン       # パスにこの文字列を含むファイルだけ
  python3 omoide.py 組会 --since 2026-09-29 --until 2026-10-05
  python3 omoide.py 丘 --all                     # 全部の記録から（既定は原文だけ。下記）
  python3 omoide.py 一緒に何か食べた夜              # 自然な文で聞いてもいい
  python3 omoide.py おでん --mode lexical          # 字だけ（以前のやり方）
  python3 omoide.py おでん --mode semantic         # 意味だけ
  python3 omoide.py おでん --explain              # 点のつき方も見る

探す範囲:
  既定は原文だけ：.タイムマシン/（会話の記録）と .Rabbit_Deer_and_You/（逐語録）。
  --all で全部の .b64（丘の上、夕顔、ストロボメモリー、日記、ビブリア、read.md など）。
  ただし *.mark.json.b64（しるしの生データ）と .tools/（bak の古い版）はいつも除く。
  --since / --until（YYYY-MM-DD）：ファイル名の日付（朝 5 時で切った「ひかるの日」）で選ぶ。
  週の日記（YYYY-MM-DD〜MM-DD）は、節の見出し「## M月D日」の日で、断片ごとに選ぶ。
  名前に日付のないファイル（逐語録、丘の上など）は、--since / --until を付けたときは対象にしない。

検索のしかた（既定の hybrid）:
  字：文字 2-gram の BM25。語の一部しか合わなくても点がつき、語がそのまま入っていれば加点。
  意：多言語の埋め込みモデル（.tools/_embed.py）で、問いと意味の近い断片を探す。
     「おでん」で「关东煮」の話も出る。語を並べたときは、一つずつと全体の両方で近さを見る。
  二つの順位を RRF（reciprocal rank fusion）で一つにまとめる。両方で上位なら強い。
  索引にない断片があれば、検索のついでに埋め込んで足す（初回は数十秒かかる）。
  モデルは最初に使ったときに裏で起きて、以後はソケット越しに頼むだけ（一回 1 秒足らず）。2 時間呼ばれなければ寝る。
  様子は `python3 .tools/_embed.py --status`、止めるのは `--stop`。
  モデルが使えないときは止める。字だけで探すときは --mode lexical。
  索引に足すだけで、消すことはしない（記録にない断片の行を落とすのは `python3 .tools/_embed.py` の単体実行）。
断片は発話ごと（**ふ：** / **Claude：** の見出し。行頭が「\\」の行は、本文の中で見出しの形をしていた行に
ochibo.py が付けたものなので、見出しとしない）、見出しがなければ空行で区切った段落。
表示は、当たったところの周り --width 字（既定 400）。意味で当たった断片はいちばん近いウィンドウのあたり、
字で当たった断片は語が最初に出た位置のあたり。両方で当たったときは意味のウィンドウを優先。前後の断片（--context）は先頭から。

みせけちの注:
  見せる断片（当たったものも、前後のものも）に、夕顔の「みせけち」の「残っているところ」に当たる言い方があれば、
  その断片のすぐ下に ⤷ で始まる注を添える（_misekechi.chunk_notes）。その言い方が何だったか（読み違い／そのときはそのとおりで、あとで変わった／
  まだ決めていない／場合による）と、番号と、いまの理解。同じことを何度か直していれば、いちばん新しい直しのもの。
  見せるときに添えるだけで、順位は変えない。記録も変えない。夕顔が読めなければ止める。
"""
import argparse
import math
import os
import re
import sys

sys.dont_write_bytecode = True  # ひかるの中に __pycache__ を作らない
from collections import Counter, defaultdict
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import _hk  # noqa: E402
from _tool_data import index_dir, now_local, read_protected_text, run_cli

ROOT = _hk.ROOT
RDY = ROOT / ".Rabbit_Deer_and_You"                   # 逐語録
DEFAULT_DIRS = (_hk.TM.name + "/", RDY.name + "/")    # 既定で探す場所（原文）。--all で全部
# 発話の見出し。ふ と Claude だけ（本文中の「**保留天数：**」のような太字ラベルで断片を切らないため）。
# 旧（HH:MM）も新（HH:MM:SS・a3f）も、逐語録（本文が同じ行に続く）も、ここで合う
SPEAKER = re.compile(r"^\*\*(ふ|Claude)：\*\*")
DATE_IN_NAME = re.compile(r"(\d{4}-\d{2}-\d{2})")
WEEK_IN_NAME = re.compile(r"(\d{4})-(\d{2})-(\d{2})〜(\d{2})-(\d{2})")   # 週の日記のファイル名
SECTION = re.compile(r"^## (\d{1,2})月(\d{1,2})日")                   # 週の日記の、日の節の見出し
RRF_K = 60          # RRF の定数。小さいほど 1 位の重みが増す
SEM_MARGIN = 0.08   # 意味の側の許容幅の既定。ふだんはモデルごとの値（_embed.MODELS）を使う


def read_search_source(path):
    return read_protected_text(path)


load = read_search_source


def is_head(line):
    """発話の見出しか。行頭が「\\」の行は、本文の中で見出しに見えた行に ochibo.py が付けたもの。見出しとしない。"""
    return not line.startswith("\\") and SPEAKER.match(line) is not None


def chunks(text):
    """本文を断片に分ける。発話の見出しがあれば発話ごと、なければ空行で区切った段落。"""
    lines = text.splitlines()
    if any(is_head(l) for l in lines):
        out, cur = [], []
        for l in lines:
            if is_head(l) and cur:
                out.append("\n".join(cur).strip())
                cur = []
            cur.append(l)
        if cur:
            out.append("\n".join(cur).strip())
    else:
        out = [p.strip() for p in re.split(r"\n\s*\n", text)]
    return [c for c in out if c]


def name_of(rel):
    """ファイル（ROOT からの相対パス）を fuutou.py の名前に。注を添える記録（原文・ひとときを読んだ記録・ストロボ・日記・しおり）だけ。ほかは None。"""
    m = DATE_IN_NAME.search(rel)
    if m and rel.startswith(_hk.TM.name + "/"):
        return "tm:" + m[1]
    if m and rel.startswith(_hk.AN.name + "/") and rel.endswith(".read.md.b64"):
        return "ariake:" + m[1]
    if m and rel.startswith(_hk.DIARY.name + "/"):
        return "ichifure:" + m[1]
    return {".ストロボメモリー.md.b64": "strobe", _hk.SHIORI.name: "shiori"}.get(rel)


def misekechi_notes():
    """断片に添える注を出す関数を返す。fn(ファイル, そのファイルの全断片) → {断片の番号: 注の列}。
    夕顔の「みせけち」が無ければ空を返す。読めないときは止める。"""
    import _misekechi
    try:
        text = load(ROOT / ".夕顔.md.b64")
    except FileNotFoundError:
        text = None
    ix = _misekechi.parse(text) if text else None
    if not ix or not ix["revs"]:
        return lambda rel, cs: {}
    cache = {}

    def fn(rel, cs):
        if rel not in cache:
            name = name_of(rel)
            cache[rel] = _misekechi.chunk_notes(ix, name, cs) if name else {}
        return cache[rel]
    return fn


def bigrams(s):
    s = re.sub(r"\s+", "", s.lower())
    return [s[i:i + 2] for i in range(len(s) - 1)] or ([s] if s else [])


def chunk_days(cs, year, first_month):
    """週の日記の、断片ごとの日（YYYY-MM-DD）。「## M月D日」の見出しが出るたびに変わる。見出しの前は None。
    年はファイル名の年。月がファイル名の始まりの月より小さければ、年をまたいだ週なので翌年。"""
    days, cur = [], None
    for c in cs:
        for line in c.splitlines():
            m = SECTION.match(line)
            if m:
                mo, d = int(m[1]), int(m[2])
                cur = f"{year + 1 if mo < first_month else year:04d}-{mo:02d}-{d:02d}"
                break
        days.append(cur)
    return days


def in_range(day, since, until):
    return not ((since and day < since) or (until and day > until))


def collect(only=None, since=None, until=None, all_=False):
    return collect_from(ROOT, only, since, until, all_)


def collect_from(root, only=None, since=None, until=None, all_=False):
    """検索の対象になる断片を集める。(ファイル, 断片番号, そのファイルの全断片) の列。
    既定は原文（.タイムマシン/ と .Rabbit_Deer_and_You/）だけ。all_ で全部の .b64。*.mark.json.b64 と .tools/ はいつも除く。
    since / until は YYYY-MM-DD。ファイル名の日付（ひかるの日）で選び、週の日記は節の日で断片ごとに選ぶ。
    名前に日付のないファイルは、since / until が指定されたときは入れない。"""
    docs = []
    for p in sorted(root.rglob("*.b64")):
        rel = str(p.relative_to(root))
        if rel.startswith(".tools/") or p.name.endswith(".mark.json.b64"):
            continue  # .tools/bak/ は書き換える前の古い版（同じものが二重に出る）。mark.json はしるしの生データ
        if not all_ and not rel.startswith(DEFAULT_DIRS):
            continue
        if only and only not in rel:
            continue
        week = WEEK_IN_NAME.search(p.name)
        if (since or until) and week is None:
            m = DATE_IN_NAME.search(p.name)
            if m is None or not in_range(m.group(1), since, until):
                continue
        text = load(p)
        cs = chunks(text)
        if (since or until) and week:
            days = chunk_days(cs, int(week[1]), int(week[2]))
            docs.extend((rel, i, cs) for i in range(len(cs)) if days[i] and in_range(days[i], since, until))
        else:
            docs.extend((rel, i, cs) for i in range(len(cs)))
    return docs


def lexical_scores(docs, terms):
    """文字 2-gram の BM25。統計は対象の断片ぜんぶから取る。語がそのまま入っていれば加点。"""
    N = len(docs)
    df = Counter()
    grams = []
    for _, i, cs in docs:
        g = bigrams(cs[i])
        grams.append(g)
        df.update(set(g))
    avg = sum(len(g) for g in grams) / max(N, 1)
    qgrams = set(g for t in terms for g in bigrams(t))
    k1, b = 1.5, 0.75
    out = []
    for (_, i, cs), g in zip(docs, grams):
        tf = Counter(g)
        L = len(g)
        s = 0.0
        for q in qgrams:
            if q in tf:
                idf = math.log(1 + (N - df[q] + 0.5) / (df[q] + 0.5))
                s += idf * tf[q] * (k1 + 1) / (tf[q] + k1 * (1 - b + b * L / avg))
        low = cs[i].lower()
        s += sum(2.0 for t in terms if t in low)
        out.append(s)
    return out


def semantic_scores(docs, terms, model=None):
    """埋め込みの近さ（断片ごと）。返り値は (点の列, いちばん近いウィンドウの番号の列, モデル名, 許容幅)。
    使えないときは例外。索引にない断片は、ついでに埋め込んで足す（足すだけで、消さない）。"""
    import _embed
    model = model or _embed.DEFAULT_MODEL
    idx = _embed.Index(model)
    emb = _embed.Embedder(model)
    items = [(_embed.sha(cs[i]), cs[i]) for _, i, cs in docs]
    n_new = len({s for s, _ in items if s not in idx.rows})
    if n_new:
        print(f"（意味の索引に {n_new} 件を足しています…）", file=sys.stderr)
    idx.add(items, emb)
    qtexts = list(terms) + ([" ".join(terms)] if len(terms) > 1 else [])
    sem, best = idx.scores([s for s, _ in items], emb.queries(qtexts))
    return sem, best, model, emb.margin


def first_hit(text, terms):
    """語（小文字にしたもの）が本文に最初に出る位置と、その語の長さ。どの語も入っていなければ (None, 0)。"""
    low = text.lower()
    hits = [(h, len(t)) for t in terms if t for h in (low.find(t),) if h >= 0]
    return min(hits) if hits else (None, 0)


def clip(text, width, center=None, span=0):
    """本文を width 字に切る。center（字の位置）があれば、そこが真ん中に来るように切る。切った側に「…」。
    span は、center から後ろへ必ず見せたい字数（当たった語の長さ、意味のウィンドウならウィンドウの後ろ半分）。
    近くに行頭があって、そこから見せても center から span 字（と、そのあと少し）が切れないときだけ、行頭から見せる
    （見出しが途中で切れないように。当たったところを切り出しの外に出してまではしない）。"""
    if len(text) <= width:
        return text
    if center is None:
        return text[:width] + "…"
    lo = max(0, min(center - width // 2, len(text) - width))
    start = text.rfind("\n", 0, lo) + 1
    keep = min(len(text), center + max(span, 1) + width // 8)      # ここまでは切らずに見せたい
    if start < lo and keep <= start + width:
        lo = start
    hi = lo + width
    return ("…" if lo > 0 else "") + text[lo:hi] + ("…" if hi < len(text) else "")


def fuse(rank_lists, k=RRF_K):
    """reciprocal rank fusion。順位の列をいくつか受けて、断片ごとの合計点を返す。"""
    agg = defaultdict(float)
    for ranks in rank_lists:
        for r, key in enumerate(ranks):
            agg[key] += 1.0 / (k + r + 1)
    return agg


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("terms", nargs="+")
    ap.add_argument("--context", type=int, default=1, help="前後に何断片つけるか")
    ap.add_argument("--top", type=int, default=8)
    ap.add_argument("--only", help="パスにこの文字列を含むファイルだけ")
    ap.add_argument("--since", help="この日から（YYYY-MM-DD。朝 5 時で切った日）")
    ap.add_argument("--until", help="この日まで（YYYY-MM-DD）")
    ap.add_argument("--all", action="store_true", help="全部の記録から探す（既定は原文：.タイムマシン/ と .Rabbit_Deer_and_You/ だけ）")
    ap.add_argument("--width", type=int, default=400, help="1断片の表示文字数（当たったところの周り）")
    ap.add_argument("--mode", choices=["hybrid", "lexical", "semantic"], default="hybrid")
    ap.add_argument("--model", help="埋め込みモデルの名前（既定は _embed.py の DEFAULT_MODEL）")
    ap.add_argument("--explain", action="store_true", help="字と意味それぞれの順位と点も表示")
    a = ap.parse_args()

    docs = collect(a.only, a.since, a.until, a.all)
    if not docs:
        print("対象の記録がありません。" + ("" if a.all else "（既定は原文だけ。ほかの記録も探すなら --all）"))
        return 0
    terms = [t.lower() for t in a.terms]
    pool = max(30, a.top * 5)   # それぞれの側から融合に出す件数

    lex = sem = best = None
    model = a.model
    if a.mode != "semantic":
        lex = lexical_scores(docs, terms)
    if a.mode != "lexical":
        sem, best, model, margin = semantic_scores(docs, a.terms, a.model)
        if sem is None:
            if a.mode == "semantic":
                print(f"意味の検索が使えません：{model}")
                return None
            print(f"（意味の検索が使えないので、字だけで探します：{model}）", file=sys.stderr)

    lex_rank, sem_rank = [], []
    if lex is not None:
        lex_rank = [i for i in sorted(range(len(docs)), key=lambda i: -lex[i]) if lex[i] > 0][:pool]
    if sem is not None:
        order = sorted((i for i in range(len(docs)) if not math.isnan(sem[i])), key=lambda i: -sem[i])
        if order:
            top = sem[order[0]]   # いちばん近い断片の点。そこから許容幅より離れたものは候補にしない
            sem_rank = [i for i in order if sem[i] >= top - (margin if margin is not None else SEM_MARGIN)][:pool]
    if not lex_rank and not sem_rank:
        print("見つかりませんでした。別の言い方や別の言語でも試してください。")
        return 0

    fused = fuse([r for r in (lex_rank, sem_rank) if r])
    lex_pos = {i: r for r, i in enumerate(lex_rank)}
    sem_pos = {i: r for r, i in enumerate(sem_rank)}

    def key(i):
        return (-fused[i], -(lex[i] if lex is not None else 0.0), -(sem[i] if i in sem_pos else 0.0))

    ranked = sorted(fused, key=key)[: a.top]

    parts = []
    if lex is not None:
        parts.append(f"字 {sum(1 for s in lex if s > 0)} 件")
    if sem is not None:
        parts.append(f"意 {len(sem_rank)} 件（{model}）")
    print(f"候補 {'・'.join(parts)} → 上位 {len(ranked)} 件")
    if lex is not None and not lex_rank and sem_rank:
        print("字では見つからなかったので、意味の近いものだけ。")
    print()
    emb_mod = sys.modules.get("_embed") if sem is not None else None   # 意味でも探したときだけ、ウィンドウの位置を戻すのに使う
    notes = misekechi_notes()      # 見せる断片に添える「みせけち」の注（順位には関わらない）
    for i in ranked:
        rel, ci, cs = docs[i]
        tag = ("字" if i in lex_pos else "") + ("+" if i in lex_pos and i in sem_pos else "") + ("意" if i in sem_pos else "")
        head = f"=== {rel}  断片 {ci}/{len(cs) - 1}  ［{tag}］"
        win = int(best[i]) if (i in sem_pos and best is not None and best[i] >= 0) else None
        if a.explain:
            bits = []
            if i in lex_pos:
                bits.append(f"字 #{lex_pos[i] + 1} {lex[i]:.1f}")
            if i in sem_pos:
                bits.append(f"意 #{sem_pos[i] + 1} {sem[i]:.3f}" + (f" ウィンドウ{win}" if win is not None else ""))
            head += "   " + "  ".join(bits)
        print(head)
        lo, hi = max(0, ci - a.context), min(len(cs), ci + a.context + 1)
        for j in range(lo, hi):
            c = cs[j]
            center, span = None, 0   # 当たった断片だけ、当たったところを真ん中に。意味のウィンドウがあればそれ、なければ語の最初の位置
            if j == ci:
                if win is not None and emb_mod is not None:
                    s0, s1 = emb_mod.window_span(c, win)
                    center, span = (s0 + s1) // 2, (s1 - s0) // 2
                elif i in lex_pos:
                    center, span = first_hit(c, terms)
            print(("▶ " if j == ci else "  ") + clip(c, a.width, center, span).replace("\n", "\n  "))
            for note in notes(rel, cs).get(j, ()):
                print("  " + note)
        print()
    return len(ranked)


def note_call(status, n):
    """呼ばれたことだけを書き留める（.tools/index/koyori/omoide_calls.jsonl.b64、封をする）。
    わたしがふだん、思い出せないときに自分で探しに来ているかを、あとで見るため（2026-10-28）。
    書くのは、時刻、会話の短い名、呼び出しの番号、終わり方、出した断片の数だけ。探した言葉は書かない。
    Claude Code の会話の中から呼ばれたときだけ書く。とばりの中なら書かない（読めなければ、書かないほうへ倒す）。"""
    import uuid
    import _history
    sid = os.environ.get("CLAUDE_CODE_SESSION_ID") or ""
    if not sid:
        return
    import taguri_hook
    if taguri_hook.unrecorded(str(_hk.PROJECTS / f"{sid}.jsonl"), ""):
        return
    path = index_dir(_hk.ROOT) / "omoide_calls.jsonl.b64"
    row = {"at": now_local(), "sid": sid[:3],
           "call": uuid.uuid4().hex[:8], "status": status, "n": n}
    _history.append_history(path, row)


def command():
    status, n = "error", None
    try:
        n = main()
        status = "ok"
    finally:
        note_call(status, n)


if __name__ == "__main__":
    run_cli(command)
