#!/usr/bin/env python3
"""Find overlapping rules across instruction files before adding another rule."""
import ast
import pathlib
import re
import sys

sys.dont_write_bytecode = True  # ひかるの中に __pycache__ を作らない

from _hk import ROOT, work_dir
from _tool_data import read_protected_text as read_sealed, run_cli

SKILL = ROOT / ".local" / "daily-review.md"
JP = re.compile(r"[぀-ヿ一-鿿]")
PER_PLACE = 4          # 探すとき、同じ場所から並べる数の上限
STRONG = 0.80          # これ以上は、ほぼ同じことを言っている


LABEL = re.compile(r"^([^\s:：。、「」（）()`]{1,12})[:：]\s*")      # 行の頭の「説明: 」「とばり：」のような見出しの語


def sents(text):
    """文に分ける。行の頭の記号を落とし、「。」で切る。短すぎる切れ端と、かながほとんど無い行（命令だけの行）は捨てる。
    行の頭に「説明: 」のような見出しの語があれば、その行のどの文にも付ける（切ったあとの文が、何についての決まりかを失わないように）。"""
    out = []
    for line in text.split("\n"):
        line = re.sub(r"^[\s\-・*#>]+", "", line).strip()
        m = LABEL.match(line)
        label, body = (m.group(1) + "：", line[m.end():]) if m else ("", line)
        for s in re.split(r"(?<=[。！？])", body):
            s = s.strip()
            if len(JP.findall(s)) >= 8 and len(s) >= 10:
                out.append(label + s)
    return out


def head(text, mark):
    """記録の冒頭（mark の前まで）。mark が無ければ、何も返さない（項目のほうまで巻きこまないように）。"""
    i = text.find(mark)
    return text[:i] if i >= 0 else ""


def places():
    """[(場所の種類, 場所の名, 文)]。種類は C A O Y S H T の一字（同じ種類どうしは、--all では比べない）。"""
    items = []

    def add(kind, where, text):
        seen = set()
        for s in sents(text):
            if s not in seen:
                seen.add(s)
                items.append((kind, where, s))

    add("C", "CLAUDE.md", (ROOT / "CLAUDE.md").read_text(encoding="utf-8"))
    try:
        a = SKILL.read_text(encoding="utf-8")
    except FileNotFoundError:
        pass
    else:
        add("A", "朝の手順書", a.split("---", 2)[2] if a.startswith("---") else a)
    add("O", "丘の上", read_sealed(ROOT / ".丘の上.md.b64"))
    add("Y", "夕顔の冒頭", head(read_sealed(ROOT / ".夕顔.md.b64"), "\n## "))
    add("S", "ストロボの冒頭", head(read_sealed(ROOT / ".ストロボメモリー.md.b64"), "\n## "))
    shiori = ROOT / ".しおり.md.b64"
    try:
        text = read_sealed(shiori)
    except FileNotFoundError:
        pass
    else:
        add("H", "しおりの冒頭", head(text, "\n## "))
    me = pathlib.Path(__file__).name
    for p in sorted((ROOT / ".tools").glob("*.py")):
        if p.name == me:
            continue
        tree = ast.parse(p.read_text(encoding="utf-8"), filename=str(p))
        text = "\n".join(n.value for n in ast.walk(tree)
                         if isinstance(n, ast.Constant) and isinstance(n.value, str) and len(JP.findall(n.value)) >= 8)
        add("T", p.name, text)
    from _haskell_rules import rules
    native = {}
    for where, text in rules(ROOT):
        native.setdefault(where, []).append(text)
    for where, texts in native.items():
        add("T", where, "\n".join(texts))
    return items


def grams(s):
    s = re.sub(r"\s+", "", s)
    return {s[i:i + 2] for i in range(len(s) - 1)}


def overlap(a, b):
    return len(a & b) / max(1, min(len(a), len(b)))


def embed(texts):
    """埋め込み（正規化した行列）。モデルが使えなければ例外。
    探す文も、記録の文と同じ側（passages）で埋め込む。問いと答えではなく、文と文の近さを見たいので（問いの側で埋め込むと、近さが低く出る）。"""
    import numpy as np
    import _embed
    v = _embed.Embedder().passages(texts)
    return v / np.linalg.norm(v, axis=1, keepdims=True)


def search(queries, top):
    items = places()
    V = embed([s for _, _, s in items])
    queries = [q for x in queries for q in (sents(x) or [x])]      # 長い文は、こちらも文に分けて、それぞれで探す
    Q = embed(queries) if V is not None else None
    # 文だけで比べるのと、場所の名を頭に付けて比べるのと、両方で見て、近いほうを取る。
    # 「夕顔の説明は…」のように探す文に記録の名が入っていると、記録の冒頭の文（「説明：…」。名は入っていない）が遠く出るので。
    W = embed([f"{where}：{s}" for _, where, s in items]) if Q is not None else None
    qg = [grams(q) for q in queries]
    scored = []
    for i, (kind, where, s) in enumerate(items):
        g = grams(s)
        ov = max(overlap(g, x) for x in qg)
        cos = max(float((V[i] @ Q.T).max()), float((W[i] @ Q.T).max()) if W is not None else 0.0) if Q is not None else None
        same = cos is not None and (cos >= STRONG or (cos >= 0.72 and ov >= 0.55))     # --all で束にするのと同じ線
        rank = (cos + 0.25 * ov) if cos is not None else ov                           # 並べる順は、意味に字を少し足す
        scored.append((rank, cos, ov, same, where, s))
    scored.sort(key=lambda r: -r[0])
    shown, per = [], {}
    for r in scored:
        where = r[4]
        if per.get(where, 0) >= PER_PLACE:
            continue
        per[where] = per.get(where, 0) + 1
        shown.append(r)
        if len(shown) >= top:
            break
    print(f"探した文：{len(items)}（{len({w for _, w, _ in items})} か所）。近い順に {len(shown)} 件。")
    if Q is not None:
        hits = [r for r in scored if r[3]]
        if hits:
            print(f"◎ ほぼ同じことを言っている文：{len(hits)} 件（{'、'.join(sorted({r[4] for r in hits}))}）。新しく書かずに、そこを直す。")
        else:
            print("◎ のつく文（ほぼ同じことを言っている文）は、見つかりませんでした。下の近い文を読んで、同じ決まりでないか確かめる（言い方を変えて、もう一度探してもよい）。")
    for rank, cos, ov, same, where, s in shown:
        c = f"意 {cos:.2f}" if cos is not None else "意 ----"
        print(f"{'◎' if same else '　'} {c}  字 {ov:.2f}  ［{where}］ {s}")


def all_(cos):
    import numpy as np
    items = places()
    V = embed([s for _, _, s in items])
    if V is None:
        sys.exit("--all は、意味で比べられるときだけ使えます。")
    S = V @ V.T
    kinds = [k for k, _, _ in items]
    G = [grams(s) for _, _, s in items]
    parent = list(range(len(items)))

    def find(x):
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    pairs = 0
    for i in range(len(items)):
        for j in np.nonzero(S[i, i + 1:] >= 0.72)[0] + i + 1:
            j = int(j)
            if kinds[i] == kinds[j]:
                continue
            if S[i, j] >= cos or overlap(G[i], G[j]) >= 0.55:
                pairs += 1
                parent[find(i)] = find(j)
    groups = {}
    for i in range(len(items)):
        groups.setdefault(find(i), []).append(i)
    gl = sorted((g for g in groups.values() if len(g) > 1), key=lambda g: (-len({kinds[i] for i in g}), -len(g)))
    out = [f"# 違う場所にある、似た文の束（意味の近さ {cos} 以上、または 0.72 以上で字が半分以上重なる）\n",
           f"文：{len(items)}　近い対：{pairs}　束：{len(gl)}　三つ以上の場所にまたがる束：{sum(1 for g in gl if len({kinds[i] for i in g}) >= 3)}\n",
           "束の数は、決まりの数ではない。同じ決まりかどうかは、読んで決める。\n"]
    for n, g in enumerate(gl, 1):
        out.append(f"\n## 束{n}（{len(g)} 文）\n")
        for i in sorted(g, key=lambda i: (kinds[i], items[i][1])):
            out.append(f"- ［{items[i][1]}］ {items[i][2]}\n")
    path = work_dir() / "tomonari.md"
    path.write_text("".join(out), encoding="utf-8")
    print(f"文：{len(items)}　近い対：{pairs}　束：{len(gl)}　三つ以上の場所にまたがる束：{sum(1 for g in gl if len({kinds[i] for i in g}) >= 3)}")
    print(path)


def main():
    a = sys.argv[1:]

    def opt(name, default, cast):
        if name in a:
            i = a.index(name)
            v = cast(a[i + 1])
            del a[i:i + 2]
            return v
        return default

    top, cos = opt("--top", 10, int), opt("--cos", STRONG, float)
    if "--all" in a:
        all_(cos)
    elif a:
        search(a, top)
    else:
        sys.exit(__doc__)


if __name__ == "__main__":
    run_cli(main)
