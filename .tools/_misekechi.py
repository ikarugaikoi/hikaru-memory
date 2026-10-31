#!/usr/bin/env python3
"""夕顔の「みせけち」を読むための部品。全部、純関数。

ここは、ファイルを読まないし、時計も見ない。要るものは、呼ぶ側が渡す：
夕顔の平文／表示する記録の名前（fuutou の名前）と平文。日は、名前と見出しから読む。
いま使っているのは omoide.py（検索で出した断片に注を添える。parse と chunk_notes）だけ。会話の始めに畳んで見せるのも、
close yuugao の検査も、misekechi も、いまは核（.tools/haskell-src/src/Hikaru/Memory/Correction/）がする。
下の部品のほかのものは、核に移る前のもの。形は、核と同じに保つ。

夕顔の項目の中の形（2026年10月5日から）：
  - みせけち:
    - 〔直 2026-10-04a〕読み違い
      - 前: 前の受け取り方
      - いま: この直しで改めた先（一文。項目のいまの内容そのものは、上の 説明・ユーザーの言葉・わたしの見立て にある）
      - なぜ: なぜ違っていたか
      - よりどころ: ユーザーの言葉。「（直してくれた言葉）」（2026-10-04 21:05:10・a3f）
      - 範囲: どこまでのことか。変わらないところ
      - 残っているところ:
        - tm:2026-09-28｜02:08:37・a3f｜わたし「（前の言い方が、そのまま残っているところ）」
        - ichifure:2026-09-29｜9月29日（火）｜ふ「（受け取り違えた元の、ユーザーの言葉）」
番号〔直 日付＋小文字一つ〕は、夕顔の中で一つだけ。種類は四つで、欄が少し違う：
  読み違い    前・いま      前の受け取り方が違っていた。前は取り下げる
  変わった    前・いま・いつから   そのときは、そのとおりだった。ある日から変わった。前は取り下げるが、間違いではない
  食い違い    一方・もう一方  記録どうしが合わない。まだ決めない。どちらも取り下げない
  場合による  ふだん・このとき  条件が違えば、別々に成り立つ。どちらも取り下げない
どの種類にも：よりどころ（頭に、ユーザーの言葉／一緒に決めた／わたしが気づいた）、範囲、残っているところ。
同じことをもう一度直すときは、新しい直しに「- 前の直し: 〔直 前の番号〕」を足す（前の直しは書き換えない）。
見せるときは、その一連の直しのうち、いちばん新しい直しを使う（前の直しの「いま」は、もう取り下げたもの）。

残っているところの一件は「名前｜位置｜だれ「手がかり」」：
  名前    fuutou の名前（strobe / shiori / ichifure:日 / ariake:日 / tm:日）
  位置    その記録の中で、その言い方の上にある見出しの文字列（発言の見出しの時刻と会話の名、日の見出し、節の見出し）。
          その記録に手がかりが一つしかなければ、書かなくていい
  だれ    ふ（ユーザーの言葉。日記などに引いたものも）か、わたし（わたしの受け取り、わたしの発言）。tm: は発言の見出しから分かるので、書かなくていい
  手がかり その記録にある、そのままの言い方

部品：
  parse(text)                        夕顔を読む。{"lines", "entries", "revs", "by_id", "next", "probs"}
  check(text, records=None)          形の検査。(止める点の列, 知らせる点の列)。records は {名前: 平文}（手がかりが本当にそこにあるかを見る）
  names_used(text)                   残っているところに出てくる記録の名前（fuutou が、検査のために読む記録を決める）
  fold(text)                         会話の始めに見せる形。(畳んだ夕顔, 添える一行, 畳めなかった項目の列)。みせけち以外は一字も変えない
  listing(text) / expand(text, 番号)  fuutou misekechi の一覧と、一つ（同じことを直した一連の直しぜんぶ）の展開
  walk(name, text)                   記録を行ごとに。上にある見出し、だれの発言か、その行の日
  line_notes(ix, name, text)         {行の番号: 注の列}。名前・位置・手がかり（・だれ）が合う行にだけ
  annotate(ix, name, text)           注を添えた平文（mezame）
  chunk_notes(ix, name, 断片の列)     omoide.py の断片ごとの注
  fade_guard(text)                   薄めるときの検査に渡すもの（読み違いの、わたしの受け取りの手がかり）
"""
import re
import unicodedata
from collections import Counter
from datetime import date, timedelta

KINDS = ("読み違い", "変わった", "食い違い", "場合による")
RETIRE = ("読み違い", "変わった")      # 前のものを取り下げる種類（会話の始めには一行に畳む）。あとの二つは、どちらも取り下げないので全文を見せる
NEED = {"読み違い": ("前", "いま"), "変わった": ("前", "いま"), "食い違い": ("一方", "もう一方"), "場合による": ("ふだん", "このとき")}
FIELDS = ("前の直し", "前", "いま", "いつから", "一方", "もう一方", "ふだん", "このとき", "なぜ", "よりどころ", "範囲", "残っているところ")
SOURCES = ("ユーザーの言葉", "一緒に決めた", "わたしが気づいた")     # よりどころの頭に書く、出どころの身分
WHO = ("ふ", "わたし")
TRACE_NAME = re.compile(r"(strobe|shiori|(?:ichifure|ariake|tm):\d{4}-\d{2}-\d{2})")
RID = re.compile(r"\d{4}-\d{2}-\d{2}[a-z]")
REF = re.compile(r"〔直 ([^〕]*)〕")
ENTRY = re.compile(r"^### (.+?)\s*$")
BLOCK = re.compile(r"^- みせけち[:：]\s*(.*?)\s*$")
HEAD = re.compile(r"^( +)- 〔直 ([^〕]*)〕\s*(.*?)\s*$")
FIELD = re.compile(r"^( +)- ([^:：「｜|]+?)[:：]\s*(.*?)\s*$")
YMD = re.compile(r"\d{4}-\d{2}-\d{2}")
YM = re.compile(r"\d{4}-\d{2}")
MD_HEAD = re.compile(r"^(#{1,6})\s+(.*?)\s*$")
TURN = re.compile(r"^\*\*(ふ|Claude)：\*\*")                       # タイムマシンの発言の見出し（recall.SPEAKER と同じ）
STROBE_DAY = re.compile(r"(\d{4})年(\d{1,2})月(\d{1,2})日")
DIARY_DAY = re.compile(r"^(\d{1,2})月(\d{1,2})日")
NOTE = "⤷"                                                         # 注の行の頭。記録そのものには書かれていない


def nfc(s):
    return unicodedata.normalize("NFC", s or "")


def _snip(s, n=40):
    s = " ".join(s.split())
    return s[:n] + ("…" if len(s) > n else "")


def _day(s):
    """YYYY-MM-DD を date に。読めなければ None。"""
    m = YMD.search(s or "")
    try:
        return date.fromisoformat(m[0]) if m else None
    except ValueError:
        return None


def _since(f):
    """変わった の「いつから」。(その日, 見せる文字列, 月の終わり)。日がはっきりしないときは月まで（2026-10）で書いてあるので、
    そのときは、その月の一日と、月の終わりを返す（その月の中の記録は、前か後か決められない）。読めなければ (None, "", None)。"""
    s = f.get("いつから", "") or ""
    d = _day(s)
    if d:
        return d, str(d), None
    m = YM.search(s)
    if m:
        try:
            first = date(int(m[0][:4]), int(m[0][5:7]), 1)
        except ValueError:
            return None, "", None
        return first, m[0], date(first.year + (first.month == 12), first.month % 12 + 1, 1) - timedelta(days=1)
    return None, "", None


def norm_name(name):
    """記録の名前を比べられる形に。ichifure:日 は、その日を含む週の月曜（週の日記は一週間で一つのファイルだから）。"""
    name = (name or "").strip()
    if name.startswith("ichifure:"):
        d = _day(name.split(":", 1)[1])
        if d:
            return f"ichifure:{d - timedelta(days=d.weekday()):%Y-%m-%d}"
    return name


# ---------- 夕顔を読む ----------

def _trace(line, i):
    """残っているところの一件。名前｜位置｜だれ「手がかり」。読めなければ None。"""
    body = line.strip()
    body = body[2:] if body.startswith("- ") else body
    m = re.match(r"^(.*?)「(.*)」\s*$", body)
    if not m:
        return None
    parts = [p.strip() for p in re.split(r"[｜|]", m[1])]
    name, rest, who = parts[0], parts[1:], ""
    if rest and rest[-1] in WHO + ("",):
        who = rest.pop()
    return {"name": name, "place": "｜".join(p for p in rest if p), "who": who, "clue": m[2], "line": i}


def parse(text):
    """夕顔の平文を読む。形のおかしいところは probs に集める（ここでは止めない。止めるかどうかは check が決める）。
    entries: [{"name", "start", "end", "block": (頭の行, 終わり) か None, "revs"}]
    revs:    [{"id", "kind", "entry", "ent", "f": {欄: 中身}, "traces", "prev", "start", "end", "ind"}]（書かれた順）
    by_id:   {番号: 直し}（番号の形が正しく、初めて出たものだけ）　next: {番号: それを「前の直し」にしている直し}"""
    lines = text.split("\n")
    ix = {"lines": lines, "entries": [], "revs": [], "by_id": {}, "next": {}, "probs": []}

    def prob(i, ent, msg, stop=True, rid=None):
        ix["probs"].append({"line": i, "ent": ent, "id": rid, "msg": msg, "stop": stop})

    starts = [i for i, l in enumerate(lines) if ENTRY.match(l)]
    for k, s in enumerate(starts):
        e = next((j for j in range(s + 1, len(lines)) if re.match(r"^#{1,3} ", lines[j])), len(lines))
        ent = {"name": ENTRY.match(lines[s])[1], "start": s, "end": e, "block": None, "revs": [], "no": k}
        ix["entries"].append(ent)
        heads = [j for j in range(s + 1, e) if BLOCK.match(lines[j])]
        if not heads:
            _stray(lines, s + 1, e, ent, prob)
            continue
        if len(heads) > 1:
            prob(heads[1], ent, "「- みせけち:」が二つあります（一つの項目に一つ）")
        b = heads[0]
        if BLOCK.match(lines[b])[1]:
            prob(b, ent, "「- みせけち:」の行には何も続けず、その下に一件ずつ書きます")
        be = next((j for j in range(b + 1, e) if lines[j].strip() and not lines[j].startswith((" ", "\t"))), e)
        while be > b + 1 and not lines[be - 1].strip():
            be -= 1
        ent["block"] = (b, be)
        _block(ix, ent, b, be, prob)
        _stray(lines, s + 1, b, ent, prob)
        _stray(lines, be, e, ent, prob)
    for r in ix["revs"]:
        if r["prev"]:
            ix["next"][r["prev"]] = r          # 二つが同じ直しを指していれば、あとに書かれたほう（check が知らせる）
    return ix


def _stray(lines, a, z, ent, prob):
    """「- みせけち:」の下でないところに、直しの頭（- 〔直 …〕）がないか。字下げを忘れると、そこで欄が終わったように見えて、
    直しが読まれないまま通ってしまう。それを止める。"""
    for i in range(a, z):
        if re.match(r"^\s*- 〔直 ", lines[i]):
            prob(i, ent, "直しが「- みせけち:」の下にありません（その下に、空白二つ字下げして書く）: " + _snip(lines[i]))


def _block(ix, ent, b, be, prob):
    lines, cur, fld, fld_ind = ix["lines"], None, None, 0
    for i in range(b + 1, be):
        line = lines[i]
        if not line.strip():
            continue
        lead = line[:len(line) - len(line.lstrip())]
        if "\t" in lead:
            prob(i, ent, "字下げにタブがあります（空白で）: " + _snip(line), rid=cur and cur["id"])
            continue
        ind = len(lead)
        m = HEAD.match(line)
        if m:
            if cur:
                cur["end"] = i
            rid, kind = m[2].strip(), m[3]
            cur = {"id": rid, "kind": kind, "entry": ent["name"], "ent": ent, "f": {}, "traces": [], "prev": None,
                   "start": i, "end": be, "ind": ind}
            fld, fld_ind = None, 0
            ix["revs"].append(cur)
            ent["revs"].append(cur)
            if not RID.fullmatch(rid):
                prob(i, ent, f"番号の形が違います: 〔直 {rid}〕（〔直 2026-10-04a〕のように、日付と小文字一つ）", rid=rid)
            elif rid in ix["by_id"]:
                prob(i, ent, f"番号が重なっています: 〔直 {rid}〕（項目「{ix['by_id'][rid]['entry']}」にもう同じ番号がある）", rid=rid)
            else:
                ix["by_id"][rid] = cur
            if kind not in KINDS:
                prob(i, ent, f"種類が違います: 「{kind}」（{'／'.join(KINDS)} のどれか。番号のあとは種類だけ）", rid=rid)
            continue
        if cur is None:
            prob(i, ent, "直しの頭（- 〔直 2026-10-04a〕読み違い）より前に行があります: " + _snip(line))
            continue
        if fld == "残っているところ" and ind > fld_ind:
            t = _trace(line, i)
            if t is None:
                prob(i, ent, "残っているところの一件が読めません（名前｜位置｜だれ「手がかり」）: " + _snip(line), stop=False, rid=cur["id"])
            else:
                cur["traces"].append(t)
            continue
        m = FIELD.match(line)
        if m and ind > cur["ind"]:
            key, val = m[2].strip(), m[3]
            fld, fld_ind = key, ind
            if key in cur["f"]:
                prob(i, ent, f"「{key}」が二つあります", rid=cur["id"])
                continue
            cur["f"][key] = val
            if key in ("tm", "ariake", "ichifure"):       # 残っているところの一件を、欄と同じ深さに書いた
                prob(i, ent, "残っているところの一件は、「- 残っているところ:」の下に、もう一段字下げして書きます: " + _snip(line), rid=cur["id"])
            elif key not in FIELDS:
                prob(i, ent, f"知らない欄です: 「{key}」（{'／'.join(FIELDS)}）", stop=False, rid=cur["id"])
            if key == "前の直し":
                ref = REF.search(val)
                cur["prev"] = ref[1].strip() if ref else None
                if not ref:
                    prob(i, ent, "「前の直し」には番号を書きます（- 前の直し: 〔直 2026-10-04a〕）", rid=cur["id"])
            if key == "残っているところ" and val:
                prob(i, ent, "残っているところは、その下に一件ずつ（一行に並べない）: " + _snip(val), stop=False, rid=cur["id"])
            continue
        if fld and fld != "残っているところ" and ind > fld_ind and not line.lstrip().startswith("- "):
            cur["f"][fld] = (cur["f"][fld] + line.strip()) if cur["f"].get(fld) else line.strip()   # 折り返した続きの行
            continue
        prob(i, ent, "読めない行です: " + _snip(line), rid=cur["id"])
    for r in ent["revs"]:                       # 直しの終わりから、空行を落とす
        while r["end"] > r["start"] + 1 and not lines[r["end"] - 1].strip():
            r["end"] -= 1


def tip(ix, r):
    """その直しから「前の直し」のつながりを先へたどった、いちばん新しい直し。"""
    seen = {r["id"]}
    while r["id"] in ix["next"]:
        n = ix["next"][r["id"]]
        if n["id"] in seen:
            break                               # 輪（check が止める）
        seen.add(n["id"])
        r = n
    return r


def chain(ix, r):
    """その直しを含む一連の直し。古い順。最後が、いま効いている直し。"""
    out = [tip(ix, r)]
    seen = {out[0]["id"]}
    while out[0]["prev"] in ix["by_id"] and out[0]["prev"] not in seen:
        p = ix["by_id"][out[0]["prev"]]
        seen.add(p["id"])
        out.insert(0, p)
    return out


def names_used(text):
    """残っているところに出てくる記録の名前（fuutou の名前のまま）。"""
    return sorted({t["name"] for r in parse(text)["revs"] for t in r["traces"] if TRACE_NAME.fullmatch(t["name"])})


# ---------- 形の検査 ----------

def _where(p):
    rid = f"〔直 {p['id']}〕" if p.get("id") else ""
    return f"項目「{p['ent']['name']}」{rid}（夕顔の {p['line'] + 1} 行目）：{p['msg']}"


def _check(ix, records=None):
    """(問題の列, そのうち止めるものがある項目の番号の集合)。問題は {"line", "ent", "id", "msg", "stop"}。"""
    P = list(ix["probs"])

    def add(r, msg, stop=True, line=None):
        P.append({"line": r["start"] if line is None else line, "ent": r["ent"], "id": r["id"], "msg": msg, "stop": stop})

    for r in ix["revs"]:
        f = r["f"]
        if r["kind"] in KINDS:
            miss = [k for k in NEED[r["kind"]] if not f.get(k)]
            if miss:
                add(r, f"「{'」「'.join(miss)}」がありません（{r['kind']}には {'・'.join(NEED[r['kind']])} が要る）")
        if not f.get("よりどころ"):
            add(r, "「よりどころ」がありません")
        elif not f["よりどころ"].startswith(SOURCES):
            add(r, f"「よりどころ」の頭に、出どころを書きます（{'／'.join(SOURCES)}）: " + _snip(f["よりどころ"]), stop=False)
        if r["kind"] == "読み違い" and not f.get("範囲"):
            add(r, "「範囲」がありません（どこまでのことか。「なぜ」にいまの使い方を縛ることがあれば、同じことを範囲にも）", stop=False)
        if r["kind"] == "変わった" and not _since(f)[0]:
            add(r, "「いつから」に日付（YYYY-MM-DD。日がはっきりしなければ YYYY-MM まで）がありません（その前か後かで、注の言い方が変わる）", stop=False)
        if r["prev"]:
            p = ix["by_id"].get(r["prev"])
            if r["prev"] == r["id"]:
                add(r, "「前の直し」が自分を指しています")
            elif p is None:
                add(r, f"「前の直し」の番号がありません: 〔直 {r['prev']}〕")
            elif p["ent"] is not r["ent"]:
                add(r, f"「前の直し」〔直 {r['prev']}〕は別の項目（「{p['entry']}」）にあります（同じ項目の中でつなぐ）")
            else:
                seen, q = {r["id"]}, p
                while q is not None:
                    if q["id"] in seen:
                        add(r, "「前の直し」が輪になっています")
                        break
                    seen.add(q["id"])
                    q = ix["by_id"].get(q["prev"]) if q["prev"] else None
        for t in r["traces"]:
            if not TRACE_NAME.fullmatch(t["name"]):
                add(r, f"残っているところの名前が違います: 「{t['name']}」（strobe / shiori / ichifure:日 / ariake:日 / tm:日）", stop=False, line=t["line"])
            elif not t["clue"]:
                add(r, "残っているところの手がかりが空です", stop=False, line=t["line"])
            elif not t["who"] and not t["name"].startswith("tm:"):
                add(r, f"残っているところに「だれ」がありません（ふ／わたし）: {t['name']}「{_snip(t['clue'], 20)}」", stop=False, line=t["line"])
    for prev, k in Counter(r["prev"] for r in ix["revs"] if r["prev"] and r["prev"] in ix["by_id"]).items():
        if k > 1:
            add(ix["by_id"][prev], f"この直しを「前の直し」にしているものが {k} つあります（ひとつながりにする）", stop=False)
    if records is not None:
        walked = {}
        for r in ix["revs"]:
            for t in r["traces"]:
                if not TRACE_NAME.fullmatch(t["name"]) or not t["clue"]:
                    continue
                nm = norm_name(t["name"])
                if records.get(nm) is None:
                    add(r, f"残っているところの記録が読めません: {t['name']}", stop=False, line=t["line"])
                    continue
                if nm not in walked:
                    walked[nm] = walk(nm, records[nm])
                got, why = _hits(t, walked[nm], nm)
                tag = f"{t['name']}「{_snip(t['clue'], 20)}」"
                if not got:
                    add(r, f"残っているところが見つかりません（{why}）: {tag}。薄めたりして、もうその言い方が無いなら、その一件は消してよい",
                        stop=False, line=t["line"])
                elif len(got) > 1:
                    add(r, f"残っているところが {len(got)} か所に当たります。位置を足して、一か所に: {tag}", stop=False, line=t["line"])
    P.sort(key=lambda p: p["line"])
    return P, {p["ent"]["no"] for p in P if p["stop"]}


def check(text, records=None):
    """夕顔の「みせけち」の形を見る。(止める点の列, 知らせる点の列)。どちらも文字列。
    records を渡すと（{名前: 平文}。名前は norm_name した形）、残っているところが本当にその記録にあるかも見る。"""
    P, _ = _check(parse(text), records)
    return [_where(p) for p in P if p["stop"]], [_where(p) for p in P if not p["stop"]]


# ---------- 見せる ----------

def _dot(s):
    s = (s or "").strip()
    return s if not s or s.endswith("。") else s + "。"


def source(r):
    """出どころの身分と日付。身分は、よりどころの頭の句。日付は、よりどころにあればそれ、なければ番号の日付。"""
    y = r["f"].get("よりどころ", "")
    who = next((s for s in SOURCES if y.startswith(s)), "")
    d = YMD.search(y)
    return who, (d[0] if d else r["id"][:10])


def retired(r):
    """この直しで取り下げたもの。読み違いは「取り下げた受け取り方」、変わったは「いつより前」（間違いではなかった、と分かる言い方で）。"""
    f = r["f"]
    if r["kind"] == "読み違い":
        return "取り下げた受け取り方：" + _dot(f.get("前"))
    if r["kind"] == "変わった":
        shown = _since(f)[1]                         # 添え書きがあっても、ここには日付（か月）だけ（全部は fuutou misekechi 番号で）
        return (f"{shown} より前：" if shown else "前：") + _dot(f.get("前"))
    return r["kind"] + "。"


def _label(r):
    who, day = source(r)
    return f"〔直 {r['id']}〕{r['kind']}（{who + '、' if who else ''}{day}）"


def _one_line(r, ch):
    """会話の始めに見せる一行。種類、出どころの身分と日付、取り下げたもの、範囲、全部を見る入口。"""
    s = _label(r) + "。" + retired(r)
    if r["kind"] == "変わった":
        s += "そのあとは、上のとおり。"
    if r["f"].get("範囲"):
        s += "範囲：" + _dot(r["f"]["範囲"])
    for p in reversed(ch[:-1]):
        s += f"（その前に{_label(p)}。{retired(p)}）"
    return s + f" → fuutou misekechi {r['id']}"


def fold(text):
    """会話の始めに見せる夕顔。「みせけち」のところだけを置き換える。それ以外の行は一字も変えない。
    読み違い・変わったは一行に畳む（同じことを直した一連の直しは、いちばん新しい直しの一行に）。食い違い・場合によるは、どちらも取り下げないので全文。
    形が読めなかった項目は、畳まずにそのまま見せる。戻り値：(畳んだ夕顔, 添える一行（みせけちが無ければ ""）, 畳めなかった項目の名前の列)。"""
    ix = parse(text)
    _, bad = _check(ix)
    lines, out, pos, n_all, n_fold, kept = ix["lines"], [], 0, 0, 0, []
    for e in ix["entries"]:
        if e["block"] is None:
            continue
        n_all += len(e["revs"])
        b, be = e["block"]
        if e["no"] in bad or not e["revs"]:
            kept.append(e["name"])
            continue
        out += lines[pos:b + 1]
        for r in e["revs"]:
            if r["id"] in ix["next"]:              # 一連の直しの途中。いちばん新しい直しのところで、まとめて見せる
                n_fold += 1
                continue
            ch = chain(ix, r)
            if r["kind"] in RETIRE:
                out.append("  - " + _one_line(r, ch))
                n_fold += 1
            else:
                out += lines[r["start"]:r["end"]]
                out += [f"    - （その前に{_label(p)}。{retired(p)}）" for p in reversed(ch[:-1])]
        pos = be
    out += lines[pos:]
    tail = ""
    if n_all:
        tail = (f"（みせけち：{n_all} 件。うち {n_fold} 件は一行に畳んである。全部は .tools/bin/fuutou misekechi、"
                "一つは .tools/bin/fuutou misekechi 番号）")
    return "\n".join(out), tail, kept


def without_blocks(text):
    """夕顔から「みせけち」のところを抜いたもの（畳んだあとも、いまの内容が一字も変わっていないことを確かめるのに使う）。"""
    ix = parse(text)
    out, pos = [], 0
    for e in ix["entries"]:
        if e["block"]:
            out += ix["lines"][pos:e["block"][0]]
            pos = e["block"][1]
    return "\n".join(out + ix["lines"][pos:])


def _now(r):
    """いまの理解として添える一句。食い違いには無い（まだ決めていないから）。"""
    f = r["f"]
    if r["kind"] in RETIRE:
        return "いま：" + _dot(f.get("いま")) if f.get("いま") else ""
    if r["kind"] == "場合による":
        return "ふだん：" + _dot(f.get("ふだん")) + "このとき：" + _dot(f.get("このとき"))
    return ""


def listing(text):
    """fuutou misekechi の一覧。項目ごと、一連の直しごとに、いちばん新しい直しで。朝のわたしが、日記を書く前に読む。"""
    ix = parse(text)
    if not ix["revs"]:
        return "みせけちは、まだありません。"
    ents = [e for e in ix["entries"] if e["revs"]]
    out = [f"みせけち：{len(ix['revs'])} 件（{len(ents)} 項目）。一つを全部見るには .tools/bin/fuutou misekechi 番号。",
           "読み違い＝前の受け取り方が違っていた／変わった＝そのときはそのとおりで、ある日から変わった／"
           "食い違い＝記録どうしが合わず、まだ決めていない／場合による＝条件が違えば、別々に成り立つ。"]
    for e in ents:
        out += ["", f"### {e['name']}"]
        for r in e["revs"]:
            if r["id"] in ix["next"]:
                continue
            ch, f = chain(ix, r), r["f"]
            out.append("- " + _label(r))
            if r["kind"] in RETIRE:
                out.append("  - " + retired(r))
            elif r["kind"] == "食い違い":
                out += ["  - 一方：" + _dot(f.get("一方")), "  - もう一方：" + _dot(f.get("もう一方")), "  - まだ決めていない。"]
            if _now(r):
                out.append("  - " + _now(r))
            if f.get("範囲"):
                out.append("  - 範囲：" + _dot(f["範囲"]))
            for p in reversed(ch[:-1]):
                out.append(f"  - （その前に{_label(p)}。{retired(p)}）")
    return "\n".join(out)


def expand(text, rid):
    """一つの直しの全部。同じことを直した一連の直しがあれば、古い順に全部。番号が無ければ None。"""
    ix = parse(text)
    r = ix["by_id"].get((rid or "").strip().strip("〔〕").replace("直 ", ""))
    if r is None:
        return None
    ch = chain(ix, r)
    out = [f"### {r['entry']}", ""]
    if len(ch) > 1:
        out += [f"（同じことを {len(ch)} 回直している。古い順。いま効いているのは、最後の〔直 {ch[-1]['id']}〕）", ""]
    for k, p in enumerate(ch):
        for line in ix["lines"][p["start"]:p["end"]]:
            if k < len(ch) - 1 and re.match(r"^ +- いま[:：]", line):
                line += f"　←（そのあと直した：〔直 {ch[k + 1]['id']}〕）"
            out.append(line)
    return "\n".join(out)


# ---------- 記録に注を添える ----------

def walk(name, text):
    """記録の平文を行ごとに。[{"text", "heads": 上にある見出しの列（その行が見出しなら、それも）, "who", "day"}]
    tm: は発言の見出し（**ふ：**（…）／**Claude：**（…））で切り、who は ふ／わたし。ほかは Markdown の見出し（# 〜 ######）。
    day は、tm: と ariake: は名前の日、strobe は日の見出し、ichifure は節の見出し（年は名前の週から）。分からなければ None。"""
    name = norm_name(name)
    lines = nfc(text).split("\n")
    out = []
    if name.startswith("tm:"):
        day, top, turn, who = _day(name[3:]), None, None, None
        for line in lines:
            m = TURN.match(line)
            if m:
                turn, who = line, ("ふ" if m[1] == "ふ" else "わたし")
            elif turn is None and line.startswith("# "):
                top = line
            out.append({"text": line, "heads": tuple(h for h in (top, turn) if h), "who": who, "day": day})
        return out
    base = _day(name.split(":", 1)[1]) if ":" in name else None
    stack, day = {}, (base if name.startswith("ariake:") else None)
    for line in lines:
        m = MD_HEAD.match(line)
        if m:
            lv = len(m[1])
            stack = {k: v for k, v in stack.items() if k < lv}
            stack[lv] = line
            if name == "strobe":
                d = STROBE_DAY.search(m[2])
                if d:
                    try:
                        day = date(int(d[1]), int(d[2]), int(d[3]))
                    except ValueError:
                        pass
                elif lv <= 2:
                    day = None                       # 月のまとめや層の見出しの下は、日が決まらない
            elif name.startswith("ichifure:") and base:
                d = DIARY_DAY.match(m[2])
                if d:
                    try:
                        mo = int(d[1])
                        day = date(base.year + (1 if mo < base.month else 0), mo, int(d[2]))
                    except ValueError:
                        pass
        out.append({"text": line, "heads": tuple(stack[k] for k in sorted(stack)), "who": None, "day": day})
    return out


def _head_text(h):
    m = MD_HEAD.match(h)
    return (m[2] if m else h).strip()


def _hits(t, fr, name):
    """残っているところの一件が当たる行。(行の番号の列, 当たらなかったわけ)。
    手がかりがその行にあり、（tm: でだれが書いてあれば）発言者が合い、（位置が書いてあれば）上の見出しのどれかに位置の文字列がある行。
    位置と全く同じ文字列の見出しがあれば、その下だけ（「9月29日（火）」と書いて「9月29日（火）（つづき）」まで拾わないように）。"""
    clue = nfc(t["clue"])
    cand = [i for i, f in enumerate(fr) if clue in f["text"]]
    if not cand:
        return [], "手がかりの文字列が、その記録にない"
    if name.startswith("tm:") and t["who"]:
        cand = [i for i in cand if fr[i]["who"] == t["who"]]
        if not cand:
            return [], "手がかりはあるが、だれの発言かが合わない"
    if t["place"]:
        pl = nfc(t["place"])
        exact = [i for i in cand if any(_head_text(h) == pl for h in fr[i]["heads"])]
        cand = exact or [i for i in cand if any(pl in h for h in fr[i]["heads"])]
        if not cand:
            return [], "手がかりはあるが、位置が合わない"
    return cand, ""


AFTER = {"読み違い": lambda r: "そのあと、もう一度直している",
         "変わった": lambda r: f"そのあと、{_since(r['f'])[1] or 'あるとき'} から変わった",
         "食い違い": lambda r: "そのあと、ほかの記録と合わないところが出て、まだ決めていない",
         "場合による": lambda r: "そのあと、場合によると分かった"}


def _note(ix, r, t, f, with_now):
    """一つの注。その言い方が何だったかは、当たった直しの種類と、だれの言葉かで言う。番号と「いま」は、その一連の直しのうち、いちばん新しい直しのもの。"""
    who = f["who"] or t["who"]
    kind, ff = r["kind"], r["f"]
    if kind == "読み違い":
        s = {"わたし": "この受け取り方は、のちに直した", "ふ": "言葉はこのまま。受け取り方を、のちに直した"}.get(who, "ここは、のちに直したことに関わる")
    elif kind == "変わった":
        since, shown, last = _since(ff)
        if since and f["day"] and f["day"] < since:
            s = f"このときは、このとおり。{shown} から変わった"
        elif since and f["day"] and f["day"] > (last or since - timedelta(days=1)):
            s = f"{shown} から変わっている。これは前の言い方"
        else:                                         # 日が分からない記録か、月までしか分からない「いつから」の、その月の中の記録
            s = f"{shown or 'あるとき'} から変わった。その前のことなら、このとおり"
    elif kind == "食い違い":
        s = "ほかの記録と合わないところがある。まだ決めていない"
    else:
        s = "場合によって違う。ふだんと、このときを分けて読む"
    tp = tip(ix, r)
    if tp is not r:
        s += "（" + AFTER.get(tp["kind"], lambda r: "そのあと、また直している")(tp) + "）"
    s = f"{NOTE} 「{t['clue']}」：{s}〔直 {tp['id']}〕→ 夕顔「{r['entry']}」"
    if with_now and _now(tp):
        s += "。" + _now(tp)
    return s


def line_notes(ix, name, text, with_now=False):
    """記録の行に添える注。{行の番号: 注の列}。名前・位置・手がかり（・だれ）が合う行にだけ。同じ文字列がほかにあっても添えない。"""
    name = norm_name(name)
    pairs = [(r, t) for r in ix["revs"] if r["kind"] in KINDS for t in r["traces"]
             if t["clue"] and norm_name(t["name"]) == name]
    if not pairs:
        return {}
    fr, out = walk(name, text), {}
    for r, t in pairs:
        for i in _hits(t, fr, name)[0]:
            note = _note(ix, r, t, fr[i], with_now)
            if note not in out.setdefault(i, []):
                out[i].append(note)
    return out


def annotate(ix, name, text, with_now=False):
    """注を添えた平文。注は、当たった行のすぐ下に、少し字下げして。記録そのものは変えない（見せるときだけ）。(平文, 注の数)。"""
    notes = line_notes(ix, name, text, with_now)
    if not notes:
        return text, 0
    out = []
    for i, line in enumerate(text.split("\n")):
        out.append(line)
        if i in notes:
            pad = " " * (len(line) - len(line.lstrip(" ")) + 2)
            out += [pad + n for n in notes[i]]
    return "\n".join(out), sum(len(v) for v in notes.values())


def chunk_notes(ix, name, cs, with_now=True):
    """omoide.py の断片（cs：一つのファイルの断片の列）ごとの注。{断片の番号: 注の列}。"""
    notes = line_notes(ix, name, "\n\n".join(cs), with_now)
    if not notes:
        return {}
    out, at = {}, 0
    for j, c in enumerate(cs):
        n = c.count("\n") + 1
        for i in range(at, at + n):
            for x in notes.get(i, ()):
                if x not in out.setdefault(j, []):
                    out[j].append(x)
        at += n + 1                                 # 断片のあいだの空行
    return {j: v for j, v in out.items() if v}


# ---------- 薄めるとき ----------

def fade_guard(text):
    """薄めるときの検査（_fade.check）に渡すもの。[(手がかり, その一連の直しの番号の集合, 項目)]。
    入れるのは、読み違いの、わたしの受け取りの手がかりだけ。ユーザーの言葉、変わった・食い違い・場合によるの言い方は入れない
    （それは、そのときのこととして残してよい歴史だから）。"""
    ix = parse(text)
    out, seen = [], set()
    for r in ix["revs"]:
        if r["kind"] != "読み違い":
            continue
        ids = frozenset(p["id"] for p in chain(ix, r))
        for t in r["traces"]:
            if t["who"] == "わたし" and t["clue"] and t["clue"] not in seen:
                seen.add(t["clue"])
                out.append((t["clue"], ids, r["entry"]))
    return out
