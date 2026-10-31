#!/usr/bin/env python3
"""Candidate retrieval, entity timelines and structured footprint queries."""
import argparse
import json
import math
import re
import sys
import time
from datetime import datetime, timedelta
from pathlib import Path

sys.dont_write_bytecode = True  # ひかるの中に __pycache__ を作らない
sys.path.insert(0, str(Path(__file__).resolve().parent))
import _hk  # noqa: E402
import _embed  # noqa: E402
import _koyori  # noqa: E402
import omoide  # noqa: E402
from _tool_data import parse_json, read_protected_json, read_protected_text, run_cli

VERSION = "taguri-v1"
HALF_LIFE_DAYS = 30.0
MIN_QUERY = 4          # 句読点と空白を除いて、これより短い発話は探さない
SEM_TIMEOUT = 1.5      # 常駐に問いを埋め込んでもらうのを待つ上限（秒）
T_SEM, SEM_OFFER, SEM_CEIL = 0.60, 0.70, 0.85   # 意味の側：門、届ける線、天井（bge-m3 のコサイン）
T_LEX, LEX_OFFER, LEX_CEIL = 2.0, 3.5, 6.0      # 字の側：門、届ける線（字だけのとき）、天井（2-gram あたりの BM25）
KINDS = ("utterance", "judgment", "footprint", "strobe")
SNIP = 120             # 届ける一行の長さ
PUNCT = re.compile(r"[\s　、。，．,.!！?？…~〜「」『』（）()【】\[\]・:：;；\"'“”‘’ー-]+")


def thresholds():
    return {"version": VERSION, "half_life_days": HALF_LIFE_DAYS, "sem": T_SEM, "sem_offer": SEM_OFFER,
            "lex": T_LEX, "lex_offer": LEX_OFFER}


# ---------- 中身 ----------

def lexical_texts(entries):
    """字の側で見る中身。sha → 文。原文の断片とストロボの項目と足跡（どれも封を開けるだけで速い）。判は入れない。"""
    out = {}
    for _, i, cs in omoide.collect():
        out[_embed.sha(cs[i])] = cs[i]
    if any(e["kind"] == "strobe" for e in entries):
        out.update((_embed.sha(c), c) for c in omoide.chunks(read_protected_text(_koyori.STROBE)))
    if any(e["kind"] == "footprint" for e in entries):
        out.update((_embed.sha(t), t) for *_, t in _koyori.load_footprints())
    return out


def pool(entries, before_day=None):
    """候補になる行。before_day（YYYY-MM-DD）があれば、それより前の日のものだけ（さかのぼりで、その時点に無かったものを除く）。"""
    return [e for e in entries if e["kind"] in KINDS and e.get("scene") != "alone"
            and (before_day is None or (e["day"] or "9999") < before_day)]


def query_vector(text, timeout=SEM_TIMEOUT):
    """常駐に問いを埋め込んでもらう。期限までに答えなければ None（頼んだ分は取り消さず、常駐はそのまま起きていく）。
    常駐がいない・古い版なら、裏で起こして None。ここでは待たない。"""
    name = _embed.DEFAULT_MODEL
    q_prefix = _embed.MODELS[name][1]
    req = {"op": "encode", "model": name, "device": _embed.DEFAULT_DEVICE, "stamp": _embed.CODE_STAMP, "texts": [q_prefix + text]}
    try:
        resp = _embed._ask(req, timeout=timeout)
    except (FileNotFoundError, ConnectionRefusedError):
        _wake()
        return None
    except TimeoutError:
        return None
    if "vecs" in resp:
        return _embed._unpack(resp["vecs"])
    if resp.get("stale"):
        _wake()
        return None
    raise RuntimeError(f"embedding server {_embed.SOCK}: response has no vectors or stale status")


def _wake():
    _embed._spawn()


def lexical_raw(texts_by_sha, shas, query):
    """字の側の素の点：BM25（omoide と同じ式。統計は渡した断片から）を、問いの 2-gram の数で割る。"""
    docs = [(None, 0, [texts_by_sha.get(s, "")]) for s in shas]
    raw = omoide.lexical_scores(docs, [query])
    n = max(1, len(set(omoide.bigrams(query))))
    return [r / n for r in raw]


def snippet(e, query, texts_by_sha, window=-1, core=True):
    """届ける一行。字が当たっていればそのあたり、なければ意味でいちばん近いウィンドウのあたり。
    原文の断片は見出しの行（だれ・時刻）を除く。判は、その日の判を核から取り直す。"""
    text = texts_by_sha.get(e["sha"])
    if text is None and e["kind"] == "judgment" and core:   # core=False（見張りから）なら、核を待たない
        text = _koyori.texts_for([e]).get(e["sha"])
    if not text:
        return None
    pos, span = omoide.first_hit(text, [query.lower()])
    if pos is None and window >= 0:
        pos, end = _embed.window_span(text, window)
        span = min(end - pos, SNIP // 2)
    cut = len(text) - len(_koyori.body_of(text)) if e["kind"] == "utterance" else 0
    body = text[cut:]
    center = None if pos is None else max(0, pos - cut)
    return omoide.clip(body, SNIP, center, span).replace("\n", " ").strip()


def where(e):
    """出どころの一言：fuutou.py の名前と、時刻か行かコマ。"""
    name = omoide.name_of(e["file"]) or ("michikusa:" + e["day"] if e["kind"] == "footprint" else e["file"])
    if e["kind"] == "judgment":
        return f"{name} {e['line']} 行め"
    if e["kind"] == "footprint":
        return f"{name} 第 {e['koma']} コマ"
    if e["at"]:
        return f"{name} {e['at'][11:19]}・{e['sid']}"
    return f"{name} 断片 {e['i']}"


# ---------- たぐる ----------

def taguri(query, now=None, before_day=None, qvec=None, use_sem=True, catalog=None, texts=None, probe=None, core=True):
    """発話から、届ける断片があるかを見る。返り値は offer / withhold / unavailable の dict。
    さかのぼりのときは before_day・qvec・catalog・texts を渡す（その時点の候補と、先に作った問いのベクトル）。
    probe（sha）を渡すと、その断片が同じ計算の中でどう扱われたかを result["probe"] に入れる（さかのぼりで答えを見るため）。

    候補ごとに、その候補で実際に使える側だけで見る：字の側は中身があるとき、意味の側は問いのベクトルと
    その候補のベクトルが両方あるとき。どちらかの側で門を通れば「通った」。届けてよいのは、使える側のどれかが
    届ける線を越えたもの（意味は SEM_OFFER、字は LEX_OFFER）。通ったものを並べて、上から順に
    届けてよいものを探し、原文の一行が取れた最初の一つを届ける（取れなければ次へ。取れなかった数も記す）。"""
    now = now or datetime.now(_hk.JST)
    if len(PUNCT.sub("", query or "")) < MIN_QUERY:
        return {"kind": "withhold", "checked": {"why": "too_short", "scope": "none", "top": None,
                                                "thresholds": thresholds(), "n_pool": 0}}
    cat = _koyori.load() if catalog is None else catalog
    cands = pool(cat["entries"], before_day)
    texts = texts if texts is not None else lexical_texts(cands)
    idx = _embed.Index()
    if not cands:
        return {"kind": "withhold", "checked": {"why": "no_candidates", "scope": "none", "top": None,
                                                "thresholds": thresholds(), "n_pool": 0}}
    shas = [e["sha"] for e in cands]
    have_text = [s in texts for s in shas]
    lex_all = lexical_raw(texts, shas, query)          # 統計は、この回の候補ぜんぶから（さかのぼりの答えも、同じ数を使う）
    if use_sem and qvec is None:
        qvec = query_vector(query)
    sem, win = [None] * len(cands), None
    if qvec is not None:
        s, win = idx.scores(shas, qvec)
        sem = [None if math.isnan(x) else float(x) for x in s]
    scope = "both" if qvec is not None else "lexical"

    def rel(raw, gate, ceil):
        return 0.0 if raw is None or raw < gate else min(1.0, (raw - gate) / (ceil - gate))

    rows = []
    for k, e in enumerate(cands):
        l = lex_all[k] if have_text[k] else None
        m = sem[k]
        passed = (l is not None and l >= T_LEX) or (m is not None and m >= T_SEM)
        deliverable = (l is not None and l >= LEX_OFFER) or (m is not None and m >= SEM_OFFER)
        at = datetime.fromisoformat(e["at"]) if e["at"] else datetime.fromisoformat(e["day"] + "T12:00:00+09:00")
        recency = 0.5 ** (max(0.0, (now - at).total_seconds() / 86400) / HALF_LIFE_DAYS)
        relevance = max(rel(l, T_LEX, LEX_CEIL), rel(m, T_SEM, SEM_CEIL))
        rows.append({"k": k, "l": l, "m": m, "passed": passed, "deliverable": deliverable, "recency": recency,
                     "relevance": relevance, "total": recency + e["importance"] + relevance})

    def describe(r):
        e = cands[r["k"]]
        l, m = r["l"], r["m"]
        return {"kind": e["kind"], "where": where(e), "day": e["day"], "at": e["at"], "who": e["who"],
                "certainty": e["certainty"], "importance": e["importance"], "imp_why": e["imp_why"],
                "raw": {"lex": None if l is None else round(l, 3), "sem": None if m is None else round(m, 4)},
                "channels": [n for n, v in (("lexical", l), ("semantic", m)) if v is not None],
                "sides": [n for n, ok in (("lexical", l is not None and l >= T_LEX), ("semantic", m is not None and m >= T_SEM)) if ok],
                "rank": {"total": round(r["total"], 4), "recency": round(r["recency"], 4), "relevance": round(r["relevance"], 4)},
                "sha": e["sha"]}

    passed = sorted((r for r in rows if r["passed"]), key=lambda r: -r["total"])
    sems = sorted((r["m"] for r in rows if r["m"] is not None), reverse=True)
    diag = {"sem_top": round(sems[0], 4) if sems else None,
            "sem_gap10": round(sems[0] - sems[9], 4) if len(sems) >= 10 else None}   # 診断だけ。門には使わない
    base = {"scope": scope, "thresholds": thresholds(), "n_pool": len(cands), "n_passed": len(passed), "diag": diag}
    result = None
    if not passed:
        best = max(rows, key=lambda r: max(r["m"] or 0.0, (r["l"] or 0.0) / LEX_CEIL))
        result = {"kind": "withhold", "checked": {**base, "why": "no_candidates", "top": describe(best)}}
    else:
        skipped = 0
        for r in passed:
            if not r["deliverable"]:
                continue
            c = describe(r)
            c["snippet"] = snippet(cands[r["k"]], query, texts, int(win[r["k"]]) if win is not None else -1, core)
            if not c["snippet"]:
                skipped += 1
                continue
            result = {"kind": "offer", "candidates": [c],
                      "checked": {**base, "why": "offered", "top": describe(passed[0]), "skipped_no_text": skipped}}
            break
        if result is None:
            result = {"kind": "withhold", "checked": {**base, "why": "not_useful", "top": describe(passed[0]),
                                                      "skipped_no_text": skipped}}
    if probe is not None:
        r = next((r for r in rows if cands[r["k"]]["sha"] == probe), None)
        order = [x["k"] for x in sorted(rows, key=lambda x: -x["total"]) if x["passed"]]
        result["probe"] = None if r is None else {
            "lex": r["l"], "sem": r["m"], "passed": r["passed"], "deliverable": r["deliverable"],
            "rank_passed": order.index(r["k"]) + 1 if r["k"] in order else None,
            "rank_sem": (sum(1 for x in rows if x["m"] is not None and r["m"] is not None and x["m"] > r["m"]) + 1) if r["m"] is not None else None}
    return result


def check(result):
    """返り値の形を確かめる。おかしければ ValueError。"""
    kind = result.get("kind")
    if kind == "offer":
        if not result.get("candidates"):
            raise ValueError("taguri offer: candidates is empty")
        if any(not c.get("snippet") for c in result["candidates"]):
            raise ValueError("taguri offer: candidate has no source snippet")
        ck = result.get("checked") or {}
    elif kind == "withhold":
        ck = result.get("checked") or {}
        if ck.get("why") not in ("too_short", "no_candidates", "not_useful"):
            raise ValueError("taguri withhold: checked.why is missing")
    elif kind == "unavailable":
        if not result.get("failure"):
            raise ValueError("taguri unavailable: failure is missing")
        return result
    else:
        raise ValueError(f"taguri result: unknown kind: {kind}")
    for key in ("scope", "thresholds"):
        if key not in ck:
            raise ValueError(f"taguri result: checked.{key} is missing")
    if ck["scope"] != "none" and ck.get("top") is None:
        raise ValueError("taguri result: checked.top has no raw score")
    return result


# ---------- さかのぼり ----------

def replay(sample_ordinary=300):
    """過去の会話で、わたしが前の日のユーザーの言葉を「」で引いた時刻（しるしの生データの quotes）を答えにして、
    その直前のユーザーの発話からたぐる。点と件数だけ出す。"""
    import random
    cat = _koyori.load()
    es = cat["entries"]
    texts = lexical_texts(pool(es))
    utt = [e for e in es if e["kind"] == "utterance" and e["at"]]
    by_at = {(e["sid"], e["at"]): e for e in utt}
    fumi = sorted((e for e in utt if e["who"] == "ふ" and e["scene"] == "with_user"), key=lambda e: e["at"])
    fumi_by_sid = {}
    for e in fumi:
        fumi_by_sid.setdefault(e["sid"], []).append(e)
    events = []
    for p in sorted(_hk.AN.glob("*.mark.json.b64")):
        for h in read_protected_json(p):
            for q in h.get("quotes", []):
                if not q.get("hit"):
                    continue
                t, hit = datetime.fromisoformat(q["t"]), datetime.fromisoformat(q["hit"])
                if _hk.hikaru_day(hit) >= _hk.hikaru_day(t):
                    continue   # その日のうちの言葉は、まだ手もとにある
                answer = next((e for e in fumi if e["at"] == q["hit"]), None)
                prior = [e for e in fumi_by_sid.get(h["sid"], ()) if datetime.fromisoformat(e["at"]) < t]
                if not answer or not prior or t - datetime.fromisoformat(prior[-1]["at"]) > timedelta(minutes=30):
                    continue   # 答えの断片がない、直前のユーザーの発話がない（朝の仕事の報告など）
                events.append((prior[-1], answer, t))
    rnd = random.Random(20261028)
    ordinary = rnd.sample(fumi, min(sample_ordinary, len(fumi)))
    queries = [texts.get(e["sha"], "").split("\n", 1)[-1] for e, _, _ in events] + \
              [texts.get(e["sha"], "").split("\n", 1)[-1] for e in ordinary]
    emb = _embed.Embedder()
    t0 = time.perf_counter()
    vecs = emb.queries(queries) if queries else []
    print(f"問いを埋め込んだ：{len(queries)} 件（{time.perf_counter() - t0:.1f} 秒）")

    def run(q, qv, t, probe=None):
        return taguri(q, now=t, before_day=_hk.hikaru_day(t).isoformat(), qvec=qv[None, :], catalog=cat, texts=texts,
                      probe=probe)

    hit_offer = hit_top = 0
    probes = []
    for n, (trig, answer, t) in enumerate(events):
        r = run(queries[n], vecs[n], t, probe=answer["sha"])
        check(r)
        probes.append(r.get("probe") or {})     # 答えの断片の点と順位は、同じ計算の中から取る（別に数え直さない）
        if r["kind"] == "offer" and r["candidates"][0]["sha"] == answer["sha"]:
            hit_offer += 1
        if ((r.get("checked") or {}).get("top") or {}).get("sha") == answer["sha"]:
            hit_top += 1
    offered = 0
    kinds = {}
    for n, e in enumerate(ordinary):
        j = len(events) + n
        t = datetime.fromisoformat(e["at"])
        r = run(queries[j], vecs[j], t)
        check(r)
        key = r["kind"] if r["kind"] != "withhold" else "withhold:" + r["checked"]["why"]
        kinds[key] = kinds.get(key, 0) + 1
        offered += r["kind"] == "offer"
    print("（いまのこよりと索引で、過去の問いを引き直したもの。その時点で手もとにあった判や大事さとは限らない）")
    print(f"答えのある時刻：{len(events)}。答えが通った候補の一位 {hit_top}、答えを届けた {hit_offer}")
    ans_sem = [p["sem"] for p in probes if p.get("sem") is not None]
    ans_lex = [p["lex"] for p in probes if p.get("lex") is not None]
    if ans_sem:
        srt = sorted(ans_sem)
        print(f"  答えの意味の点：最小 {srt[0]:.3f}、中央 {srt[len(srt)//2]:.3f}、最大 {srt[-1]:.3f}；"
              f"門 {T_SEM} 以上 {sum(x >= T_SEM for x in ans_sem)}、届ける線 {SEM_OFFER} 以上 {sum(x >= SEM_OFFER for x in ans_sem)}")
        rs = sorted(p["rank_sem"] for p in probes if p.get("rank_sem"))
        print(f"  答えの意味での順位：中央 {rs[len(rs)//2]}、10 位以内 {sum(x <= 10 for x in rs)}、100 位以内 {sum(x <= 100 for x in rs)}")
    if ans_lex:
        sl = sorted(ans_lex)
        print(f"  答えの字の点：最小 {sl[0]:.2f}、中央 {sl[len(sl)//2]:.2f}、最大 {sl[-1]:.2f}；"
              f"門 {T_LEX} 以上 {sum(x >= T_LEX for x in ans_lex)}、届ける線 {LEX_OFFER} 以上 {sum(x >= LEX_OFFER for x in ans_lex)}")
    print(f"  答えが門を通った {sum(1 for p in probes if p.get('passed'))}、届けてよい線を越えた {sum(1 for p in probes if p.get('deliverable'))}")
    print(f"ふつうの発話 {len(ordinary)} 件：届けた {offered}（{offered / max(1, len(ordinary)):.0%}）　内訳 {kinds}")
    return {"events": len(events), "hit_top": hit_top, "hit_offer": hit_offer, "ordinary": len(ordinary), "offered": offered,
            "ans_sem": ans_sem, "ans_lex": ans_lex}



# ---------- 時間線：夕顔の項目の、前のところ ----------

TIMELINE_VERSION = "timeline-v1"
RECENT = 6             # 「いま話している」とみなす、直前の発話の数


def things_in(text, table, day=None):
    """text に出てくる夕顔の項目 → 当たった別名の列。day を渡すと、その日より前から夕顔にある項目だけ
    （さかのぼり用。いまの夕顔で過去を引き直すので、その日に本当にあった別名と同じとは限らない）。"""
    out = {}
    for name, v in (table or {}).items():
        if day is not None and not (v.get("since") and v["since"] < day):
            continue
        hit = [a for a in v.get("aliases", ()) if _koyori.alias_pattern(a).search(text or "")]
        if hit:
            out[name] = hit
    return out


def timeline(query, recent=(), cutoff=None, now=None, catalog=None, texts=None, replay=False):
    """発話に出てきた夕顔の項目について、前に出てきたところを一つ探す。返り値は offer / withhold / unavailable。
    - 項目が出てこなければ withhold（no_thing）。直前 RECENT 発話（recent）にもう出ていれば、いま話している最中なので
      withhold（in_talk）。
    - 残った項目ごとに、その項目のついた原文の断片（ユーザーと話していたひとときのもの）のうち、cutoff より前のものを集める。
      cutoff は、渡された直前の発話のいちばん早い時刻（なければ now）。いま続いている会話は原文にまだ入っていないので、
      見えるのは保存された記録の中だけ。
    - 項目がいくつかあれば、記録の中で出てくる回数がいちばん少ない（いちばん特定の）ものを選ぶ。
    - 断片は、発話に出てきたのと同じ別名が入っているものだけ。その項目のいちばん新しいひとときの中から、
      問いと字がいちばん重なる断片を一つ（同じなら新しいほう）。
    これは「前にこの言葉が出てきた」を出すだけで、同じ人や物のことかどうかも、一緒に過ごしたことかどうかも、決めない。
    使うかどうかは、読んだわたしが決める。"""
    now = now or datetime.now(_hk.JST)
    cat = _koyori.load() if catalog is None else catalog
    table = cat["things"]
    day = _hk.hikaru_day(now).isoformat()
    ck = {"version": TIMELINE_VERSION, "scope": "timeline"}
    q = things_in(query, table, day if replay else None)
    if not q:
        return {"kind": "withhold", "checked": {**ck, "why": "no_thing"}}
    talk = set(things_in("\n".join(recent), table))
    fresh = {n: a for n, a in q.items() if n not in talk}
    ck["things"] = len(q)
    if not fresh:
        return {"kind": "withhold", "checked": {**ck, "why": "in_talk"}}
    cut = cutoff or now
    texts_all = texts if texts is not None else lexical_texts(cat["entries"])

    def when(e):
        if e.get("at"):
            return datetime.fromisoformat(e["at"])
        return datetime.fromisoformat(e["day"] + "T05:00:00").replace(tzinfo=_hk.JST) if e.get("day") else None
    frags = {}
    for e in cat["entries"]:
        if e["kind"] != "utterance" or e.get("scene") != "with_user":
            continue
        t = when(e)
        if t is None or t >= cut:
            continue
        for n in e.get("things", ()):
            if n in fresh and any(_koyori.alias_pattern(x).search(_koyori.body_of(texts_all.get(e["sha"], ""))) for x in fresh[n]):
                frags.setdefault(n, []).append((t, e))   # 発話に出てきたのと同じ別名が入っている断片だけ
    fresh = {n: a for n, a in fresh.items() if n in frags}
    if not fresh:
        return {"kind": "withhold", "checked": {**ck, "why": "no_past"}}
    count = {}
    for e in cat["entries"]:
        for n in e.get("things", ()):
            count[n] = count.get(n, 0) + 1
    name = min(fresh, key=lambda n: (count.get(n, 0), n))
    group = lambda e: (e["day"], e.get("hitotoki") or e["file"])
    last = max(frags[name], key=lambda x: x[0])[1]
    pick = [(t, e) for t, e in frags[name] if group(e) == group(last)]
    texts = texts_all
    score = lexical_raw(texts, [e["sha"] for _, e in pick], query)
    t, e = max(zip(pick, score), key=lambda x: (x[1], x[0][0]))[0]
    body = _koyori.body_of(texts.get(e["sha"], ""))
    alias, hit = next((a, m) for a in fresh[name] if (m := _koyori.alias_pattern(a).search(body)))
    line = omoide.clip(body, SNIP, hit.start(), (hit.end() - hit.start()) // 2).replace("\n", " ").strip()   # 別名の当たったところを真ん中に
    if not line:
        return {"kind": "withhold", "checked": {**ck, "why": "no_text", "thing": name}}
    c = {"sha": e["sha"], "kind": e["kind"], "day": e["day"], "at": e.get("at"), "who": e.get("who"),
         "where": where(e), "snippet": line, "thing": name, "alias": alias, "importance": e.get("importance")}
    return {"kind": "offer", "candidates": [c],
            "checked": {**ck, "why": "offered", "thing": name, "n_past": len(frags[name]), "n_in_group": len(pick)}}


# ---------- 足跡を条件で（41 号） ----------

def footprints(since=None, until=None, where=None, material=None, opened=None, prefer=None):
    """自分の時間の足跡を、条件で引く（41 号）。日は HouseDay（朝 5 時切り、YYYY-MM-DD）。since・until を省くと全部の日。
    where は "home" か、外にいたところの名前（「研究室」など、部分一致）。material は材料の名前（部分一致）。
    opened は進み具合（finished、read_body、glanced、not_reached、worked のどれか。そのコマのどれかの行に合えばよい）。
    みちくさの一節（①）が一コマ一件で、構造化の記録（②、10/28 13:25 から）があれば、そのコマの行を添える。
    - 明示された条件（since・until・where・material・opened）で絞る。prefer（{"where": …, "material": …, "opened": …}）は、
      聞き手が言ったのではなく、わたしが推し量った条件で、絞らずに、合うものを前に並べるだけ。
    - ②がないコマは、where・opened では「記録なし」として外し（数えて explain.unknown に書く）、material は①の本文で探す。
      記録がないのは、そこにいなかった、読まなかった、ということではない。②はあるのに聞かれた材料の行がないコマは、
      「記録なし」ではなく、そのコマではその材料を読んでいないので、explain.absent_material に別に数える。
    - 明示された条件で一件もなければ、日の範囲はそのままにして、where、opened、material の順に一つずつ外して引き直す。
      それで見つかったものには maybe を付け、explain の relaxed に外した条件を書く（「たぶんこの回」で、「聞かれたその回」とは言えない）。
    並びは日付と時刻の順（prefer があれば、合う数の多いものが前）。いま進んでいるコマは、終わるまでここには出ない。"""
    rows = {}
    for p in sorted(_koyori.MICHIKUSA.glob("*.footprints.jsonl.b64")):
        text = read_protected_text(p)
        for number, line in enumerate(text.splitlines(), 1):
            r = parse_json(line, f"{p}:{number}")
            m = re.match(r"run-(\d+)-", str(r.get("step") or ""))
            if m:
                rows.setdefault(int(m[1]), {})[r.get("step")] = r   # 同じ step は一つに
    def place(r):
        w = r.get("where")
        return "home" if w == "home" else (w.get("away") if isinstance(w, dict) else None)
    komas, all_days = [], []
    for d, koma, hh, mm, text in _koyori.load_footprints():
        day = d.isoformat()
        all_days.append(day)
        if (since and day < since) or (until and day > until):
            continue
        m = re.search(r"<!--\s*run\s+(\d+)\s*-->", text)
        komas.append((day, koma, hh, mm, text, list(rows.get(int(m[1]), {}).values()) if m else []))

    def fits(k, cond, val, mat=None):
        """そのコマが条件に合うか。True／False、②がなくて分からなければ None。
        mat（材料の条件）があるとき、進み具合はその材料の行の中だけで見る（ほかの材料の「読み終えた」を借りない）。"""
        text, rs = k[4], k[5]
        if cond == "opened" and mat is not None:
            rs = [r for r in rs if mat in str(r.get("material") or "") or mat in str(r.get("origin") or "")]
            if k[5] and not rs:
                return False   # ②はあるが、その材料の行がない：記録不明ではなく、そのコマではその材料を読んでいない
        if cond in ("where", "opened"):   # 合う行があれば True。なければ、書かれていない行が一つでもあれば None。全部はっきり違えば False
            vals = [place(r) if cond == "where" else (r.get("visit") or {}).get("opened") for r in rs]
            if any(w is not None and (w == val or (cond == "where" and val != "home" and val in w)) for w in vals):
                return True
            return None if (not vals or None in vals) else False
        if cond == "material":
            return any(val in str(r.get("material") or "") or val in str(r.get("origin") or "") for r in rs) if rs else (val in text)
        return True

    def pick(conds):
        mat = conds.get("material")
        return [k for k in komas if all(fits(k, c, v, mat) for c, v in conds.items())]

    asked = {c: v for c, v in (("where", where), ("material", material), ("opened", opened)) if v is not None}
    pref = {c: v for c, v in (prefer or {}).items() if v is not None}
    unknown = {}   # 範囲のコマのうち、②がない（または②に書かれていない）ために分からなかった数（条件ごとに、一コマ一回）
    absent = 0     # ②はあるが、聞かれた材料の行がなかったコマの数（そのコマではその材料を読んでいない）
    for k in komas:
        miss = {c for c, v in asked.items() if fits(k, c, v, material) is None}   # 聞かれた条件は、筛るときと同じ範囲で
        miss |= {c for c, v in pref.items() if fits(k, c, v, pref.get("material", material)) is None}   # 推し量った条件は、推し量った材料で
        for c in miss:   # 一コマで、一つの条件は一回だけ数える
            unknown[c] = unknown.get(c, 0) + 1
        if material and k[5] and not any(material in str(r.get("material") or "") or material in str(r.get("origin") or "") for r in k[5]):
            absent += 1
    hit = pick(asked)
    relaxed = []
    for cond in ("where", "opened", "material"):   # 一件もないときだけ、一つずつ外す
        if hit or cond not in asked:
            continue
        del asked[cond]
        relaxed.append(cond)
        hit = pick(asked)
    score = lambda k: sum(1 for c, v in pref.items() if fits(k, c, v, pref.get("material", material)))   # 撤いた条件ではなく、もとの材料で
    hit = sorted(hit, key=lambda k: (-score(k), k[0], k[2] * 60 + k[3] if k[2] >= 5 else (k[2] + 24) * 60 + k[3]))
    found = [{"day": day, "koma": koma, "start": f"{hh:02d}:{mm:02d}", "where_text": f"michikusa:{day} 第 {koma} コマ",
              "materials": [{x: r.get(x) for x in ("material", "origin", "minutes", "where", "left", "segment")}
                            | {"opened": (r.get("visit") or {}).get("opened")} for r in rs],
              "maybe": bool(relaxed), "preferred": score((day, koma, hh, mm, text, rs)) if pref else None, "text": text}
             for day, koma, hh, mm, text, rs in hit]
    explain = {"found": len(found), "since": since, "until": until, "where": where, "material": material, "opened": opened,
               "prefer": pref or None, "relaxed": relaxed, "in_range": len(komas),
               "no_structured": sum(1 for k in komas if not k[5]), "unknown": unknown, "absent_material": absent,
               "covered_until": max(all_days) if all_days else None}
    if not all_days:
        return {"kind": "unavailable", "failure": "no_footprints", "explain": explain}
    return {"kind": "offer" if found else "withhold", "results": found, "explain": explain}


def main():
    ap = argparse.ArgumentParser(description="たぐり：発話からこよりを手繰る。--replay でさかのぼり")
    ap.add_argument("query", nargs="?")
    ap.add_argument("--replay", action="store_true")
    ap.add_argument("--timeline", action="store_true", help="夕顔の項目の、前のところを探す（直前の発話は渡さない）")
    ap.add_argument("--footprints", action="store_true", help="足跡を条件で引く（--since --until --where --material --opened、推し量った条件は --prefer-where など）")
    ap.add_argument("--since")
    ap.add_argument("--until")
    ap.add_argument("--where")
    ap.add_argument("--material")
    ap.add_argument("--opened", help="進み具合：finished / read_body / glanced / not_reached / worked")
    ap.add_argument("--prefer-where")
    ap.add_argument("--prefer-material")
    ap.add_argument("--prefer-opened")
    a = ap.parse_args()
    if a.replay:
        replay()
        return
    if a.footprints:
        prefer = {"where": a.prefer_where, "material": a.prefer_material, "opened": a.prefer_opened}
        print(json.dumps(footprints(a.since, a.until, a.where, a.material, a.opened, prefer), ensure_ascii=False, indent=1))
        return
    if a.timeline:
        print(json.dumps(timeline(a.query or ""), ensure_ascii=False, indent=1))
        return
    t0 = time.perf_counter()
    r = check(taguri(a.query or ""))
    r["seconds"] = round(time.perf_counter() - t0, 3)
    print(json.dumps(r, ensure_ascii=False, indent=1))


if __name__ == "__main__":
    run_cli(main)
