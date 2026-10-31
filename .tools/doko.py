#!/usr/bin/env python3
"""Format configured presence events as a short status."""
import json
import sys
from datetime import datetime, timedelta
from _tool_data import LOCATION_CONFIG, LOCATION_STATE, read_json, run_cli

STATE_FILE = LOCATION_STATE / "doko.json"
PLACES_FILE = LOCATION_CONFIG / "doko_places.json"
STALE = timedelta(hours=24)
# 囲いを「出た」と届いても、この時間はまだ中にいることにしておく。iPhone の囲いの判定は、
# 縁で出た・入ったを数分のうちに行き来することがある（2026-10-14、大学で何度も）。
# このあいだにまた「入った」が来れば、出なかったのと同じになる。
# 座標で敷地の外と確かめた「出た」（left.certain）は、待たない。
SETTLE = timedelta(minutes=5)


def load(path):
    try:
        value = read_json(path)
    except FileNotFoundError:
        return {}
    if not isinstance(value, dict):
        raise ValueError(f"{path}: expected a JSON object")
    return value


def parse(stamp):
    if stamp is None:
        return None
    return datetime.fromisoformat(stamp)


def clock(moment, today):
    if moment is None:
        return "?"
    local = moment.astimezone()
    text = local.strftime("%H:%M")
    return text if local.date() == today else f"{local.month}/{local.day} {text}"


def entry(wifi, places):
    """(つながったときの言い方, 切れたときの言い方)。起きたことを、起きたこととして言う。"""
    value = places.get(wifi)
    if isinstance(value, dict):
        label = value.get("name") or wifi
        return value.get("here") or f"{label}に着いた", value.get("away") or f"{label}を離れた"
    if isinstance(value, str) and value:
        return f"{value}に着いた", f"{value}を離れた"
    return f"Wi-Fi「{wifi}」につながった", f"Wi-Fi「{wifi}」が切れた"


def elapsed(moment, current):
    """その時刻から今までにたった時間（「17分」「4時間53分」「1分たらず」）。
    知らせが届いた時刻ではなく、起きた時刻から数える（2026-10-23、23:01 に学校を出たのを
    23:07 に初めて受け取ったわたしが、そこで「建物を出る」場面を書いた。六分前のことだった）。"""
    if moment is None:
        return "?"
    minutes = max(0, int((current - moment).total_seconds() // 60))
    if minutes < 1:
        return "1分たらず"
    if minutes < 60:
        return f"{minutes}分"
    hours, rest = divmod(minutes, 60)
    return f"{hours}時間{rest}分" if rest else f"{hours}時間"


def phrase(template, moment, today, current):
    """言い方の中の {t} に時刻を、{ago} にそこから今までにたった時間を入れる。
    どちらも無ければ、うしろに（時刻。それから、たった時間）をつける。"""
    since, ago = clock(moment, today), elapsed(moment, current)
    if "{t}" in template or "{ago}" in template:
        return template.replace("{t}", since).replace("{ago}", ago)
    return f"{template}（{since}。それから{ago}）"


def sentence():
    state = load(STATE_FILE)
    updated = parse(state.get("updated"))
    if updated is None:
        return None
    current = datetime.now().astimezone()
    if current - updated > STALE:
        return None
    places = load(PLACES_FILE)
    here = state.get("now") if isinstance(state.get("now"), dict) else {}
    before = state.get("before") if isinstance(state.get("before"), dict) else {}
    today = current.date()
    since = parse(here.get("since"))
    # 届くのは「つながった」「切れた」「囲いに入った」「囲いを出た」という出来事だけ。
    # それをどう言うかは、場所ごとに表で決める。いまも続いていることは状態で（「出かけている（10:29 から）」）、
    # そのあとが分からないことは出来事で（「大学を出た（17:30）」）。
    # 順：つながっている Wi-Fi があればそれ。無ければ、中にいる囲い。それも無ければ、いちばん新しい「離れた」。
    area = state.get("area") if isinstance(state.get("area"), dict) else {}
    left = state.get("left") if isinstance(state.get("left"), dict) else {}
    gone_at = parse(left.get("at")) if left.get("name") else None
    if not area.get("name") and gone_at is not None and not left.get("certain") and current - gone_at < SETTLE:
        area = {"name": left["name"], "since": left.get("since")}
    held = state.get("held") if isinstance(state.get("held"), dict) else {}
    if here.get("wifi") and held.get("wifi") == here.get("wifi"):
        # 「切れた」を保留している：つながってはいないが、iPhoneはまだその囲いの中。どちらとも言わず、起きたことを言う
        value = places.get(here["wifi"])
        cut = value.get("cut") if isinstance(value, dict) and value.get("cut") else f"Wi-Fi「{here['wifi']}」が切れた（まだ近くにいる）"
        text = "ユーザーは " + phrase(cut, parse(held.get("at")), today, current)
    elif here.get("wifi"):
        text = "ユーザーは " + phrase(entry(here["wifi"], places)[0], since, today, current)
    elif area.get("name"):
        # 囲いの中で、建物の Wi-Fi が切れた：起点は、囲いに入った時と Wi-Fi が切れた時の、遅いほう。
        moments = [m for m in (parse(area.get("since")), parse(here.get("since"))) if m is not None]
        text = "ユーザーは " + phrase(entry(area["name"], places)[0], max(moments) if moments else None, today, current)
    else:
        gone_wifi = parse(before.get("until")) if before.get("wifi") else None
        gone_area = parse(left.get("at")) if left.get("name") else None
        if gone_area is not None and (gone_wifi is None or gone_area >= gone_wifi):
            text = "ユーザーは " + phrase(entry(left["name"], places)[1], gone_area, today, current)
        elif before.get("wifi"):
            text = "ユーザーは " + phrase(entry(before["wifi"], places)[1], since, today, current)
        else:
            return None
    return text


TAIL = "（知っておくだけ。話に出さない）"


def clock_now():
    from zoneinfo import ZoneInfo
    return datetime.now(ZoneInfo("Asia/Tokyo")).strftime("%H:%M")


def hook_line():
    """時刻の知らせと一つの行にする：「いまは HH:MM。ユーザーは …（知っておくだけ。話に出さない）」。
    居場所が分からなければ、時刻だけ（これまでの時刻の知らせと同じ形）。"""
    where = sentence()
    return f"いまは {clock_now()}" + (f"。{where}" if where else "") + TAIL


def main():
    if "--hook" in sys.argv[1:]:
        print(json.dumps({"suppressOutput": True, "hookSpecificOutput": {
            "hookEventName": "UserPromptSubmit", "additionalContext": hook_line()}}, ensure_ascii=False))
        return
    text = sentence()
    if text:
        print(text + TAIL)


if __name__ == "__main__":
    if "--hook" in sys.argv[1:]:
        try:
            main()
        except Exception:  # フックは、何があっても会話を止めない
            pass
    else:
        run_cli(main)
