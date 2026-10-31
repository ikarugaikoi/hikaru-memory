#!/usr/bin/env python3
"""Optional authenticated presence receiver. Configure places locally; no route history is stored."""
import base64
import hmac
import json
import os
import secrets
import subprocess
import sys
import threading
from datetime import datetime, timezone
from http.server import BaseHTTPRequestHandler, HTTPServer
from _tool_data import LOCATION_CONFIG, LOCATION_STATE, read_json, run_cli

PORT = 8765
MAX_BODY = 4096
CONFIG = LOCATION_CONFIG
STATE = LOCATION_STATE
TOKEN_FILE = CONFIG / "doko.token"
STATE_FILE = STATE / "doko.json"
PLACES_FILE = CONFIG / "doko_places.json"
CAMPUS_FILE = CONFIG / "campus.json"   # 大学の敷地の形（OpenStreetMap から取った輪。緯度・経度の並び）
MARGIN_M = 40                          # 敷地の縁からこれより近いときは、座標では決めない（位置のぶれを見込む）
EVENTS = ("join", "leave", "enter", "exit", "ping")
LOCK = threading.Lock()   # 二つの口（Tailscale の住所と 127.0.0.1）から同時に来ても、記録は一つずつ直す


def now_iso():
    return datetime.now(timezone.utc).astimezone().isoformat(timespec="seconds")


def minutes_between(earlier, later):
    """二つの時刻（isoformat）のあいだの分。"""
    return (datetime.fromisoformat(later) - datetime.fromisoformat(earlier)).total_seconds() / 60


def tailscale_ip():
    try:
        out = subprocess.run(["tailscale", "ip", "-4"], capture_output=True, text=True, timeout=5).stdout
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return None
    for line in out.splitlines():
        line = line.strip()
        if line.startswith("100."):
            return line
    return None


def init_token():
    CONFIG.mkdir(parents=True, exist_ok=True)
    try:
        fd = os.open(TOKEN_FILE, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    except FileExistsError:
        print(f"合言葉はもうあります: {TOKEN_FILE}")
        return
    with os.fdopen(fd, "w") as handle:
        handle.write(secrets.token_urlsafe(24) + "\n")
    print(f"合言葉を作りました: {TOKEN_FILE}（中身はここには出しません）")


def clean_text(value, limit):
    if not isinstance(value, str):
        return None
    value = "".join(ch for ch in value if ch.isprintable()).strip()
    return value[:limit] or None


def apply(report, previous):
    """受け取った一件から、新しい状態を作る。前の「いま」は、変わったときだけ「ひとつ前」に下がる。"""
    stamp = now_iso()
    event = report.get("event")
    wifi = clean_text(report.get("wifi"), 64)
    state = previous.copy()
    here = state.get("now") if isinstance(state.get("now"), dict) else {}
    if event == "join" and wifi:
        state.pop("held", None)   # つながったのなら、保留していた「切れた」は、切れたままではなかった
        if here.get("wifi") != wifi:
            # 「ひとつ前」は、最後にいた名前のある所。外にいたあいだのことでは上書きしない。
            if here.get("wifi"):
                state["before"] = {"wifi": here.get("wifi"), "since": here.get("since"), "until": stamp}
            state["now"] = {"wifi": wifi, "since": stamp}
        bssid = clean_text(report.get("bssid"), 32)
        if bssid:
            state["now"]["bssid"] = bssid
    elif event == "leave":
        # 切れた相手が、いまいる所と同じとき（か、名前が来なかったとき）だけ、外に出たことにする。
        inside = state.get("areas") if isinstance(state.get("areas"), dict) else {}
        if here.get("wifi") and (wifi is None or wifi == here.get("wifi")):
            if any(wifi_within(here.get("wifi"), n) for n in inside):
                # 第二信号：その Wi-Fi のある囲いの中にiPhoneがまだいると言っているあいだは、出たことにせず、保留する。
                # 囲いを出たときに出たことにする（release_held）。二度目の「切れた」は、保留の時刻を新しくする。
                state["held"] = {"wifi": here.get("wifi"), "at": stamp}
                by = "held"
            else:
                state.pop("held", None)
                state["before"] = {"wifi": here.get("wifi"), "since": here.get("since"), "until": stamp}
                state["now"] = {"wifi": None, "since": stamp}
        elif not here.get("wifi") and here.get("since") and wifi and minutes_between(here.get("since"), stamp) >= 10:
            # もう「外」なのに、名前つきの「切れた」がまた来た：あいだの「つながった」を取りこぼしている
            # （切れるには、先につながっていなければならない）。いつ戻ったかは分からないので、ひとつ前は
            # 「その名前に、いつからかは分からないが、いままで」とし、外はいまから数え直す。十分以内の二度目は、
            # 同じ一件の重複として見ない。2026-10-23：00:43 に家の Wi-Fi が切れたと届いたまま朝の「つながった」が
            # 届かず、14:59 の本当の出門が「変わらない」として捨てられ、見張りが 00:43 からの長い外出と見ていた。
            state["before"] = {"wifi": wifi, "since": None, "until": stamp}
            state["now"] = {"wifi": None, "since": stamp}
    elif event in ("enter", "exit"):
        # 囲いは重なることがある（東西の校区の囲いが大学通りの上で重なる、など）。
        # だから「いま中にいる囲い」は一つではなく、名前と入った時刻の組を全部持つ。
        # 全部から出たときだけ「出た」にする。area には、代表として、いちばん前から中にいるものを置く（doko.py が読む）。
        area = clean_text(report.get("area"), 64)
        inside = state.get("areas") if isinstance(state.get("areas"), dict) else {}
        if not inside and isinstance(state.get("area"), dict) and state["area"].get("name"):
            inside = {state["area"]["name"]: state["area"].get("since")}
        inside = dict(inside)
        point = coords(report)
        where = campus_where(*point) if point and area and area.startswith("campus") else None
        by = "phone" if point is None else f"coords-{where or 'edge'}"
        if where == "out":
            # 座標ではっきり敷地の外：携帯が「入った」と言っても信じず、大学の囲いを全部出たことにする。
            gone = [n for n in inside if n.startswith("campus")]
            for n in gone:
                since = inside.pop(n)
                if not inside:
                    state["left"] = {"name": n, "since": since, "at": stamp, "certain": True}
            if state.get("held") and any(wifi_within(state["held"].get("wifi"), n) for n in gone):
                release_held(state, stamp)
            event = None
        elif where == "in":
            # 座標ではっきり敷地の中：「出た」は信じない。「入った」は受ける。
            if event == "exit":
                event = None
        if event == "enter" and area:
            inside.setdefault(area, stamp)
        elif event == "exit" and inside and wifi_within(here.get("wifi"), area) and not state.get("held"):
            # 囲いの中にあると分かっている Wi-Fi（大学の建物の Wi-Fi など）につながっているあいだは、
            # 囲いの「出た」は信じない。建物の中では位置がぶれて、出ていないのに「出た」が来るから（2026-10-14）。
            # ただし、その Wi-Fi の「切れた」を保留しているときは、つながっていないので、「出た」を受ける。
            pass
        elif event == "exit" and inside:
            names = [area] if area else list(inside)
            gone = [n for n in names if n in inside]
            for n in gone:
                since = inside.pop(n)
                if not inside:
                    state["left"] = {"name": n, "since": since, "at": stamp}
            if state.get("held") and any(wifi_within(state["held"].get("wifi"), n) for n in gone):
                release_held(state, stamp)
        state["areas"] = inside
        if inside:
            first = min(inside, key=lambda n: inside[n] or "")
            state["area"] = {"name": first, "since": inside[first]}
        else:
            state["area"] = None
    battery = report.get("battery")
    if isinstance(battery, (int, float)) and not isinstance(battery, bool) and 0 <= battery <= 100:
        state["battery"] = int(battery)
    if isinstance(report.get("charging"), bool):
        state["charging"] = report["charging"]
    # 受け取った知らせを、新しいほうから五件だけ控える（何が来て、それで記録が変わったか）。
    # 場所の名前と時刻だけ。ほかの中身は残さない。古いものは押し出されて消える。
    name = clean_text(report.get("wifi"), 64) if event in ("join", "leave") else clean_text(report.get("area"), 64)
    changed = {k: state.get(k) for k in ("now", "before", "areas", "left")} != \
              {k: (previous or {}).get(k) for k in ("now", "before", "areas", "left")}
    recent = state.get("recent") if isinstance(state.get("recent"), list) else []
    if report.get("event") == "ping" and not changed:
        # 何も変わらなかった位置の知らせ（OwnTracks は動くたびに送ってくる）は控えない。五件が埋まってしまうから。
        state["updated"] = stamp
        return state
    state["recent"] = (recent + [{"event": report.get("event"), "name": name, "at": stamp, "changed": changed,
                                  "by": locals().get("by", "phone")}])[-5:]
    state["updated"] = stamp
    return state


def release_held(state, stamp):
    """保留していた「切れた」を、囲いを出たいま、出たことにする。囲いを出る 20 分以内に来ていた「切れた」なら
    その時刻で（出かける前に切れた）、それより前なら出た時刻で（夜中に切れていて、朝そのまま出た）。"""
    held = state.pop("held", None)
    here = state.get("now") if isinstance(state.get("now"), dict) else {}
    if not isinstance(held, dict) or not here.get("wifi") or here.get("wifi") != held.get("wifi"):
        return
    when = held.get("at") if held.get("at") and minutes_between(held["at"], stamp) <= 20 else stamp
    state["before"] = {"wifi": here.get("wifi"), "since": here.get("since"), "until": when}
    state["now"] = {"wifi": None, "since": when}


def campus_where(lat, lon):
    """座標が大学の敷地の「in」か「out」か。縁に近すぎるときや、敷地のファイルがまだないときは None。読めないときは止める。
    座標はここで見るだけで、どこにも残さない。"""
    try:
        rings = read_json(CAMPUS_FILE)["rings"]
    except FileNotFoundError:
        return None
    import math
    kx = 111320 * math.cos(math.radians(lat))
    ky = 110540
    inside = False
    nearest = float("inf")
    for ring in rings:
        pts = [((b - lon) * kx, (a - lat) * ky) for a, b in ring]   # 自分を原点にした平面（メートル）
        hit = False
        for (x1, y1), (x2, y2) in zip(pts, pts[1:] + pts[:1]):
            if (y1 > 0) != (y2 > 0) and 0 < x1 + (0 - y1) * (x2 - x1) / (y2 - y1):
                hit = not hit
            dx, dy = x2 - x1, y2 - y1
            t = max(0.0, min(1.0, -(x1 * dx + y1 * dy) / (dx * dx + dy * dy))) if dx or dy else 0.0
            nearest = min(nearest, math.hypot(x1 + t * dx, y1 + t * dy))
        inside = inside or hit
    if nearest < MARGIN_M:
        return None
    return "in" if inside else "out"


def coords(report):
    lat, lon = report.get("lat"), report.get("lon")
    try:
        lat, lon = float(str(lat).replace(",", ".")), float(str(lon).replace(",", "."))
    except (TypeError, ValueError):
        return None
    if -90 <= lat <= 90 and -180 <= lon <= 180:
        return lat, lon
    return None


def wifi_within(wifi, area):
    """その Wi-Fi が、その囲いの中にあると表に書いてあるか（doko_places.json の within）。"""
    if not wifi:
        return False
    try:
        places = read_json(PLACES_FILE)
    except FileNotFoundError:
        return False
    entry = places.get(wifi) if isinstance(places, dict) else None
    within = entry.get("within") if isinstance(entry, dict) else None
    if not isinstance(within, list):
        return False
    return area is None or area in within


def from_owntracks(message, state):
    """OwnTracks の一通を、こちらの知らせ（0 件以上）に直す。座標は判定に渡すだけで、残らない。"""
    if not isinstance(message, dict):
        return []
    extra = {"lat": message.get("lat"), "lon": message.get("lon")}
    battery = message.get("batt")
    if isinstance(battery, (int, float)) and not isinstance(battery, bool) and battery >= 0:
        extra["battery"] = battery
    if message.get("bs") in (1, 2, 3):
        extra["charging"] = message["bs"] in (2, 3)
    kind = message.get("_type")
    if kind == "transition":
        event = {"enter": "enter", "leave": "exit"}.get(message.get("event"))
        area = clean_text(message.get("desc"), 64)
        return [dict(extra, event=event, area=area)] if event and area else []
    if kind != "location":
        return []
    listed = message.get("inregions") if isinstance(message.get("inregions"), list) else []
    regions = {name for name in (clean_text(n, 64) for n in listed) if name}
    inside = state.get("areas") if isinstance(state.get("areas"), dict) else {}
    reports = [dict(extra, event="enter", area=n) for n in sorted(regions - set(inside))]
    reports += [dict(extra, event="exit", area=n) for n in sorted(set(inside) - regions)]
    if not regions and not any(n.startswith("campus") for n in inside):
        point = coords(extra)
        if point and campus_where(*point) == "in":
            # 囲いの円は敷地の角まで届かない。円の外でも、座標が敷地の中なら、大学の構内にいる。
            reports.append(dict(extra, event="enter", area="campus"))
    return reports or [dict(extra, event="ping")]


def read_state():
    try:
        return read_json(STATE_FILE)
    except FileNotFoundError:
        return {}


def write_state(state):
    STATE.mkdir(parents=True, exist_ok=True)
    temporary = STATE_FILE.with_suffix(".tmp")
    fd = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(fd, "w", encoding="utf-8") as handle:
        json.dump(state, handle, ensure_ascii=False)
    os.replace(temporary, STATE_FILE)


class Handler(BaseHTTPRequestHandler):
    token = ""

    def log_message(self, *_):  # 何も書かない
        return

    def answer(self, code):
        self.send_response(code)
        self.send_header("Content-Length", "0")
        self.end_headers()

    def do_GET(self):
        self.answer(404)

    def store_report(self, report):
        try:
            with LOCK:
                state = read_state()
                reports = from_owntracks(report, state) if self.path == "/owntracks" else [report]
                for one in reports:
                    state = apply(one, state)
                write_state(state)
        except (OSError, ValueError, KeyError, TypeError, AttributeError) as error:
            print(f"{type(error).__name__}: doko request {self.path}: {error}", file=sys.stderr)
            self.answer(500)
            return False
        return True

    def authorized(self):
        given = self.headers.get("X-Doko-Token", "")
        basic = self.headers.get("Authorization", "")
        if not given and basic.startswith("Basic "):
            try:
                given = base64.b64decode(basic[6:].strip()).decode("utf-8").partition(":")[2]
            except (ValueError, UnicodeDecodeError):
                given = ""
        return hmac.compare_digest(given.encode("utf-8"), self.token.encode("utf-8"))

    def do_POST(self):
        if self.path not in ("/doko", "/owntracks"):
            return self.answer(404)
        if not self.authorized():
            return self.answer(403)
        try:
            length = int(self.headers.get("Content-Length", "0"))
        except ValueError:
            return self.answer(400)
        if length <= 0 or length > MAX_BODY:
            return self.answer(413)
        try:
            report = json.loads(self.rfile.read(length).decode("utf-8"))
        except (ValueError, UnicodeDecodeError):
            return self.answer(400)
        if self.path == "/owntracks":
            if not self.store_report(report):
                return
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", "2")
            self.end_headers()
            self.wfile.write(b"[]")
            return
        if not isinstance(report, dict) or report.get("event") not in EVENTS:
            return self.answer(400)
        if self.store_report(report):
            self.answer(204)


def main():
    if sys.argv[1:] == ["--init"]:
        return init_token()
    if sys.argv[1:]:
        print(__doc__)
        return
    try:
        Handler.token = TOKEN_FILE.read_text(encoding="utf-8").strip()
    except FileNotFoundError:
        sys.exit("合言葉がありません。先に: python3 .tools/doko_recv.py --init")
    if len(Handler.token) < 16:
        sys.exit(f"ValueError: {TOKEN_FILE}: token is too short (at least 16 characters required)")
    address = tailscale_ip()
    if not address:
        sys.exit("Tailscale の住所が取れません（Tailscale の外には開きません）。")
    local = HTTPServer(("127.0.0.1", PORT), Handler)   # tailscale serve（https の前）が、ここへ回してくる
    threading.Thread(target=local.serve_forever, daemon=True).start()
    HTTPServer((address, PORT), Handler).serve_forever()


if __name__ == "__main__":
    run_cli(main)
