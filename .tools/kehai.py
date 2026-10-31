#!/usr/bin/env python3
"""Optional macOS media playback status adapter."""
import ctypes as C
import json
import sys

CA = C.CDLL("/System/Library/Frameworks/CoreAudio.framework/CoreAudio")


def fourcc(s):
    return int.from_bytes(s.encode(), "big")


class Address(C.Structure):
    _fields_ = [("selector", C.c_uint32), ("scope", C.c_uint32), ("element", C.c_uint32)]


SYSTEM_OBJECT = 1
GLOBAL = fourcc("glob")
MAIN = 0
DEFAULT_OUTPUT = fourcc("dOut")
RUNNING_SOMEWHERE = fourcc("gone")

CA.AudioObjectGetPropertyData.argtypes = [C.c_uint32, C.POINTER(Address), C.c_uint32, C.c_void_p,
                                          C.POINTER(C.c_uint32), C.c_void_p]
CA.AudioObjectGetPropertyData.restype = C.c_int32


def get_u32(obj, selector):
    addr = Address(selector, GLOBAL, MAIN)
    value = C.c_uint32(0)
    size = C.c_uint32(C.sizeof(value))
    status = CA.AudioObjectGetPropertyData(obj, C.byref(addr), 0, None, C.byref(size), C.byref(value))
    if status != 0:
        raise OSError(status)
    return value.value


ADAPTER = __import__("pathlib").Path.home() / ".local" / "lib" / "mediaremote-adapter"


def now_playing():
    """「再生中」なら True、止まっていれば False、読めなければ None。題名などは返さない。"""
    import subprocess
    try:
        out = subprocess.run(["/usr/bin/perl", str(ADAPTER / "mediaremote-adapter.pl"),
                              str(ADAPTER / "MediaRemoteAdapter.framework"), "get"],
                             capture_output=True, text=True, timeout=5).stdout.strip()
    except (OSError, subprocess.SubprocessError):
        return None
    if not out or out == "null":
        return False if out == "null" else None
    try:
        info = json.loads(out)
    except ValueError:
        return None
    return bool(info.get("playing")) if isinstance(info, dict) else None


def playing():
    """True / False。分からなければ None。"""
    state = now_playing()
    if state is not None:
        return state
    try:
        device = get_u32(SYSTEM_OBJECT, DEFAULT_OUTPUT)
        return bool(get_u32(device, RUNNING_SOMEWHERE))
    except OSError:
        return None


def main():
    state = playing()
    if "--json" in sys.argv[1:]:
        print(json.dumps({"playing": state}))
    else:
        print({True: "再生中", False: "再生していない", None: "不明"}[state])


if __name__ == "__main__":
    main()
