"""Shared activity history helpers."""
import json

import _hk
from _tool_data import parse_json, read_protected_text


def read_history_text(path):
    try:
        return read_protected_text(path)
    except FileNotFoundError:
        return None


def read_history_lines(path):
    old = read_history_text(path)
    if old is None:
        return ""
    for number, line in enumerate(old.splitlines(), 1):
        if line.strip() and not isinstance(parse_json(line, f"{path}:{number}"), dict):
            raise ValueError(f"{path}:{number}: history row must be an object")
    # A valid final JSON row need not have a trailing newline.
    return old + ("\n" if old and not old.endswith("\n") else "")


def read_history_reads(path):
    old = read_history_text(path)
    if old is None:
        return {}
    value = parse_json(old, path)
    if not isinstance(value, dict) or any(not isinstance(v, list) or any(not isinstance(t, str) for t in v)
                                          for v in value.values()):
        raise ValueError(f"{path}: history must map keys to timestamp lists")
    return value


def append_history(path, entry):
    with _hk.lock(path):
        old = read_history_lines(path)
        _hk.write_sealed(path, old + json.dumps(entry, ensure_ascii=False, separators=(",", ":")) + "\n")


text, json_lines, reads = read_history_text, read_history_lines, read_history_reads
