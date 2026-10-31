"""Shared paths and record boundaries; recovery policy belongs to the caller."""
import os
import base64
import binascii
import json
import re
from typing import NamedTuple
import sys
from datetime import datetime
from pathlib import Path
from subprocess import TimeoutExpired

import _hk

LOCATION_CONFIG = Path(os.environ.get("HIKARU_LOCATION_CONFIG", _hk.ROOT / ".local" / "config"))
LOCATION_STATE = Path(os.environ.get("HIKARU_LOCATION_STATE", _hk.ROOT / ".local" / "state"))


def index_dir(root):
    return root / ".tools" / "index" / "koyori"


def unseal_protected(data):
    return base64.b64decode(b"".join(data.split()), validate=True).decode("utf-8")


def read_protected_text(path):
    data = Path(path).read_bytes()
    try:
        return unseal_protected(data)
    except UnicodeDecodeError as error:
        raise UnicodeDecodeError(error.encoding, error.object, error.start, error.end,
                                 f"{path}: {error.reason}") from None
    except binascii.Error as error:
        raise binascii.Error(f"{path}: {error}") from None


def parse_json(text, path):
    try:
        return json.loads(text)
    except json.JSONDecodeError as error:
        raise json.JSONDecodeError(f"{path}: {error.msg}", error.doc, error.pos) from None


def read_json(path):
    try:
        text = Path(path).read_text(encoding="utf-8")
    except UnicodeDecodeError as error:
        raise UnicodeDecodeError(error.encoding, error.object, error.start, error.end,
                                 f"{path}: {error.reason}") from None
    return parse_json(text, path)


def read_protected_json(path):
    return parse_json(read_protected_text(path), path)


def read_rebuildable_json(path):
    try:
        return read_protected_json(path)
    except (FileNotFoundError, binascii.Error, UnicodeDecodeError, json.JSONDecodeError):
        return None


def read_frequency_cache(path):
    value = read_rebuildable_json(path)
    if not isinstance(value, dict):
        return {}
    aliases = value.get("aliases")
    return aliases if isinstance(aliases, dict) else {}


def write_json(path, value):
    _hk.write_sealed(path, json.dumps(value, ensure_ascii=False, separators=(",", ":")))


def now_jst():
    return datetime.now(_hk.JST).isoformat(timespec="seconds")


def now_local():
    return datetime.now().astimezone().isoformat(timespec="seconds")


class ConversationLogError(ValueError):
    """A complete conversation row could not be decoded or interpreted."""


class ConversationRecord(NamedTuple):
    number: int
    begin: int
    end: int
    terminated: bool
    open_string: bool
    raw: bytes


_RECORD_BYTES = re.compile(br'["\\\n]')


def conversation_records(data):
    """Byte framing only; LF ends a record exactly when outside a string."""
    begin = pos = 0
    line = start_line = 1
    quoted = False
    while match := _RECORD_BYTES.search(data, pos):
        index = match.start()
        byte = data[index]
        pos = index + 1
        if byte == 34:
            quoted = not quoted
        elif byte == 92 and quoted:
            # A backslash quotes exactly the next byte for framing. Invalid
            # JSON escapes are rejected by the decoder, never repaired here.
            if pos < len(data) and data[pos] == 10:
                line += 1
            pos += 1
        elif byte == 10:
            line += 1
            if not quoted:
                yield ConversationRecord(start_line, begin, pos, True, False, data[begin:pos])
                begin, start_line = pos, line
    if begin < len(data):
        yield ConversationRecord(start_line, begin, len(data), False, quoted, data[begin:])


def _conversation_json(text):
    def invalid_constant(token):
        raise json.JSONDecodeError('Invalid JSON constant', text, text.index(token))
    return json.loads(text, strict=False, parse_constant=invalid_constant)


def _unfinished_json(text):
    """Use the decoder's first failure; only a truncated final token may extend.

    Numbers are special: the decoder accepts their complete leading part, so
    confirm that completing the suffix consumes it instead of hiding garbage.
    """
    try:
        _conversation_json(text)
    except json.JSONDecodeError as error:
        tail = text[error.pos:]
        if error.pos == len(text) or error.msg == 'Unterminated string starting at':
            return True
        if error.msg == 'Expecting value':
            return tail == '-' or any(word.startswith(tail) for word in ('true', 'false', 'null'))
        if error.msg == 'Invalid \\uXXXX escape':
            return re.fullmatch(r'u[0-9a-fA-F]{0,4}', tail) is not None
        if (error.msg in ('Extra data', "Expecting ',' delimiter")
                and re.fullmatch(r'(?:\.[0-9]*)?(?:[eE][+-]?)?', tail)):
            try:
                _conversation_json(text + '0')
            except json.JSONDecodeError as completed:
                return completed.pos == len(text) + 1
            return True
    return False


def conversation_rows_bytes(data, path):
    """One snapshot: (record's first physical line, original text, object).

    Raw U+0000..U+001F in strings are literal content. Only an incomplete EOF
    record waits for more bytes; every other syntax/UTF-8 failure stops reading.
    Auxiliary histories and transaction journals retain their strict readers.
    """
    rows = []
    for record in conversation_records(data):
        if not record.terminated and record.open_string:
            break
        raw = record.raw[:-1] if record.terminated else record.raw
        try:
            text = raw.decode('utf-8')
            value = _conversation_json(text)
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            if (not record.terminated and isinstance(error, json.JSONDecodeError)
                    and _unfinished_json(text)):
                break
            raise ConversationLogError(f'{path}:{record.number}: {type(error).__name__}: {error}') from None
        if not isinstance(value, dict):
            raise ConversationLogError(f'{path}:{record.number}: expected a JSON object')
        # Keep the established API: no terminating LF in the original-text slot.
        rows.append((record.number, text, value))
    return rows


def conversation_rows(path):
    return conversation_rows_bytes(Path(path).read_bytes(), path)


def run_cli(action):
    """Report the original low-level exception and stop the command."""
    try:
        action()
    except (TimeoutExpired, OSError, ValueError, KeyError, TypeError, AttributeError,
            RuntimeError, ImportError, SyntaxError) as error:
        detail = "; ".join([*getattr(error, "__notes__", ()), str(error)])
        sys.exit(f"{type(error).__name__}: {detail}")
