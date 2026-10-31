"""Source-bound rule registry, not a scan of arbitrary Haskell strings.

Only the listed production definitions supply user-facing rules/instructions.
Tests, comments, identifiers and other definitions are not rule sources. Keep
the registry in sync when these definitions move; missing/unreadable sources
and unsupported literal syntax fail visibly instead of claiming full coverage.
"""
from bisect import bisect_right
import re

SOURCES = {
    "Fuutou/Text.hs": {"usage"},
    "Fuutou/Entry.hs": {"languageOptionMessage"},
    "Fuutou/Check.hs": {"oldMarkMessage", "review", "reviewAppend"},
    "Memory/FadeInstructions.hs": {"how"},
    "Memory/Validation.hs": {"correctionCheck"},
    "Memory/Fade.hs": {"guarded", "check", "report"},
    "Memory/Correction/Check.hs": {"checkIndex"},
    "Oshirase/Notice.hs": {"okaNote"},
}

# A limited lexer for ordinary Haskell string literals, comments and chars.
# String gaps and numeric escapes are Haskell syntax, not Python/JSON syntax.
SIMPLE = dict(zip("abfnrtv\\\"'", "\a\b\f\n\r\t\v\\\"'"))
ESCAPE = re.compile(r"(?:x[0-9a-fA-F]+|o[0-7]+|[0-9]+)")
CHAR = re.compile(r"'(?:[^'\\\n]|\\(?:[abfnrtv\\\"']|x[0-9a-fA-F]+|o[0-7]+|[0-9]+))'")
DECL = re.compile(r"^([a-z_][a-zA-Z0-9_']*)\b", re.M)


def literal(source, start):
    out, i = [], start + 1
    while i < len(source):
        c = source[i]
        i += 1
        if c == '"':
            return "".join(out), i
        if c != "\\":
            if c in "\r\n":
                raise ValueError("unescaped newline in Haskell rule string")
            out.append(c)
            continue
        if i == len(source):
            break
        c = source[i]
        if c in SIMPLE:
            out.append(SIMPLE[c])
            i += 1
        elif c == "&":
            i += 1
        elif c.isspace():
            while i < len(source) and source[i].isspace():
                i += 1
            if i == len(source) or source[i] != "\\":
                raise ValueError("unterminated Haskell string gap")
            i += 1
        elif m := ESCAPE.match(source, i):
            word = m[0]
            out.append(chr(int(word[1:], 16 if word[0] == "x" else 8) if word[0] in "xo" else int(word)))
            i = m.end()
        else:
            raise ValueError("unsupported Haskell rule string escape")
    raise ValueError("unterminated Haskell rule string")


def strings(source):
    """Return code with literals/comments blanked, and (offset, value) pairs."""
    code, values, i = list(source), [], 0

    def blank(a, b):
        code[a:b] = ["\n" if c == "\n" else " " for c in source[a:b]]

    while i < len(source):
        start = i
        if source.startswith("--", i):
            end = source.find("\n", i)
            i = len(source) if end < 0 else end
        elif source.startswith("{-", i):
            depth, i = 1, i + 2
            while depth and i < len(source):
                if source.startswith("{-", i):
                    depth, i = depth + 1, i + 2
                elif source.startswith("-}", i):
                    depth, i = depth - 1, i + 2
                else:
                    i += 1
            if depth:
                raise ValueError("unterminated Haskell comment")
        elif source[i] == '"':
            value, i = literal(source, i)
            values.append((start, value))
        elif m := CHAR.match(source, i):
            i = m.end()
        else:
            i += 1
            continue
        blank(start, i)
    return "".join(code), values


def rules(root):
    tree = root / ".tools/haskell-src"
    if not tree.exists():
        return []  # The older Python-only installation has no native sources.
    source_root = tree / "src/Hikaru"
    out = []
    for relative, names in SOURCES.items():
        path = source_root / relative
        source = path.read_text(encoding="utf-8")
        try:
            code, values = strings(source)
        except ValueError as e:
            raise ValueError(f"Haskell rule extraction incomplete: {relative}: {e}") from None
        declarations = [(m.start(), m[1]) for m in DECL.finditer(code)]
        missing = names - {name for _, name in declarations}
        if missing:
            raise ValueError(f"Haskell rule definitions missing: {relative}: {', '.join(sorted(missing))}")
        offsets = [pos for pos, _ in declarations]
        for pos, value in values:
            owner = bisect_right(offsets, pos) - 1
            if owner >= 0 and declarations[owner][1] in names:
                line = source.count("\n", 0, pos) + 1
                out.append((f"haskell-src/src/Hikaru/{relative}:{line}", value))
    return out
