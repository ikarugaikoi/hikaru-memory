"""Catalog construction values. JSON field order is fixed at this boundary."""
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any, Literal

BASE_IMPORTANCE = 0.2


@dataclass(frozen=True)
class MemoryPaths:
    root: Path
    index: Path
    annotations: Path


@dataclass(frozen=True)
class JudgmentInput:
    judgment: str
    section: str | None
    hitotoki: str | None
    shirushi: str | None
    line: Any
    reread: bool

    @classmethod
    def from_json(cls, row):
        return cls(row["judgment"], row.get("section"), row.get("hitotoki"), row.get("shirushi"),
                   row.get("line"), bool(row.get("reread")))

    def text(self):
        return f"### {self.section or ''}\n判：{self.judgment}"


@dataclass
class KeepReport:
    day: str
    mark: str
    line: int
    quotes: int
    hand: int
    exact: int
    as_strobe: bool = False


@dataclass(frozen=True)
class Chunk:
    file: str
    i: int


@dataclass(frozen=True)
class Judgment:
    file: str
    line: Any
    at_end: str | None
    shirushi: str | None


@dataclass(frozen=True)
class Footprint:
    file: str
    koma: int


@dataclass
class Entry:
    source: Chunk | Judgment | Footprint
    sha: str
    kind: Literal["utterance", "heading", "strobe", "judgment", "footprint"]
    certainty: Literal["verbatim", "transcribed", "unsure", "reread", "asserted", "self_report"]
    who: str | None = None
    day: str | None = None
    at: str | None = None
    sid: str | None = None
    hitotoki: str | None = None
    scene: Literal["with_user", "alone"] | None = None
    importance: float = BASE_IMPORTANCE
    imp_why: list[str] = field(default_factory=list)
    things: list[str] | None = None
    ca: dict[str, str] | None = None

    def to_json(self):
        source = self.source
        row = {"file": source.file, "i": source.i if isinstance(source, Chunk) else None}
        if isinstance(source, Judgment):
            row["line"] = source.line
        elif isinstance(source, Footprint):
            row["koma"] = source.koma
        row.update(sha=self.sha, kind=self.kind, certainty=self.certainty, who=self.who, day=self.day, at=self.at)
        if isinstance(source, Judgment):
            row["at_end"] = source.at_end
        row.update(sid=self.sid, hitotoki=self.hitotoki, scene=self.scene)
        if isinstance(source, Judgment):
            row["shirushi"] = source.shirushi
        row.update(importance=self.importance, imp_why=self.imp_why)
        if self.things is not None:
            row["things"] = self.things
        if self.ca is not None:
            row["ca"] = self.ca
        return row


@dataclass
class BuildResult:
    entries: list[Entry]
    keeps: list[KeepReport]
    ca: dict[str, int]
    things: dict[str, dict]
    things_report: dict

    def to_json(self):
        return {"entries": [entry.to_json() for entry in self.entries], "keeps": [asdict(k) for k in self.keeps],
                "ca": self.ca, "things": self.things, "things_report": self.things_report}
