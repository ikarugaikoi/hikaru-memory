# Tool index

[中文](../tools.md) · **English** · [日本語](../ja/tools.md) · [Home](../../README.en.md)

Common entry point: `python3 scripts/hikaru TOOL ...`. It directs logs, working directories, index sockets, location state, and background-activity state to this installation. Japanese command names remain unchanged; their functions are explained below.

## Everyday memory tools

| Tool | Purpose | Common usage |
|---|---|---|
| `mezame` | Assemble recent records, current understanding, pending matters, and revision notes at session start | `mezame`; `mezame --sazanami` shows only the calendar/recent overview |
| `ima` | Current time and session-state hook; consumes host input | Host hook, JSON on standard input |
| `oshirase` | Record status and pending matters requiring attention in the current session | Host hook |
| `ochibo` | Collect transcripts from authorized JSONL logs, merge duplicates, determine excluded intervals, save images | `ochibo --dry-run`, then `ochibo` after checking scope |
| `shirushi` | Scan dated transcripts for mechanical conversation cues; does not replace model judgment | `shirushi YYYY-MM-DD YYYY-MM-DD --json` |
| `fuutou` | Controlled record access, progress, revisions, and compression plans | See below |
| `choubo` | Invalidate incremental log-scanning state for repair/rebuilding | `choubo invalidate --all` or `--conversation PATH` |
| `koyomi` | Japanese calendar, seasons, optional weather | Weather off by default; configure coordinates yourself |
| `omoide` | Active transcript retrieval: lexical, vector, hybrid, date ranges | `omoide QUERY --mode lexical --top 8` |

**Record operations: `fuutou`**

| Subcommand | Function |
|---|---|
| `show NAME` | Display a record, optionally paginated; reading does not rewrite it |
| `open NAME` / `close NAME` | Open a temporary draft and retain a version baseline; validate version and content constraints before saving |
| `reopen NAME` | Realign a draft with a concurrently changed record, retaining local edits for review |
| `append NAME FILE` | Append a paragraph after language checks and core write checks |
| `diary DAY FILE` | Write a day's diary; validate structure and new text |
| `read DAY FILE` / `read DAY FILE --check` | Write or only validate a conversation review; does not run the same language scan as append |
| `mark [DAY]` | Generate mechanical cues and record consolidation progress |
| `judgments ariake:DAY` | Export existing judgments by position as structured JSON for the catalog |
| `misekechi [ID]` | Show revisions, a specific revision, and structural issues |
| `fade` | Produce an experience-compression plan and protections; does not generate the compressed prose |
| `prune` | Remove vector-index entries whose original records no longer exist; does not delete original memories |
| `koyori` | Rebuild/check the derived catalog |
| `status` / `done ...` | Inspect or register daily consolidation progress |
| `where` / `clean` | Locate the working directory; clean its temporary drafts |

`NAME` includes `oka`, `yuugao`, `strobe`, `biblia`, `shiori`, `ichifure`, `ichifure:DAY`, `tm:DAY`, `ariake:DAY`, `michikusa:DAY`, and `omoigusa:NAME`. The tools supply format guidance. These are literal parameters, not names to translate.

## Retrieval, association, and reflection

| File / entry | Function |
|---|---|
| `_koyori.py` / `_koyori --rebuild` | Catalog, integration of mechanical cues with CA judgments, experience-protection mappings, alias timelines, source signatures |
| `_koyori_types.py` | `Entry`, `Chunk`, `Judgment`, `Footprint`, and other catalog structures |
| `_taguri.py` / `_taguri QUERY --timeline` | Find the previous experience through a matched alias; also retains candidate-search and replay paths |
| `_taguri --footprints ...` | Search activity records by dates, material, setting, and reading progress; explicit conditions filter, preferences rank |
| `taguri_hook.py` | Attempt to deliver one shared experience per turn; `.tools/index/koyori/deliver.on` enables it and is absent by default |
| `_embed.py` / `_embed` | Build local vector indexes with BGE-M3 or preset E5 models; first run may download weights and start a local worker |
| `_embed --status` / `--stop` / `--prune` | Inspect/stop this installation's worker or remove stale vectors |
| `_yonotsune.py` | Precompute general-language frequency for aliases; missing values do not count as high frequency |
| `_ayame.py` | Form reflection candidates from conversation judgments/activity records; track support, counterexamples, revisions, and resurfacing. Some commands call a model |
| `_misekechi.py` | Shared Python implementation for reading/annotating revisions, not another semantic judge |

`omoide --mode hybrid` merges BM25 and semantic results. `--since`, `--until`, and `--only` constrain scope; `--context` controls adjacent passages; `--explain` shows component scores. The default searches transcripts; `--all` includes other memory records.

## Autonomous activity (optional)

Entry: `python3 scripts/hikaru owntime ...`. Source: `Hikaru/OwnTime/`.

| Subcommand / helper | Function |
|---|---|
| `status` / `now` | Inspect background state and current/recent activity without launching a model |
| `talk-mark` | Host hook updating the conversation-activity signal |
| `presence` | Record manually supplied presence/busy context; `clear` removes it, `talk` marks conversation |
| `check --dry-run` | Inspect the state-machine decision under current conditions |
| `run` | Run activity scheduling; may launch Claude subprocesses. Configure permissions, material, credentials, and instructions first |
| `turn-end` | Retained compatibility entry; currently does nothing |
| `owntime/megane.py` | Extract external page text and handle encoding; accesses the network |
| `owntime/tameshi.py` | Run small programs from the activity directory inside macOS `sandbox-exec`, limiting network, paths, time, and output |

`landing.json`, activity records, and works are parsed by `OwnTime/Records.hs`, `Michikusa.hs`, `Omoigusa.hs`, and related modules. [The template](../../templates/en/owntime.md) requires completion; it is not the original deployment's fully configured autonomous-agent instructions.

## Language, rules, and maintenance

| Tool / file | Function |
|---|---|
| `_kotoba.py` | Minimal pre-write language check: character forms, configured retired expressions, optional dictionary cues. Suspicious text pauses the write until corrected or acknowledged with `--lang-ok` |
| `rosetta.json` | Project vocabulary, protected markers, retired expressions; the release contains only minimal generic examples |
| `mizukagami.py` | Targeted wording repairs in existing records: `prepare`, `round2` to import second-review results, bounded `apply`, and `ack`; retains version/protection checks |
| `tomonari.py` | Find similar rules across persona instructions, record preambles, and tool rules to reduce duplication |
| `_haskell_rules.py` | Extract rule text from explicitly registered Haskell definitions for rule deduplication |
| `kotozute.py` | Restore unanswered questions after context compaction |
| `kohaku.py` | restic backup/checks for explicitly configured directories; blank settings cannot back up |
| `_hk.py` | Shared Python date, path, wrapping, locking, phrase, record, and progress operations |
| `_tool_data.py` | Shared file boundaries, JSON/wrapped reads, error handling, location paths |
| `_history.py` | Structured activity-history reading and append helpers |

## Environmental inputs (all optional)

| Tool | Function and dependencies |
|---|---|
| `doko.py` | Format known location/presence events as one line; no personal location is fabricated without configuration |
| `doko_recv.py` | Receive token-authenticated Wi-Fi/OwnTracks events; maintain current/previous state. Configure Tailscale, place mappings, and devices yourself; installation does not start it |
| `kehai.py` | macOS media playback state; requires separately installed `mediaremote-adapter` |
| `omron_read.py` | Read a specific OMRON environmental sensor using libusb and hardware; not a general system-state interface |

## Installation and validation scripts

- `scripts/configure.py`: validate privacy phrases, generate shared Python/Haskell local values, create empty records without overwriting existing ones.
- `scripts/build.py`: build core and OwnTime with a separate macOS compilation sandbox.
- `scripts/hikaru`: set this installation's paths for tools.
- `tests/smoke.py`: validate key paths with synthetic inputs in a temporary installation.
- `scripts/package.py`: package only allowlisted public files, excluding generated local data and binaries.
