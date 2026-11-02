# Data structures and detailed flow

[中文](../architecture.md) · **English** · [日本語](../ja/architecture.md) · [Home](../../README.en.md)

## Responsibilities

The host model interprets evidence, selects material, and generates responses. Tools store and retrieve records and enforce explicit structural constraints. There is no universal score for semantic correctness. Mechanical cues, CA judgments, retrieval relevance, reflection evidence, and compression protections are separate kinds of data.

## How a new experience is recorded

```mermaid
flowchart TD
    Log[Host JSONL logs] --> Parse[Parse user and assistant messages\nSeparate direct input from tool quotations]
    Parse --> Merge[Merge and deduplicate by session source]
    Merge --> Exclusion[No-record phrase state machine\nExplicit exclusions]
    Exclusion -->|archivable| Archive[Archive by HouseDay\n05:00 boundary]
    Exclusion -->|excluded or still-open interval| Omit[Omit from this archive]
    Archive --> Signals[shirushi mechanical cues]
    Signals --> Judgment[Read adjacent turns\nRecord judgments and uncertainty]
    Judgment --> Diary[Daily summary]
    Judgment --> Current[Entity entries and current understanding]
    Judgment --> Episodes[Experiences to retain]
    Diary --> Input[New paragraph or draft]
    Current --> Input
    Episodes --> Input
    Input --> Lang[append / diary language check]
    Lang -->|clear or reviewed for this call| Guards[Version, structure, and retention checks]
    Lang -->|suspicious form| Return[Return locations without writing]
    Return --> Input
    Guards --> Commit[Back up, Base64-encode, and write atomically]
    Commit --> Rebuild[Update catalog and indexes by source]
```

`HouseDay` divides days at 05:00 Japan time. This is an implementation convention, not a freely configurable JSON option. No-record phrases match whole qualifying user inputs; they do not delete messages from the client, provider, or other backups.

### Main record formats

These are synthetic structural examples. Field names are part of the record format; do not translate them when writing records.

**Current-understanding entry:** headings and aliases locate the entity; descriptions and quotations hold interpretation and evidence; revisions belong inside the entry.

```markdown
## もの
### Example entity
- 別名：青い箱、収納箱
- 説明：机の横に置く箱。
- 日付：2026-10-30
```

**Accumulated experiences:** `## むかし`, `## すこし前`, and `## このごろ` are the three layers, organized internally by date. `（ねゆき）` and `（いけび）` are retention markers with different origins. `Memory/Strobe.hs`, `FadePolicy.hs`, and `Store/EditPlan.hs` enforce retention, append, and compression constraints.

**Conversation review:** `## ひととき` marks a session block, `###` a position, and `判：`, `再読：`, `しるし：` hold initial judgments, rereadings, and cue validation. The model writes the judgments; tools consume them so that rejected cues no longer affect scoring.

**Rebuildable catalog:** stores source, day, session, speaker, passage position, and content fingerprint. Entity entries link to alias occurrences in transcripts. The catalog is neither a new factual source nor a knowledge graph treating every association as causation.

## How the three recall paths meet

```mermaid
flowchart LR
    Begin[Session starts] --> Wake[mezame]
    Wake --> Context[Current context]
    Need[Model needs more evidence] --> Query[omoide query and scope]
    Query --> BM25[Lexical search]
    Query --> Vector[Vector search]
    BM25 --> RRF[RRF rank fusion]
    Vector --> RRF
    RRF --> Window[Transcript and adjacent passages\nDates, sources, revision notes]
    Window --> Context
    Prompt[Current user utterance] --> Alias[Match entity aliases]
    Alias --> Gate[Exclude ambiguous or generic triggers\nFrequency and clumping]
    Gate --> Recent[Exclude recently discussed entities\nLocate previous session]
    Recent --> Small[At most one short excerpt\nSpeaker, date, matched alias]
    Small --> Switch{Delivery switch}
    Switch -->|off| Shadow[Observe only]
    Switch -->|on| Context
    Context --> Model[Model checks and decides whether to use]
    Model -->|still needs evidence| Query
    Model --> Reply[Current response]
```

Lexical or semantic similarity does not establish incremental usefulness. The previous occurrence of a word does not guarantee the same referent. The model can check the entity, time, and context before using a passage.

Clumping treats days as documents, relates string frequency to days covered, and calibrates against the corpus distribution. Low-frequency aliases and aliases with unknown general-language frequency are not excluded by this filter. The score only controls automatic triggers; thresholds explored in one deployment need not be optimal for another person's corpus.

## Revision and memory compression

```mermaid
flowchart TD
    Evidence[New conversation / material / counterexample] --> Interpret[Model judges the kind of change]
    Interpret --> Revision[Misreading / change / conflict / conditions]
    Revision --> Fields[Revision ID, predecessor, evidence\nScope and earlier statement locations]
    Fields --> Check[Syntax and reference checks]
    Check --> Notes[Write current understanding]
    Notes --> Readback[Load earlier statements linked to revisions]
    Readback --> Annotate[Attach revision notes to original text]
    Episodes[Accumulated experiences] --> Plan[Capacity, age, and protection calculation]
    Notes --> Protect[Protection scope of applicable revisions]
    Protect --> Plan
    Plan --> Summarize[Model compresses eligible content]
    Summarize --> Commit[Recheck protections on write]
    Raw[Original transcript] --> Evidence
```

Write protection does not make every file permanently immutable. Appending experiences, normal revision, targeted language repairs, and compression have distinct entry points. `--force` is not a general bypass for routine language warnings. `--lang-ok` acknowledges only that call's language check; it cannot bypass core version or content protection.

## Autonomous activity and reflection

```mermaid
flowchart TD
    Trigger[Optional schedule and presence signals] --> State[OwnTime state machine]
    State --> Value[Interest / familiarity / satiation / effort\nCalculate candidate value]
    Value --> Select[Softmax proposal\nSource balance and activity budget]
    Select --> Agent[Separate tool-using agent loop]
    Agent --> Report[Report interest / effort / willingness]
    Report --> Continue[Value comparison and leaky accumulation\nContinue / reselect / exit]
    Continue --> State
    Agent --> Read[Read / search]
    Agent --> Make[Write / small program experiments]
    Read --> Landing[landing.json and footprint.md]
    Make --> Landing
    Landing --> Validate[Record actual actions and progress\nVerify activity outcome]
    Validate --> Footsteps[Daily activity records and works]
    Footsteps --> Me[Reflection on own activities]
    CA[Conversation judgments with subsequent-response evidence] --> Us[Reflection on interaction]
    Me --> Roots[Deduplicate evidence by original source\nDerived content is not new support]
    Us --> Roots
    Roots --> Candidate[Candidates open to revision from counterexamples]
    Candidate --> Search[Read back as needed]
```

This combines a deterministic activity state machine with model judgments. Source classification and candidate generation in `_ayame.py` call an external model. Blank templates do not reproduce the original deployment's persona, reading plan, or schedule. Field definitions are in `OwnTime/Reflection.hs`, `Michikusa.hs`, `Records.hs`, `Types.hs`, and `Tokimori.hs`.

See [models and parameters](mechanisms.md) for candidate values, sampling, continuation/exit accumulation, retrieval, clumping, and evidence counts.
