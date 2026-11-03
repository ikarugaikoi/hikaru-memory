# Feedback on repeated expressions

[中文](../voice-feedback.md) · **English** · [日本語](../ja/voice-feedback.md) · [Home](../../README.en.md)

A memory can continue to guide a reply without its nickname or original wording being repeated. This optional module reports recent expression use. The model decides whether to reuse the wording, rephrase it, reflect the memory in a choice, or say nothing about it. The tool neither rewrites replies nor judges repetition to be wrong.

## Entry points

| Entry point | When it runs | Feedback |
|---|---|---|
| A: opening summary | After `mezame` | Object names and aliases used in assistant replies over the past seven days, counts and last use; repeated opening/closing sentences and stock expressions |
| B: current context | On a user message | Repeated object references within three hours of the current context; once per object and context, eligible again after compaction |
| C: protected quotations | On a user message | A reminder when the preceding assistant messages contain a recognized protected quotation; deduplicated by assistant message UUID, independent of B |

Protected quotations are strings of at least six characters inside `「…」` on experience entries ending in `（ねゆき）` or `（いけび）`. They remain preserved, while bringing them up unprompted should have a reason in the present conversation. Exact matches are masked before A/B and habitual-expression counts; protection markers and C handle them separately. Associative recall can attach the same marker to a retrieved passage.

## Counting and storage

Objects use the existing Markdown structure: `### Object name`, immediately followed by `- 別名: alias one, alias two`. A reads eligible archives plus the unarchived part of the current session. Unarchived messages in other sessions are not included.

Current exclusion intervals are reapplied to archives. Live messages pass privacy-phrase and time-range filtering before UUID deduplication; a hidden copy makes the merged message hidden. No feedback is delivered during an unrecorded interval or when its boundary cannot be established.

One reply counts once per object, even if it contains several aliases. Literal overlap with either of the preceding two user messages is counted separately; it does not prove that the assistant was echoing the user. A/B require at least two remaining replies. An alias appearing in at least 5% of assistant replies in the 60 days **before** the seven-day window excludes its object from A/B. The current window does not determine that filter.

Habitual-expression feedback compares exact opening/closing sentences extended to at least seven characters, with exclusions for short replies, greetings and protected quotation fragments. Stock expressions also have a small code-defined list. Semantic paraphrases are not detected.

Only B key hashes, C message UUIDs and notification times are stored in `.local/state/tomonari/said.json`, retained for three days. No message bodies are stored there. Data paths follow `HIKARU_DATA_ROOT`. The archive role labels `ふ` and `Claude` remain format identifiers rather than configurable display names.

## Integration

Complete the existing [local setup](setup.md) first. This update adds Python code and does not itself require a Haskell rebuild; existing configuration fingerprint checks still apply.

1. Optionally copy [voice-config.example.json](../../templates/voice-config.example.json) to `.local/config/voice.json`. Set `skip_entries` to exact object headings to exclude, such as your own name entry. The default excludes only `ユーザー（呼び名）`.
2. Replace the absolute-path placeholder in the [hook template](../../templates/hooks.voice.example.json), then merge its two entries into your project hooks without overwriting existing ones. Nothing is installed automatically.
3. Run `python3 scripts/hikaru mezame` as a standalone command. The hook also recognizes `.tools/bin/mezame` and `.tools/bin/hikaru-core mezame`; compound shell commands and pipelines are not parsed.
4. Incorporate the expression-feedback and summary guidance from [the role template](../../templates/CLAUDE.md) into your own instructions. Do not replace your existing persona file wholesale.

For a read-only preview:

```bash
python3 scripts/hikaru _tomonari_koe --show-after-tool /path/to/authorized-session.jsonl
python3 scripts/hikaru _tomonari_koe --show-prompt /path/to/authorized-session.jsonl
```

These preview commands do not save notification state. Normal hooks receive the host's JSON on stdin. Removing the two voice hooks disables A/B/C; the protection marker in `taguri_hook.py` is a separate integration.

## Evaluation and validation

The [evaluation script](../../scripts/voice_baseline.py) reads archived replies and requires your own frozen JSON cue list. [Example cues](../../templates/voice-cues.example.json) are invented; no original private vocabulary, measurements or study dates are included.

```bash
python3 scripts/voice_baseline.py --cues .local/config/voice-cues.json --since YYYY-MM-DD --until YYYY-MM-DD
python3 -B tests/voice_feedback.py
```

Dates are inclusive JST days beginning at 05:00. Earlier occurrences are considered across the start boundary. The report separates per-expression hits from distinct reply counts; events per hundred replies are not the percentage of replies. If the archive watermark `.タイムマシン/.last` has not reached the end boundary, completeness remains unconfirmed. A missing daily file alone is not evidence of missing data.

The macOS sandbox tests use a relocated temporary installation, synthetic records and synthetic privacy phrases, with network access and real user-directory reads denied. They cover 17 original regression cases and 23 integration/evaluation checks. They use a synthetic Python configuration fingerprint and do not build Haskell or launch a real host session.

Correct counts do not establish more natural dialogue. Feedback may prime the expression again; exact matching misses paraphrases, and greeting/common-word filters can miss useful cases. Archived short session IDs retain their existing collision limitation. Confirm real hook delivery separately and assess actual conversations; a few repetition events cannot support a stable improvement percentage.
