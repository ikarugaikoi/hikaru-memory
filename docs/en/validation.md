# Local release validation

[中文](../validation.md) · **English** · [日本語](../ja/validation.md) · [Home](../../README.en.md)

October 31, 2026. macOS arm64, GHC 9.10.3, Python 3.12.12.

## Executed checks

A fresh installation in a temporary path containing spaces was created with synthetic configuration and conversations. Both `hikaru-core` and `hikaru-owntime` were rebuilt against an existing read-only database of pinned dependencies. The compilation sandbox denied network access and allowed writes only to that installation's build directory. Functional checks also ran without network access.

**17 checks passed:**

1. Initialize an empty instance.
2. Rebuild both executables in the relocated synthetic instance.
3. Query core status.
4. Read current-understanding records.
5. Append through language checking and read back.
6. Block suspicious wording without changing record bytes.
7. Reject path traversal in record names.
8. Reject stale drafts after concurrent changes.
9. Clean temporary drafts.
10. Generate a compression plan for synthetic records.
11. Dry-run archiving of synthetic JSONL.
12. Archive public synthetic messages while excluding synthetic private messages inside phrase-delimited intervals.
13. Load Python configuration, check phrase boundaries, import helpers.
14. Search synthetic transcripts lexically.
15. Rebuild the catalog without starting an embedding model.
16. Query OwnTime state without starting a model.
17. Reject configuration/build mismatches on both Haskell and Python sides.

All Python source was also parsed; local documentation links and the release manifest were checked. Public files and the archive were inspected for private paths, original phrases, account markers, coordinates, and excluded file types.

Implementation documentation added later was checked against existing source without changing runtime code. Equations and parameter descriptions do not imply new effectiveness experiments. The manifest, links, and archive contents were checked again after the documentation additions.

## Reproducing the checks

Prepare the pinned dependencies for GHC 9.10.3, then run:

```bash
python3 tests/smoke.py --package-db /path/to/ghc-9.10.3-xxxx/package.db
```

The entry point generates synthetic phrases and compiles in a temporary copy, leaving the current instance's configuration and memories unchanged. A development directory already using synthetic configuration may reuse its binaries with `python3 tests/smoke.py`; ordinary users should pass `--package-db`.

## Not executed in this release validation

- Full dependency installation and model downloads on a fresh machine.
- BGE-M3 inference and its worker; only lexical retrieval and catalog paths were verified here.
- Archiving, reading, or rewriting real chats or memories.
- Claude autonomous activity with real credentials, phone location, weather requests, USB sensors, media state, or restic backups.
- Historical benchmark reruns, full performance comparisons, or persona evaluation.

These checks validate the listed execution paths in an isolated copy of the release; they do not establish that every integration works in a full deployment. It is the historical validation record, not a claim that documentation translation reran those checks.
