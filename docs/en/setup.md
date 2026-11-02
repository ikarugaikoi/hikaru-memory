# Setup and configuration

[中文](../setup.md) · **English** · [日本語](../ja/setup.md) · [Home](../../README.en.md)

## Environment and scope

Validated with macOS arm64, GHC 9.10.3, Cabal, Python 3.12.12, and Apple developer tools. `icucore`, several C compatibility functions, and sandboxing use macOS APIs. Other platforms require porting; removing the sandbox is not a completed port.

A full installation on a fresh machine has not been verified. Local builds used an existing pinned dependency database; dependency caches and binaries are not included in the release.

## 1. Configure your instance

```bash
cp settings.example.json settings.local.json
```

Edit the local JSON:

| Field | Meaning |
|---|---|
| `display_name`, `assistant_name` | Names used in generated host instructions; core formats retain stable speaker markers |
| `privacy_phrases` | One or more start/end pairs for no-record intervals. Supply your own. Matches whole user messages after specified punctuation is ignored |
| `log_root` | Authorized Claude Code project JSONL directory; blank uses this instance's empty `.local/logs` |
| `project_identity` | Project path matched against log `cwd`; blank uses the absolute repository path |
| `weather.enabled` | Defaults to `false`; valid latitude/longitude are required when enabled |
| `backup` | restic executable, repository, password-reading command, and source directories; incomplete configuration does not run |

```bash
python3 scripts/configure.py
```

This generates `.local/privacy.json` and `.tools/haskell-src/.generated/Hikaru/Local/Settings.hs`, and creates empty records and `CLAUDE.md` where they do not already exist. **Recompile after changing privacy phrases or weather settings**, because the core uses compile-time constants. A configuration fingerprint mismatch causes both Haskell and Python to refuse execution until rebuilt. Generated source and binaries contain local configuration; do not publish them.

`settings.local.json`, `.local/`, generated source, memories, and indexes are excluded by `.gitignore`. Dates still use Japan time with a 05:00 boundary. There is no configurable timezone option.

### Using the translated templates

The configuration script still reads `templates/CLAUDE.md` and `templates/daily-review.md`. It does not automatically select a language. After initialization, use [templates/en/CLAUDE.md](../../templates/en/CLAUDE.md) as the basis for your local `CLAUDE.md`, replacing `{{assistant_name}}` and `{{display_name}}`. Adapt [the daily-review template](../../templates/en/daily-review.md) into `.local/daily-review.md` as needed. Preserve any instructions you have already added; do not blindly overwrite an existing file. Command names, record markers, and runtime output remain unchanged.

## 2. Compile Haskell

Dependencies are pinned in `.tools/haskell-src/cabal.project.freeze`. Install GHC 9.10.3 and Cabal first. On a fresh machine:

```bash
cd .tools/haskell-src
cabal update
cabal build --only-dependencies
cd ../..
```

These commands download dependencies. Locate the Cabal package database for GHC 9.10.3, usually under `~/.cabal/store/ghc-9.10.3-*/package.db`, then pass its actual path:

```bash
python3 scripts/build.py --package-db /path/to/ghc-9.10.3-xxxx/package.db
```

The build script puts logs and intermediate files in `.build/`. Compiler subprocesses cannot access the network and may write only to the build directory. It produces `.tools/bin/hikaru-core` and `.tools/bin/hikaru-owntime` and creates local command aliases. It neither installs scheduled jobs nor starts services.

## 3. Basic checks and integration

```bash
python3 scripts/hikaru fuutou status
python3 scripts/hikaru fuutou show yuugao
python3 scripts/hikaru ochibo --dry-run
python3 scripts/hikaru omoide 'example' --mode lexical
```

Inspect the dry-run log scope before archiving. `log_root` and `project_identity` must identify the same authorized project. If user messages are missing, inspect log fields and identity filters rather than broadening the search to the entire home directory.

The current adapter targets Claude Code. Direct user messages require `origin.kind == "human"`; it also checks `cwd`, type, time, and other fields. Other clients need adapters, not merely a different extension.

Host hooks can call `python3 /absolute/path/scripts/hikaru ima`, `oshirase`, or `taguri_hook`, passing the required JSON through standard input. Global Claude settings are not edited automatically. Check hook events and input formats for your client version before enabling them.

## 4. Optional capabilities

- **Hybrid retrieval:** install the dependencies in `.tools/haskell-src/requirements-model-macos-arm64.lock` into your Python environment; download weights separately. Use the same interpreter for `scripts/hikaru _embed` and `scripts/hikaru omoide ... --mode hybrid`. This lock records the original environment, not a verified fresh-machine installation.
- **Language dictionary:** optional fugashi and UniDic. If unavailable or timed out, the tool reports that this part of the check did not finish, while applying available character and retired-expression rules. It does not claim a complete check.
- **General frequency:** `_yonotsune.py` looks by default for an isolated wordfreq environment at `~/.local/share/hikaru/wordfreq-venv/bin/python`; precomputed results are stored within the instance. Missing frequency data does not by itself block a new alias.
- **Automatic association delivery:** observe replays first, then explicitly create `.tools/index/koyori/deliver.on`. Removing it returns to observation-only mode. Delivery is off by default.
- **Conversation-review literature:** [Biblia](biblia.md) maps questions to research directions without supplying specific papers or a full procedure. Configure your own material and instructions. The `biblia` record starts empty; configuration does not import this guide.
- **Autonomous activity:** supply Claude CLI, credentials, reading material, and complete instructions; fulfill [the OwnTime template](../../templates/en/owntime.md), then configure scheduling. The code still depends on Claude CLI. A persona template alone does not complete this integration.
- **Backup, media, phone location, sensors:** independent optional integrations; see [tools](tools.md).

## 5. Synthetic validation

`tests/smoke.py` uses a temporary copy, synthetic conversations, and a network-denying sandbox. Test builds must use public synthetic phrases, not binaries containing private phrases. See [validation](validation.md) for commands and scope.

## 6. Sharing

`scripts/package.py` packages source according to `release-files.txt`. It does not collect ignored files, traverse real memories, or include build outputs. Add new public files to the manifest and review their contents; do not substitute a zip of the entire working directory.
