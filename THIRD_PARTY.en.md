# Third-party notes

[中文](THIRD_PARTY.md) · **English** · [日本語](THIRD_PARTY.ja.md) · [Home](README.en.md)

This repository uses the [MIT license](LICENSE). Third-party code retains its respective licenses.

- `third-party/CPython-LICENSE.txt`: parts of the compatibility layer based on CPython behavior or implementation; attribution comments remain in the relevant source files.
- `third-party/MT19937.txt`: Mersenne Twister copyright and license.
- `third-party/Unicode-Data.txt`: Unicode data license.
- Haskell dependencies are installed by Cabal at pinned versions, under each package's license. Dependency source and build caches are not bundled.
- Python model dependencies and BGE-M3/E5 weights have their own licenses; weights are not included.
- Optional `mediaremote-adapter`, restic, fugashi, UniDic, wordfreq, libusb, and Claude CLI must be installed separately and are not redistributed here.

This release does not copy implementation code from lsby/ai-memory. The link acknowledges its influence on the project introduction and the discussions behind it. Original license texts are retained without translation or alteration.
