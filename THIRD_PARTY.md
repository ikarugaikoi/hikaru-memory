# 第三方说明

**中文** · [English](THIRD_PARTY.en.md) · [日本語](THIRD_PARTY.ja.md)

本仓库采用 [MIT 协议](LICENSE)；第三方代码仍适用各自的原有许可。

- `third-party/CPython-LICENSE.txt`：兼容层涉及 CPython 行为或实现来源的部分；相关源文件中的归属注释保留。
- `third-party/MT19937.txt`：Mersenne Twister 相关版权与许可。
- `third-party/Unicode-Data.txt`：Unicode 数据使用许可。
- Haskell 依赖由 Cabal 按冻结版本安装，各包适用各自许可证，未把依赖源码或编译缓存装入发布包。
- Python 模型依赖和 BGE-M3/E5 模型权重分别适用各自许可证，未打包权重。
- 可选 `mediaremote-adapter`、restic、fugashi、UniDic、wordfreq、libusb、Claude CLI 均需自行安装；这里没有重分发它们。

本次发布不复制 lsby/ai-memory 的实现代码；其链接作为介绍和交流背景保留。
