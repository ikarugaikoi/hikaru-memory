# 工具索引

**中文** · [English](en/tools.md) · [日本語](ja/tools.md) · [首页](../README.md)

统一入口：`python3 scripts/hikaru TOOL ...`。该入口把日志、工作目录、索引 socket、位置状态和后台活动状态指向本安装。代码中的日文名称保留，下面按功能解释。

## 日常记忆工具

| 工具 | 用途 | 常用入口 |
|---|---|---|
| `mezame` | 开场读取并组装近期记录、当前理解、待办及修订注记 | `mezame`；`mezame --sazanami` 只看日历/近期概览 |
| `ima` | 当前时间与会话状态 hook；消费宿主输入 | 宿主 hook，JSON 从标准输入传入 |
| `oshirase` | 需要在当前会话提醒的记录状态、待处理事项 | 宿主 hook |
| `ochibo` | 从授权 JSONL 日志收集原文、合并重复、判定排除时段、保存图片 | `ochibo --dry-run`；确认范围后 `ochibo` |
| `shirushi` | 按日期扫描原文中的机械会话线索，不代替模型判断 | `shirushi YYYY-MM-DD YYYY-MM-DD --json` |
| `fuutou` | 受控记录读写、进度、修订查看、压缩计划 | 下表 |
| `choubo` | 使增量日志扫描状态失效，供修复/重建使用 | `choubo invalidate --all` 或 `--conversation 路径` |
| `koyomi` | 日本历法、季节和可选天气输入 | 天气默认关闭，坐标需自行配置 |
| `omoide` | 原文主动检索，支持字面、向量、混合与时间范围 | `omoide 查询 --mode lexical --top 8` |

**记录操作 `fuutou`**：

| 子命令 | 作用 |
|---|---|
| `show NAME` | 显示记录，可分页；不会因为读过就改写原文 |
| `open NAME` / `close NAME` | 打开临时草稿、保存版本基准；写回前验证版本及内容约束 |
| `reopen NAME` | 并发改动后重新对齐草稿与当前记录，保留自己的改动供核对 |
| `append NAME FILE` | 追加新段落，先进行语言检查，再走核心写入检查 |
| `diary DAY FILE` | 写入当日日记；检查日记结构与新文字 |
| `read DAY FILE` / `read DAY FILE --check` | 写入或仅检查当日会话复盘；不会走与 append 相同的语言扫描 |
| `mark [DAY]` | 生成机械线索并记录整理进度 |
| `judgments ariake:DAY` | 将已有判断按位置导出为结构化 JSON，供目录消费 |
| `misekechi [ID]` | 查看修订列表、具体修订及其结构问题 |
| `fade` | 输出经历压缩计划与保护项；不替模型生成压缩文字 |
| `prune` | 清理向量索引中原记录已不再存在的片段；不是删除原始记忆 |
| `koyori` | 重建/检查可派生目录 |
| `status` / `done ...` | 查询或登记日常整理进度 |
| `where` / `clean` | 查询工作目录；清理该目录内的临时草稿 |

`NAME` 包括 `oka`、`yuugao`、`strobe`、`biblia`、`shiori`、`ichifure`、`ichifure:DAY`、`tm:DAY`、`ariake:DAY`、`michikusa:DAY`、`omoigusa:NAME`。格式提示由工具输出；这些不是可随意翻译的参数。

## 检索、联想与反思

| 文件 / 入口 | 作用 |
|---|---|
| `_koyori.py` / `_koyori --rebuild` | 记录目录、机械线索与 CA 判决接线、经历保护映射、别名时间线和来源签名 |
| `_koyori_types.py` | 目录的 Entry、Chunk、Judgment、Footprint 等数据结构 |
| `_taguri.py` / `_taguri QUERY --timeline` | 沿命中别名找前一次经历；也保留候选搜索和回放入口 |
| `_taguri --footprints ...` | 按日期、材料、所在场景和阅读进度查足迹；明示条件用于筛选、偏好条件用于排序 |
| `taguri_hook.py` | 每轮尝试递一条共同经历；开关在 `.tools/index/koyori/deliver.on`，默认未创建 |
| `_embed.py` / `_embed` | 生成本地向量索引；可用 BGE-M3 或预设 E5 模型。首次运行可能下载模型并启动本地 worker |
| `_embed --status` / `--stop` / `--prune` | 检查/停止本安装的 worker，或清理失效向量 |
| `_yonotsune.py` | 为对象别名离线生成一般语言频率缓存；缺失词频不视为高频 |
| `_ayame.py` | 根据会话判断或活动足迹形成反思候选，并记录支持、反例、修订和再次想起；部分命令会调用模型 |
| `_misekechi.py` | Python 侧读取/标注修订的共享实现，不是另一个模型判断器 |

`omoide --mode hybrid` 合并 BM25 与语义结果；`--since`、`--until`、`--only` 限定检索范围，`--context` 控制相邻片段，`--explain` 显示各路分数。默认找原文，`--all` 扩展到其他记忆记录。

## 自主活动（可选）

入口为 `python3 scripts/hikaru owntime ...`，源码在 `Hikaru/OwnTime/`。

| 子命令 / 辅助工具 | 作用 |
|---|---|
| `status` / `now` | 查看后台状态、当前或最近做过的活动，不启动模型 |
| `talk-mark` | 宿主 hook：更新对话活动信号 |
| `presence` | 记录人工提供的在场/忙闲场景；`clear` 清除，`talk` 标记正在交流 |
| `check --dry-run` | 观察当前条件对应的状态机决定 |
| `run` | 执行活动调度，可能启动 Claude 子进程；需先完成权限、材料、凭据和指令配置 |
| `turn-end` | 保留的兼容入口，目前不做动作 |
| `owntime/megane.py` | 读取外部页面正文，帮助处理编码/页面提取；会访问网络 |
| `owntime/tameshi.py` | 在 macOS `sandbox-exec` 中运行活动目录内的小程序；限制网络、可读写路径、时间和输出 |

活动的 `landing.json`、足迹与作品结构由 `OwnTime/Records.hs`、`Michikusa.hs`、`Omoigusa.hs` 等模块解析。`templates/owntime.md` 是待完善的接入模板，不能冒充原部署已经调试过的完整自主代理指令。

## 语言、规则及维护

| 工具 / 文件 | 作用 |
|---|---|
| `_kotoba.py` | 写入前的最小语言检查：字形、配置过的退役写法、可选词典线索。可疑时暂停本次写入，等待修正或 `--lang-ok` |
| `rosetta.json` | 项目词汇、受保护标记与退役写法配置；发布版仅含最小通用示例 |
| `mizukagami.py` | 旧记录定点措辞修复：`prepare` 准备、`round2` 汇入二审、`apply` 有界应用、`ack` 确认；保留版本和保护项检查 |
| `tomonari.py` | 在角色指令、记录开头及工具规则间搜索相似规则，避免一条要求重复散落 |
| `_haskell_rules.py` | 从明确登记的 Haskell 定义中提取规则文字，供规则去重使用 |
| `kotozute.py` | 把尚待回应的问题在上下文压缩后接回来 |
| `kohaku.py` | 对使用者明确填写的目录执行 restic 备份与检查；空配置不能备份 |
| `_hk.py` | Python 共享的日期、路径、封装、锁、口令、记录与进度操作 |
| `_tool_data.py` | 共享文件边界、JSON/封装读取、错误处理和位置路径 |
| `_history.py` | 活动历史的结构读取与追加辅助 |

## 环境输入（全都可不接）

| 工具 | 作用与依赖 |
|---|---|
| `doko.py` | 将已知位置/在场事件格式化为一行；无配置则不构造个人位置 |
| `doko_recv.py` | 接收带令牌的 Wi-Fi 或 OwnTracks 事件；维护当前/上一次状态。需自行配置 Tailscale、地点表和设备，不随安装启动 |
| `kehai.py` | macOS 媒体播放状态，依赖另行安装的 `mediaremote-adapter` |
| `omron_read.py` | 特定 OMRON 环境传感器读取，依赖 libusb 和硬件；不是通用系统状态接口 |

## 安装与验证脚本

- `scripts/configure.py`：校验口令配置、生成 Python/Haskell 共用的本地值、建立空记录；不覆盖既有记录。
- `scripts/build.py`：从源码构建核心与 OwnTime，保留独立的 macOS 编译沙盒。
- `scripts/hikaru`：为工具设置本安装路径。
- `tests/smoke.py`：在临时安装目录使用合成输入验证关键链路。
- `scripts/package.py`：只按公开文件清单打包，排除本地生成的数据与二进制。

## 重复表达反馈（可选）

`python3 scripts/hikaru _tomonari_koe --prompt` 接用户输入 hook，`--after-tool` 接开场工具完成后的 hook。`scripts/voice_baseline.py` 按冻结词表统计归档中的表达复用。功能、数据边界与模板见[接入说明](voice-feedback.md)。
