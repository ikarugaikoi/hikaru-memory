# 重复表达反馈

**中文** · [English](en/voice-feedback.md) · [日本語](ja/voice-feedback.md) · [首页](../README.md)

记忆可以持续影响回答，不必反复复述同一个昵称或原句。这套可选机制报告助手最近怎样使用表达，帮助模型决定照说、换一种表达、体现为选择，或暂时不说。它不自动改写回答，也不把重复判成错误。

## 三个入口

| 入口 | 触发时机 | 提供的内容 |
|---|---|---|
| A：开场反馈 | `mezame` 完成后 | 过去 7 天中，对象名字和别名出现于多少条助手回复、最近一次的时间；另列重复首尾句及固定说法 |
| B：当前上下文 | 用户发消息时 | 同一上下文近 3 小时内至少两条非字面呼应回复使用某对象别名时提醒；每个对象在该上下文提醒一次，压缩后可重新提醒 |
| C：受保护原话 | 用户发消息时 | 上一组助手回复出现已识别的受保护原话时附原则说明；按助手消息 UUID 去重，不依赖 B 的频率门槛 |

受保护原话来自累积经历中以 `（ねゆき）` 或 `（いけび）` 结尾的项目，提取其中至少 6 字的 `「…」` 引文。它们完整保留，但主动提起应有当前语境的理由。完整匹配的原话从 A/B 和口癖计数中遮去，按标记和 C 处理；自动联想递来的片段也可附这个标记。它们不因少说而失去保留资格。

## 数据和边界

- 当前理解采用现有条目格式：`### 条目名`，下一行 `- 別名: 别名一, 别名二`。只有符合此格式的条目进入对象计数。
- A 使用已归档原文，以及当前会话尚未归档的部分；其他会话当天未归档的内容不在这一版范围内。
- 当前排除范围会再次应用到存档。当前日志按口令和排除时段过滤，再合并 UUID；任一副本隐藏就不计数。现在处于不记录时段或无法确定边界时，不递送反馈。
- 一条回复命中同一对象多个别名只算一条。“用户近期也说过”只说明该别名字面出现在同一会话的前两条用户发言里，不证明助手是在接话。扣除这类回复后至少还剩两条，A/B 才提示。
- 常见词参照窗口之前的 60 天：某个别名出现在至少 5% 的助手回复里，该对象不进入 A/B。当前 7 天不参与这一判断。
- 口癖检查首尾句，延伸至至少 7 字；不足 7 字、问候以及受保护引文片段跳过。另有代码中的通用固定表达表。此处没有语义同义改写检测。
- 状态在实例的 `.local/state/tomonari/said.json`，仅保存 B 的键摘要、C 的消息 UUID 和通知时间，保留 3 天，不存正文。数据目录沿用 `HIKARU_DATA_ROOT`。

源码是 [反馈工具](../.tools/_tomonari_koe.py)及 [联想递送接口](../.tools/taguri_hook.py)。记录中的 `ふ`、`Claude` 是沿用的格式标记，不随显示名称改变。

## 接入

先完成[安装与本地配置](setup.md)。这次增加的部分是 Python，不需要为它重新编译 Haskell；已有实例仍须通过原有配置指纹检查。

1. 可将 [voice-config.example.json](../templates/voice-config.example.json) 复制到 `.local/config/voice.json`，在 `skip_entries` 中填写不希望计数的完整条目名，例如自己的称呼条目。未配置时只排除通用的 `ユーザー（呼び名）`。
2. 将 [hook 模板](../templates/hooks.voice.example.json) 中的绝对路径换成自己的仓库路径，合并到项目的 Claude Code hook 设置中，保留已有 hooks。模板不会自动安装，也不启用定时任务。
3. 开场独立执行 `python3 scripts/hikaru mezame`。PostToolUse 识别这一启动器，以及 `.tools/bin/mezame` / `.tools/bin/hikaru-core mezame`。不解析 `cd ... && ...`、管道等复合 shell 命令。
4. 将 [角色模板](../templates/CLAUDE.md) 的“表达反馈与摘要”约定补入自己的指令。不要为了接入功能覆盖已有角色文件。

可手动检查：

```bash
python3 scripts/hikaru _tomonari_koe --show-after-tool /path/to/authorized-session.jsonl
python3 scripts/hikaru _tomonari_koe --show-prompt /path/to/authorized-session.jsonl
```

这两个查看入口不保存通知状态。正常 hook 由宿主透传标准输入 JSON。删除两条 voice hooks 可停用 A/B/C；联想片段的保护标记是 `taguri_hook.py` 中的独立接入。

## 评估脚本

[voice_baseline.py](../scripts/voice_baseline.py)只统计已归档回复。私人的监测词表、真实基线数字和实验日期没有随代码发布；[示例词表](../templates/voice-cues.example.json)是虚构数据，需要换成自己的词表。前后两段使用同一份冻结词表，明确指定日期：

```bash
python3 scripts/voice_baseline.py --cues .local/config/voice-cues.json --since YYYY-MM-DD --until YYYY-MM-DD
```

日期采用 JST、05:00 日界，两端包含。跨期间起点仍查上一次出现；同时报告逐词累计命中次数和去重回复数，每百条的数是事件率，不是回复占比。`.タイムマシン/.last` 未到结束边界时，提示无法确认归档完整；没有某一天的文件不等于漏存。

## 验证与限制

```bash
python3 -B tests/voice_feedback.py
```

macOS 测试在临时搬迁副本中运行，禁止网络和真实用户目录读取，仅使用虚构记录与口令。涵盖 17 项原实现回归场景和 23 项接入、去重、压缩及评估边界检查。Python 测试使用合成配置指纹，不编译或运行 Haskell 核心，也不启动真实宿主。

计数正确不等于表达更自然。提醒本身可能再次激活相同措辞；精确匹配会漏掉改写；常见词过滤和问候规则也可能漏报。原始日志只读当前会话，归档会话标记存在短 ID 的原有局限。正式使用后仍需确认 hook 确实被宿主消费，并结合实际对话判断反馈是否有帮助。少量重复事件不足以支持稳定的效果百分比。
