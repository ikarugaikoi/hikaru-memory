# 安装与配置

**中文** · [English](en/setup.md) · [日本語](ja/setup.md) · [首页](../README.md)

## 环境与范围

当前验证：macOS arm64、GHC 9.10.3、Cabal、Python 3.12.12、Apple 开发工具。`icucore`、若干 C 兼容函数和沙盒使用 macOS 接口。其他平台需要移植，不能直接删掉沙盒来当作完成移植。

新机器尚未做完整安装验证。本地编译使用已有的锁定依赖库，但发布包不含这些缓存或二进制。

## 1. 配置自己的实例

```bash
cp settings.example.json settings.local.json
```

编辑本地 JSON：

| 字段 | 含义 |
|---|---|
| `display_name`、`assistant_name` | 生成角色模板中的称呼；核心格式仍用稳定角色标记 |
| `privacy_phrases` | 一组或多组“开始不记录／结束不记录”口令；必须自己填写。匹配整条用户输入，忽略指定标点 |
| `log_root` | 授权处理的 Claude Code 项目 JSONL 目录；留空使用本实例的空 `.local/logs` |
| `project_identity` | 日志 `cwd` 对应的项目路径；留空使用仓库绝对路径 |
| `weather.enabled` | 默认 `false`；开启时必须提供合法经纬度 |
| `backup` | restic 命令、仓库、密码读取命令、来源目录；未填齐不会运行 |

```bash
python3 scripts/configure.py
```

会生成 `.local/privacy.json` 和 `.tools/haskell-src/.generated/Hikaru/Local/Settings.hs`，并在没有旧记录时建立空记录和 `CLAUDE.md`。**口令和天气设置变更后需要重新编译**，因为核心使用编译时常量。配置指纹不一致时，Haskell 与 Python 两侧都会拒绝运行，要求先重新编译。生成的源码、二进制也包含这些本地配置，不要上传。

`settings.local.json`、`.local/`、生成源码、记忆与索引均被 `.gitignore` 排除。基础日期仍采用日本时间，日界为 05:00；配置中没有伪装成可用的时区选项。

## 2. 编译 Haskell

依赖冻结在 `.tools/haskell-src/cabal.project.freeze`。先安装 GHC 9.10.3 与 Cabal。在首次安装的机器上：

```bash
cd .tools/haskell-src
cabal update
cabal build --only-dependencies
cd ../..
```

上面会下载依赖。然后找到本机 GHC 9.10.3 对应的 Cabal package database，通常位于 `~/.cabal/store/ghc-9.10.3-*/package.db`，把真实路径传给：

```bash
python3 scripts/build.py --package-db /path/to/ghc-9.10.3-xxxx/package.db
```

构建脚本将日志和中间文件放入本仓库 `.build/`，编译子进程禁止网络、只允许写构建目录。产物为 `.tools/bin/hikaru-core`、`.tools/bin/hikaru-owntime`，并创建各命令的本地别名。不会安装定时任务或启动服务。

## 3. 基础检查与接入

```bash
python3 scripts/hikaru fuutou status
python3 scripts/hikaru fuutou show yuugao
python3 scripts/hikaru ochibo --dry-run
python3 scripts/hikaru omoide '例子' --mode lexical
```

先检查 dry-run 对应的日志范围，再决定正式归档。`log_root` 与 `project_identity` 必须对应同一个授权项目；读不到用户消息时先检查日志字段与身份筛选，不要放宽成扫描整个用户目录。

当前日志适配器面向 Claude Code，直接用户发言要求 `origin.kind == "human"`，并核对 `cwd`、类型和时间等字段。其他客户端需要新增适配，不是改扩展名就能导入。

宿主 hook 可自行配置调用 `python3 /absolute/path/scripts/hikaru ima`、`oshirase` 或 `taguri_hook`，按工具要求透传标准输入 JSON。这里不自动编辑你的全局 Claude 配置。先按客户端版本确认 hook 事件及输入格式，再启用。

## 4. 按需安装增强能力

- **混合检索**：在自己的 Python 环境中安装 `.tools/haskell-src/requirements-model-macos-arm64.lock` 对应依赖，模型权重另行下载。用同一个解释器运行 `scripts/hikaru _embed` 和 `scripts/hikaru omoide ... --mode hybrid`。这是原环境的锁定记录，不代表已验证所有新机器的下载可用性。
- **语言词典**：可选 fugashi 与 UniDic。词典缺失/超时时工具报告未完成该部分检查，仍处理可用的字符与退役词规则；不是声称已经完整检查。
- **一般词频**：`_yonotsune.py` 默认寻找 `~/.local/share/hikaru/wordfreq-venv/bin/python` 的隔离 wordfreq 环境，预计算结果存本实例目录；没有频率数据时不因缺数据而拦新别名。
- **自动联想递送**：先观察回放，再显式建立 `.tools/index/koyori/deliver.on`。删除该开关恢复观察模式。默认没有开启递送。
- **会话复盘参考材料**：[Biblia 文献导航](biblia.md)只提供问题与研究方向的对应关系，不附具体文献或完整分析规程。按需自行配置材料与复盘指令；`biblia` 初始化为空白读书记录，配置脚本不会自动导入这份导航。
- **自主活动**：需自行准备 Claude CLI、凭据、阅读材料与完整指令，填充 `templates/owntime.md` 的要求，再配置调度。当前代码仍依赖 Claude CLI；角色模板不等于已完成自主活动接入。
- **备份/媒体/手机位置/传感器**：都是独立可选项，见工具索引。

- **重复表达反馈**：手动合并 [hook 模板](../templates/hooks.voice.example.json)，并填写自己的排除项和评估词表。见[完整说明](voice-feedback.md)；不自动启用。

## 5. 合成验证

`tests/smoke.py` 使用临时副本、合成对话和禁止网络的沙盒。测试构建须使用公开的合成口令，不能拿编入私人口令的二进制当作测试配置；具体运行入口与范围见 [validation.md](validation.md)。

## 6. 准备分享

使用 `scripts/package.py` 按 `release-files.txt` 打包源码。它不收集 Git 忽略文件、不遍历真实记忆，也不把编译产物一起发布。每次增加公开文件时需更新清单并检查内容；别用整个工作目录的压缩替代这个步骤。
