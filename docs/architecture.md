# 数据结构与详细流程

## 谁负责什么

宿主模型负责理解、取舍与表达；工具负责保存、定位、检查已明确的结构边界。系统没有一个能判断所有语义正确性的总评分。机械线索、CA 判断、检索相关性、反思证据和压缩保护是不同的数据。

## 一次新经历怎样留下来

```mermaid
flowchart TD
    Log[宿主 JSONL 日志] --> Parse[解析用户与助手发言\n分清直接输入和工具转述]
    Parse --> Merge[按会话来源合并与去重]
    Merge --> Exclusion[不记录口令状态机\n显式排除范围]
    Exclusion -->|可归档| Archive[按 HouseDay 存原文\n05:00 为日界]
    Exclusion -->|排除或未结束区间| Omit[不进入本次归档]
    Archive --> Signals[shirushi 机械线索]
    Signals --> Judgment[模型读相邻话轮\n记录判、再读与不确定]
    Judgment --> Diary[按日摘要]
    Judgment --> Current[对象条目与当前理解]
    Judgment --> Episodes[值得留下的经历]
    Diary --> Input[新段落或编辑草稿]
    Current --> Input
    Episodes --> Input
    Input --> Lang[append / diary 的语言检查]
    Lang -->|无提示或本次已复核| Guards[版本、结构与保留项检查]
    Lang -->|可疑写法| Return[返回位置，暂不写入]
    Return --> Input
    Guards --> Commit[备份、封装与原子写入]
    Commit --> Rebuild[目录与索引按来源更新]
```

`HouseDay` 是按日本时间早上 5 点切分的日子。现在这是实现约定，不是 JSON 中可随意调整的选项。不记录口令匹配整条符合条件的用户输入；并不等于从客户端、供应商或其他备份中删除消息。

### 主要记录格式

以下都是合成的结构示意，字段名称有语法意义，不能仅为美观翻译后写回。

**当前理解条目**：标题与别名让检索找到对象，说明和原话保存模型理解及其依据，修订属于条目内部。

```markdown
## もの
### 例子的对象
- 別名：青い箱、収納箱
- 説明：机の横に置く箱。
- 日付：2026-10-30
```

**累积经历**：`## むかし`、`## すこし前`、`## このごろ` 是三个层次，内部按日期组织。`（ねゆき）` 与 `（いけび）` 是不同来源的保留标记。具体保留、追加与压缩约束由 `Memory/Strobe.hs`、`FadePolicy.hs`、`Store/EditPlan.hs` 处理。

**会话复盘**：`## ひととき` 标出会话块，`###` 标出位置，`判：`、`再読：`、`しるし：` 保存初判、再读和机械线索的复核。实际的判断是模型写的；工具负责读取这些已存在的结论，避免判错的线索仍在评分中生效。

**可重建目录**：记录来源、日、会话、说话者、片段位置和内容指纹；对象条目关联其别名出现过的原文位置。目录不是新的事实来源，也不是把所有关联当作因果关系的知识图谱。

## 三种召回怎样汇合

```mermaid
flowchart LR
    Begin[会话开始] --> Wake[mezame]
    Wake --> Context[当前上下文]
    Need[模型发现缺少依据] --> Query[omoide 查询与范围]
    Query --> BM25[字面检索]
    Query --> Vector[向量检索]
    BM25 --> RRF[RRF 排名合并]
    Vector --> RRF
    RRF --> Window[原文与相邻窗口\n日期、来源、修订注记]
    Window --> Context
    Prompt[当前用户发言] --> Alias[匹配对象别名]
    Alias --> Gate[排除歧义、泛词\n频率与成团程度]
    Gate --> Recent[排除最近已谈对象\n定位前一次会话]
    Recent --> Small[最多一条短原文\n谁说的、何时、命中什么]
    Small --> Switch{递送开关}
    Switch -->|关| Shadow[只观察]
    Switch -->|开| Context
    Context --> Model[模型核对并决定是否使用]
    Model -->|仍缺依据| Query
    Model --> Reply[本次回应]
```

字面或向量相似不代表“增量用途”已经成立。“前一次提到某个词”也不保证就是同一件东西。系统给模型保留核对对象、时间和语境的机会。

成团分数以天为文档，观察字串频数和覆盖天数，并用语料内的参照分布校准。一般词频不明或出现很少的别名，按现有保守策略处理。它只影响自动联想入口；阈值来自原部署实验，不保证换一个人的语料仍然最优。

## 修订与淡化

```mermaid
flowchart TD
    Evidence[新对话 / 新资料 / 反例] --> Interpret[模型判断变化的性质]
    Interpret --> Revision[误读 / 变化 / 冲突 / 条件差异]
    Revision --> Fields[修订 ID、前一版、依据\n范围、旧说法位置]
    Fields --> Check[语法与引用检查]
    Check --> Notes[写入当前理解]
    Notes --> Readback[开场与检索读到登记过的旧说法]
    Readback --> Annotate[原文旁加修订注记]
    Episodes[累积经历] --> Plan[容量、时间、保护项计算]
    Notes --> Protect[仍有效修订的保护范围]
    Protect --> Plan
    Plan --> Summarize[模型压缩符合条件的部分]
    Summarize --> Commit[写回时再次检查保护约束]
    Raw[原始对话] --> Evidence
```

写入保护不等于全部文件永久不可改。累积经历的追加、正常修订、语言定点修复及压缩各有入口；`--force` 不是日常语言提示的通用绕过参数。语言复核的 `--lang-ok` 只作用于该次语言检查，不能跳过核心版本或内容保护。

## 自主活动与反思

```mermaid
flowchart TD
    Trigger[可选时段与在场信号] --> State[OwnTime 状态机]
    State --> Value[兴趣 / 熟悉 / 饱足 / 负担\n计算候选价值]
    Value --> Select[Softmax 候选提议\n来源平衡与活动预算]
    Select --> Agent[独立工具调用过程]
    Agent --> Report[报告兴趣 / 负担 / 继续意愿]
    Report --> Continue[价值比较与泄漏累积\n继续 / 重选 / 退出]
    Continue --> State
    Agent --> Read[读资料 / 搜索]
    Agent --> Make[写作 / 小程序试验]
    Read --> Landing[landing.json 与 footprint.md]
    Make --> Landing
    Landing --> Validate[记录实际行为、材料进度\n核对活动结果]
    Validate --> Footsteps[按日足迹与作品]
    Footsteps --> Me[关于自己活动的反思]
    CA[有后续回应依据的会话判断] --> Us[关于互动的反思]
    Me --> Roots[证据根去重\n派生内容不算新证据]
    Us --> Roots
    Roots --> Candidate[可被反例修订的候选]
    Candidate --> Search[按需读回]
```

这部分同时含有确定性的活动状态机和模型判断。`_ayame.py` 的来源分类、候选生成会调用外部模型；空白模板不自动复制原部署的角色、阅读计划与调度。完整字段定义见 `OwnTime/Reflection.hs`、`Michikusa.hs`、`Records.hs`、`Types.hs` 及 `Tokimori.hs`。

活动价值、候选抽样、继续/退出累积，以及检索、成团筛选和证据计数，详见[数学模型与参数](mechanisms.md)。
