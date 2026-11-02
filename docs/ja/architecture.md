# データ構造と詳細フロー

[中文](../architecture.md) · [English](../en/architecture.md) · **日本語** · [トップ](../../README.ja.md)

## 役割分担

ホストモデルは理解、取捨選択、表現を担当し、ツールは保存、位置の特定、明示された構造的境界の検査を担当します。意味の正しさをすべて判定する総合点はありません。機械的な手がかり、CA 判断、検索関連度、内省の証拠、圧縮保護は別々のデータです。

## 新しい経験が残るまで

```mermaid
flowchart TD
    Log[ホストの JSONL ログ] --> Parse[ユーザー・助手の発話を解析\n直接入力とツール経由の引用を区別]
    Parse --> Merge[セッションの出典ごとに統合・重複排除]
    Merge --> Exclusion[不記録の合言葉の状態機械\n明示的な除外範囲]
    Exclusion -->|保存可能| Archive[HouseDay ごとに原文保存\n日界は05:00]
    Exclusion -->|除外・未終了区間| Omit[今回の保存から除外]
    Archive --> Signals[shirushi の機械的手がかり]
    Signals --> Judgment[隣接ターンを読み\n判断・再読・不確定を記録]
    Judgment --> Diary[日別の要約]
    Judgment --> Current[対象項目と現在の理解]
    Judgment --> Episodes[残す経験]
    Diary --> Input[新しい段落・編集草稿]
    Current --> Input
    Episodes --> Input
    Input --> Lang[append / diary の言語検査]
    Lang -->|問題なし・今回確認済み| Guards[版・構造・保護対象の検査]
    Lang -->|疑わしい表記| Return[位置を返して書き込みを保留]
    Return --> Input
    Guards --> Commit[バックアップ・包装・原子的書き込み]
    Commit --> Rebuild[出典ごとに目録と索引を更新]
```

`HouseDay` は日本時間の05:00で区切る日です。現在は実装上の約束であり、JSON で任意変更する項目ではありません。不記録の合言葉は条件を満たすユーザー入力全体に一致させます。クライアント、提供事業者、他のバックアップからメッセージを削除する機能ではありません。

### 主な記録形式

以下は合成した構造例です。フィールド名は構文なので、見た目のために翻訳して書き戻さないでください。

**現在の理解の項目**：見出しと別名が対象を見つける入口になり、説明と原話が理解と根拠を持ちます。修訂は項目の内部に置きます。

```markdown
## もの
### 例の対象
- 別名：青い箱、収納箱
- 説明：机の横に置く箱。
- 日付：2026-10-30
```

**蓄積した経験**：`## むかし`、`## すこし前`、`## このごろ` が3層で、内部は日付で整理します。`（ねゆき）` と `（いけび）` は出所の違う保留マーカーです。保留・追記・圧縮の制約は `Memory/Strobe.hs`、`FadePolicy.hs`、`Store/EditPlan.hs` が扱います。

**会話の振り返り**：`## ひととき` がセッションのブロック、`###` が位置を示し、`判：`、`再読：`、`しるし：` が初判・再読・手がかりの確認を保存します。判断を書くのはモデルです。ツールは既存判断を読み、誤りと判定した手がかりが採点に残らないようにします。

**再構築可能な目録**：出典、日、セッション、話者、区間位置、内容指紋を記録し、対象項目を別名の出現位置と結びます。目録は新たな事実の出典でも、すべての関連を因果と扱う知識グラフでもありません。

## 3つの想起経路の合流

```mermaid
flowchart LR
    Begin[会話開始] --> Wake[mezame]
    Wake --> Context[現在のコンテキスト]
    Need[モデルが根拠不足を認識] --> Query[omoide のクエリと範囲]
    Query --> BM25[字面検索]
    Query --> Vector[ベクトル検索]
    BM25 --> RRF[RRF による順位統合]
    Vector --> RRF
    RRF --> Window[原文と隣接区間\n日付・出典・修訂注記]
    Window --> Context
    Prompt[現在のユーザー発話] --> Alias[対象別名との一致]
    Alias --> Gate[曖昧・一般的な入口を除外\n頻度と集中度]
    Gate --> Recent[最近話した対象を除外\n前回のセッションを探す]
    Recent --> Small[短い原文を最大1件\n話者・日付・一致した別名]
    Small --> Switch{提示スイッチ}
    Switch -->|無効| Shadow[観察のみ]
    Switch -->|有効| Context
    Context --> Model[モデルが照合し利用を判断]
    Model -->|まだ根拠不足| Query
    Model --> Reply[今回の応答]
```

字面や意味の類似は、現在の会話に追加の役割を持つことの証明ではありません。「前回この語が出た」だけでは同じ対象とも限りません。モデルが対象、時刻、文脈を確かめる余地を残します。

集中度は日を文書単位とし、文字列の頻度と出現日の数を、コーパス内の基準分布で補正します。一般頻度不明や低頻度の別名は現行の保守的な扱いに従います。自動連想の入口だけに作用し、元の運用環境で選んだ閾値が他の人の記録でも最適とは限りません。

## 修訂と淡化

```mermaid
flowchart TD
    Evidence[新しい会話・資料・反例] --> Interpret[変化の種類をモデルが判断]
    Interpret --> Revision[読み違い・変化・衝突・条件差]
    Revision --> Fields[修訂 ID・前版・根拠\n範囲・旧記述の位置]
    Fields --> Check[文法と参照の検査]
    Check --> Notes[現在の理解に保存]
    Notes --> Readback[開始時や検索で登録済みの旧記述を読む]
    Readback --> Annotate[原文に修訂注記を付ける]
    Episodes[蓄積した経験] --> Plan[容量・日付・保護対象を計算]
    Notes --> Protect[有効な修訂の保護範囲]
    Protect --> Plan
    Plan --> Summarize[モデルが対象部分を圧縮]
    Summarize --> Commit[書き戻し時に保護制約を再確認]
    Raw[会話原文] --> Evidence
```

書き込み保護は、全ファイルを永久に不変にすることではありません。経験の追加、通常の修訂、局所的な表記修復、圧縮には別の入口があります。`--force` は日常の言語警告を一律に迂回する引数ではありません。`--lang-ok` はその呼び出しの言語確認だけに作用し、コアの版検査や内容保護は飛ばせません。

## 自主活動と内省

```mermaid
flowchart TD
    Trigger[任意の時間帯・在場信号] --> State[OwnTime 状態機械]
    State --> Value[関心・親しみ・飽和・負担\n候補価値を計算]
    Value --> Select[Softmax による候補提案\n出典調整と活動予算]
    Select --> Agent[独立したツール使用ループ]
    Agent --> Report[関心・負担・継続意向を報告]
    Report --> Continue[価値比較と漏れのある蓄積\n継続・再選択・終了]
    Continue --> State
    Agent --> Read[資料を読む・検索]
    Agent --> Make[執筆・小プログラム実験]
    Read --> Landing[landing.json と footprint.md]
    Make --> Landing
    Landing --> Validate[実行内容と進捗を記録\n活動結果を確認]
    Validate --> Footsteps[日別の足跡と作品]
    Footsteps --> Me[自分の活動についての内省]
    CA[後続応答の根拠を持つ会話判断] --> Us[相互行為についての内省]
    Me --> Roots[証拠の根を重複排除\n派生内容は新たな支持にしない]
    Us --> Roots
    Roots --> Candidate[反例によって修訂できる候補]
    Candidate --> Search[必要に応じて読み返す]
```

決定的な活動状態機械とモデル判断を組み合わせています。`_ayame.py` の出典分類や候補生成は外部モデルを呼びます。空テンプレートに元の人格、読書計画、スケジュールが再現されるわけではありません。完全なフィールド定義は `OwnTime/Reflection.hs`、`Michikusa.hs`、`Records.hs`、`Types.hs`、`Tokimori.hs` を参照してください。

活動価値、抽選、継続・終了の蓄積、検索、集中度、証拠の計数は[数理モデルとパラメータ](mechanisms.md)にまとめています。
