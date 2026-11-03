# ツール一覧

[中文](../tools.md) · [English](../en/tools.md) · **日本語** · [トップ](../../README.ja.md)

共通の実行コマンドは `python3 scripts/hikaru TOOL ...` です。このランチャーが、ログ、作業ディレクトリ、索引用ソケット、位置情報、バックグラウンド活動の状態を、そのインスタンスの保存先に設定します。コード内の名前は変えず、ここでは機能を説明します。

## 日常の記憶ツール

| ツール | 用途 | 主な使い方 |
|---|---|---|
| `mezame` | 最近の記録、現在の理解、未完了事項、理解の更新注記を会話開始時のコンテキストにまとめる | `mezame`。`mezame --sazanami` は暦と最近の記録の概要のみ |
| `ima` | 現在時刻とセッション状態を扱うフック。ホスト入力を処理 | 標準入力で JSON を受け取るホストのフック |
| `oshirase` | 今のセッションで知らせる記録状態や未処理事項 | ホストのフック |
| `ochibo` | 許可された JSONL から原文を集め、重複を除き、除外区間を判定して画像を保存 | `ochibo --dry-run`。範囲確認後に `ochibo` |
| `shirushi` | 日付を指定して原文から機械的な会話の手がかりを探す。モデル判断の代わりではない | `shirushi YYYY-MM-DD YYYY-MM-DD --json` |
| `fuutou` | 保護条件を検査したうえでの記録の読み書き、進捗、更新確認、圧縮計画 | 下表 |
| `choubo` | 修復・再構築のため、増分ログ走査の状態を無効化 | `choubo invalidate --all` または `--conversation PATH` |
| `koyomi` | 日本の暦、季節、任意の天気入力 | 天気は既定で無効。座標は自分で設定 |
| `omoide` | 原文の能動検索。字句・ベクトル・ハイブリッド・日付範囲 | `omoide QUERY --mode lexical --top 8` |

**記録操作 `fuutou`**

| サブコマンド | 動作 |
|---|---|
| `show NAME` | ページを指定して記録を表示。読むだけでは原文を変えない |
| `open NAME` / `close NAME` | 編集用の一時ファイルを作成し、編集元のバージョンを保存。書き戻し前にバージョンと内容制約を確認 |
| `reopen NAME` | 他の処理による更新後、草稿を最新の記録に合わせ直す。自分の変更は確認できる形で残す |
| `append NAME FILE` | 言語チェック後、コアの書き込み検査を経て段落を追加 |
| `diary DAY FILE` | 指定日の日記を書く。構造と新しい文章を検査 |
| `read DAY FILE` / `read DAY FILE --check` | 会話の振り返りを書く、または検査だけ行う。append と同じ言語チェックは行わない |
| `mark [DAY]` | 機械的な手がかりを生成し、整理の進捗を記録 |
| `judgments ariake:DAY` | 保存済みの判断を位置ごとの構造化 JSON としてカタログに渡す |
| `misekechi [ID]` | 理解の更新履歴、個別の更新内容、構造上の問題を表示 |
| `fade` | 経験の圧縮計画と保護対象を出力。要約文そのものは生成しない |
| `prune` | 元の記録がなくなったベクトル索引の項目を削除する。記憶の原本は削除しない |
| `koyori` | 派生カタログの再構築・確認 |
| `status` / `done ...` | 日次整理の進捗確認・登録 |
| `where` / `clean` | 作業ディレクトリの確認、その中の一時草稿の削除 |

`NAME` は `oka`、`yuugao`、`strobe`、`biblia`、`shiori`、`ichifure`、`ichifure:DAY`、`tm:DAY`、`ariake:DAY`、`michikusa:DAY`、`omoigusa:NAME` などです。ツールが書式を案内します。引数の名前を翻訳して渡さないでください。

## 検索・連想・内省

| ファイル / 入口 | 動作 |
|---|---|
| `_koyori.py` / `_koyori --rebuild` | 記録カタログ、機械的手がかりと CA 判断の接続、経験と保護対象の対応付け、別名ごとの時系列、出典の識別情報 |
| `_koyori_types.py` | `Entry`、`Chunk`、`Judgment`、`Footprint` などのカタログ型 |
| `_taguri.py` / `_taguri QUERY --timeline` | 一致した別名から前回の経験を探す。候補検索とリプレイの入口も残る |
| `_taguri --footprints ...` | 日付、資料、場面、読書進捗で活動記録を検索。明示された条件で絞り込み、希望条件で順位付けする |
| `taguri_hook.py` | 毎ターン、共有した経験を最大1件提示。スイッチは `.tools/index/koyori/deliver.on` で、既定では存在しない |
| `_embed.py` / `_embed` | BGE-M3 または事前設定された E5 モデルでローカル索引を生成。初回はモデル取得やローカルワーカー 起動の可能性あり |
| `_embed --status` / `--stop` / `--prune` | このインスタンスのワーカー の確認・停止、無効になったベクトルの削除 |
| `_yonotsune.py` | 対象の別名について、一般的な使用頻度を事前計算して保存。頻度不明を高頻度とは扱わない |
| `_ayame.py` | 会話判断や活動から内省候補を作り、支持・反例・更新・再浮上を記録。一部コマンドはモデルを呼び出す |
| `_misekechi.py` | Python 側で理解の更新履歴を読み、注記を付ける共通実装。独自に意味を判断するモデルではない |

`omoide --mode hybrid` は BM25 と意味検索を統合します。`--since`、`--until`、`--only` で範囲、`--context` で隣接区間を指定し、`--explain` で各経路の点数を表示します。既定は原文検索、`--all` は他の記憶記録も対象にします。

## 自主活動（任意）

入口は `python3 scripts/hikaru owntime ...`、ソースは `Hikaru/OwnTime/` です。

| サブコマンド / 補助ツール | 動作 |
|---|---|
| `status` / `now` | モデルを起動せず、バックグラウンドの状態と現在・直近の活動を表示 |
| `talk-mark` | 会話活動の信号を更新するホストのフック |
| `presence` | 手動で入力した在席状況や忙しさを記録。`clear` は消去、`talk` は会話中の印 |
| `check --dry-run` | 現在の条件で状態機械がどの動作を選ぶかを確認 |
| `run` | 活動をスケジュールし、Claude 子プロセスを起動する可能性がある。権限・資料・認証・指示の設定が先 |
| `turn-end` | 互換用の入口。現在は何もしない |
| `owntime/megane.py` | 外部ページの本文を抽出し、文字コードなどを処理。ネットワークを使う |
| `owntime/tameshi.py` | 活動ディレクトリ内の小規模プログラムを macOS `sandbox-exec` で実行。ネットワーク、パス、時間、出力を制限 |

`landing.json`、活動記録、作品の構造は `OwnTime/Records.hs`、`Michikusa.hs`、`Omoigusa.hs` などが解析します。[テンプレート](../../templates/ja/owntime.md)は利用者による追記が必要で、元の運用環境で調整済みの完全な指示ではありません。

## 言語・規則・保守

| ツール / ファイル | 動作 |
|---|---|
| `_kotoba.py` | 書き込み前の最小言語チェック。字形、使用をやめた表現のリスト、任意の辞書による検出結果を確認。疑わしい場合は修正または `--lang-ok` まで書き込みを止める |
| `rosetta.json` | プロジェクト語彙、保護マーカー、使用をやめた表現。公開版は最小限の汎用例のみ |
| `mizukagami.py` | 既存記録の該当箇所に限定した表現の修正。`prepare`、再確認の結果を取り込む `round2`、範囲を限定する `apply`、確認の `ack`。バージョンと保護条件の検査は保持 |
| `tomonari.py` | ペルソナ指示、記録冒頭、ツール規則の類似を検索し、同じ規則の重複を減らす |
| `_haskell_rules.py` | 明示的に登録された Haskell 定義から規則文を抽出し、重複検出に使う |
| `kotozute.py` | コンテキスト圧縮後に未回答の問いを引き継ぐ |
| `kohaku.py` | 明示的に設定したディレクトリの restic バックアップと確認。空設定では実行不可 |
| `_hk.py` | Python 共通の日付、パス、Base64 エンコード、ロック、合言葉、記録、進捗操作 |
| `_tool_data.py` | 共通のファイルへのアクセス範囲、JSON や Base64 エンコードされたデータの読み込み、エラー処理、位置情報パス |
| `_history.py` | 活動履歴の構造的な読み込み・追記補助 |

## 環境入力（すべて省略可能）

| ツール | 動作と依存 |
|---|---|
| `doko.py` | 取得済みの位置情報や在席イベントを1行に整形する。未設定なら位置情報を生成しない |
| `doko_recv.py` | トークンで認証された Wi-Fi / OwnTracks イベントを受信し、現在・前回の状態を管理。Tailscale、場所の対応表、端末は自分で設定し、導入時には起動しない |
| `kehai.py` | macOS のメディア再生状態。別途 `mediaremote-adapter` が必要 |
| `omron_read.py` | 特定の OMRON 環境センサー用。libusb と実機が必要で、汎用状態取得 API ではない |

## 導入・検証スクリプト

- `scripts/configure.py`：合言葉を検証し、Python/Haskell 共通のローカル値と空記録を作る。既存記録は上書きしない。
- `scripts/build.py`：独立した macOS コンパイル用サンドボックスでコアと OwnTime をビルド。
- `scripts/hikaru`：このインストール先のパスをツールに渡す。
- `tests/smoke.py`：一時インストールに合成入力を与え、主要経路を検証。
- `scripts/package.py`：公開許可リストにあるファイルだけをアーカイブに含め、ローカル生成データとバイナリを除外。

## 表現フィードバック（任意）

`python3 scripts/hikaru _tomonari_koe --prompt` はユーザー入力時、`--after-tool` は開始時ツールの終了後に呼ぶ hook です。`scripts/voice_baseline.py` は固定した語句リストで保存済みの発話を集計します。[接続方法と対象範囲](voice-feedback.md)を参照してください。
