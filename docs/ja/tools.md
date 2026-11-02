# ツール一覧

[中文](../tools.md) · [English](../en/tools.md) · **日本語** · [トップ](../../README.ja.md)

共通の入口は `python3 scripts/hikaru TOOL ...` です。この入口がログ、作業ディレクトリ、索引 socket、位置状態、バックグラウンド活動状態をこのインストール先に向けます。コード内の名前は変えず、ここでは機能を説明します。

## 日常の記憶ツール

| ツール | 用途 | 主な使い方 |
|---|---|---|
| `mezame` | 最近の記録、現在の理解、未完了事項、修訂注記を開始時に組み立てる | `mezame`。`mezame --sazanami` は暦と最近の概観のみ |
| `ima` | 現在時刻とセッション状態の hook。ホスト入力を処理 | 標準入力の JSON を受けるホスト hook |
| `oshirase` | 今のセッションで知らせる記録状態や未処理事項 | ホスト hook |
| `ochibo` | 許可された JSONL から原文を集め、重複を統合し、除外区間を判定して画像を保存 | `ochibo --dry-run`。範囲確認後に `ochibo` |
| `shirushi` | 日付を指定して原文から機械的な会話の手がかりを探す。モデル判断の代わりではない | `shirushi YYYY-MM-DD YYYY-MM-DD --json` |
| `fuutou` | 制御された記録の読み書き、進捗、修訂確認、圧縮計画 | 下表 |
| `choubo` | 修復・再構築のため、増分ログ走査の状態を無効化 | `choubo invalidate --all` または `--conversation PATH` |
| `koyomi` | 日本の暦、季節、任意の天気入力 | 天気は既定で無効。座標は自分で設定 |
| `omoide` | 原文の能動検索。字面・ベクトル・ハイブリッド・日付範囲 | `omoide QUERY --mode lexical --top 8` |

**記録操作 `fuutou`**

| サブコマンド | 動作 |
|---|---|
| `show NAME` | ページ指定可能な記録表示。読むだけでは原文を変えない |
| `open NAME` / `close NAME` | 一時草稿を開き、版の基準を保存。書き戻し前に版と内容制約を確認 |
| `reopen NAME` | 同時変更後の草稿を現行記録に合わせ直し、自分の変更を確認用に残す |
| `append NAME FILE` | 言語チェック後、コアの書き込み検査を経て段落を追加 |
| `diary DAY FILE` | 当日の日記を書く。構造と新しい文章を検査 |
| `read DAY FILE` / `read DAY FILE --check` | 会話の振り返りを書く、または検査だけ行う。append と同じ言語走査は通らない |
| `mark [DAY]` | 機械的な手がかりを生成し、整理の進捗を記録 |
| `judgments ariake:DAY` | 既存判断を位置ごとの構造化 JSON として目録に渡す |
| `misekechi [ID]` | 修訂一覧、個別修訂、構造上の問題を表示 |
| `fade` | 経験の圧縮計画と保護対象を出力。要約文そのものは生成しない |
| `prune` | 元の記録が存在しなくなったベクトル索引の区間を清掃。原記憶は削除しない |
| `koyori` | 派生目録の再構築・確認 |
| `status` / `done ...` | 日常整理の進捗確認・登録 |
| `where` / `clean` | 作業ディレクトリの確認、その中の一時草稿の清掃 |

`NAME` は `oka`、`yuugao`、`strobe`、`biblia`、`shiori`、`ichifure`、`ichifure:DAY`、`tm:DAY`、`ariake:DAY`、`michikusa:DAY`、`omoigusa:NAME` などです。ツールが書式を案内します。引数の名前を翻訳して渡さないでください。

## 検索・連想・内省

| ファイル / 入口 | 動作 |
|---|---|
| `_koyori.py` / `_koyori --rebuild` | 記録目録、機械的手がかりと CA 判断の接続、経験の保護対応、別名時系列、出典署名 |
| `_koyori_types.py` | `Entry`、`Chunk`、`Judgment`、`Footprint` などの目録型 |
| `_taguri.py` / `_taguri QUERY --timeline` | 一致した別名から前回の経験を探す。候補検索とリプレイの入口も残る |
| `_taguri --footprints ...` | 日付、資料、場面、読書進捗で活動の足跡を検索。明示条件は絞り込み、希望条件は順位付け |
| `taguri_hook.py` | 毎ターン共同経験を最大1件提示。スイッチは `.tools/index/koyori/deliver.on` で、既定では存在しない |
| `_embed.py` / `_embed` | BGE-M3 または既定の E5 でローカル索引を生成。初回はモデル取得やローカル worker 起動の可能性あり |
| `_embed --status` / `--stop` / `--prune` | このインストールの worker の確認・停止、無効なベクトルの清掃 |
| `_yonotsune.py` | 対象別名の一般言語頻度キャッシュを事前生成。不明を高頻度扱いしない |
| `_ayame.py` | 会話判断や活動から内省候補を作り、支持・反例・修訂・再浮上を記録。一部コマンドはモデルを呼ぶ |
| `_misekechi.py` | Python 側で修訂を読み注記する共通実装。別の意味判断モデルではない |

`omoide --mode hybrid` は BM25 と意味検索を統合します。`--since`、`--until`、`--only` で範囲、`--context` で隣接区間を指定し、`--explain` で各経路の点数を表示します。既定は原文検索、`--all` は他の記憶記録も対象にします。

## 自主活動（任意）

入口は `python3 scripts/hikaru owntime ...`、ソースは `Hikaru/OwnTime/` です。

| サブコマンド / 補助ツール | 動作 |
|---|---|
| `status` / `now` | モデルを起動せず、背景状態と現在・直近の活動を表示 |
| `talk-mark` | 会話活動の信号を更新するホスト hook |
| `presence` | 人が与えた在場・忙しさの場面を記録。`clear` は消去、`talk` は会話中の印 |
| `check --dry-run` | 現在の条件での状態機械の判断を観察 |
| `run` | 活動をスケジュールし、Claude 子プロセスを起動する可能性がある。権限・資料・認証・指示の設定が先 |
| `turn-end` | 互換用の入口。現在は何もしない |
| `owntime/megane.py` | 外部ページの本文を抽出し、文字コードなどを処理。ネットワークを使う |
| `owntime/tameshi.py` | 活動ディレクトリ内の小プログラムを macOS `sandbox-exec` で実行。ネットワーク、パス、時間、出力を制限 |

`landing.json`、足跡、作品の構造は `OwnTime/Records.hs`、`Michikusa.hs`、`Omoigusa.hs` などが解析します。[テンプレート](../../templates/ja/owntime.md)は補完が必要で、元の運用環境で調整済みの完全な指示ではありません。

## 言語・規則・保守

| ツール / ファイル | 動作 |
|---|---|
| `_kotoba.py` | 書き込み前の最小言語チェック。字形、設定済みの退役表現、任意の辞書手がかりを確認。疑わしい場合は修正または `--lang-ok` まで書き込みを止める |
| `rosetta.json` | プロジェクト語彙、保護マーカー、退役表現。公開版は最小限の汎用例のみ |
| `mizukagami.py` | 既存記録の局所的な表現修復。`prepare`、二次審査取り込みの `round2`、範囲を限定する `apply`、確認の `ack`。版と保護条件の検査は保持 |
| `tomonari.py` | 人格指示、記録冒頭、ツール規則の類似を検索し、同じ規則の重複を減らす |
| `_haskell_rules.py` | 明示登録された Haskell 定義から規則文を抽出し、重複検出に使う |
| `kotozute.py` | コンテキスト圧縮後に未回答の問いを引き継ぐ |
| `kohaku.py` | 明示設定したディレクトリの restic バックアップと確認。空設定では実行不可 |
| `_hk.py` | Python 共通の日付、パス、包装、ロック、合言葉、記録、進捗操作 |
| `_tool_data.py` | 共通のファイル境界、JSON・包装データ読み込み、エラー処理、位置情報パス |
| `_history.py` | 活動履歴の構造的な読み込み・追記補助 |

## 環境入力（すべて省略可能）

| ツール | 動作と依存 |
|---|---|
| `doko.py` | 既知の位置・在場イベントを1行に整える。設定なしで個人の位置を作らない |
| `doko_recv.py` | トークン付き Wi-Fi / OwnTracks イベントを受信し、現在・前回の状態を管理。Tailscale、場所表、端末は自分で設定し、導入時には起動しない |
| `kehai.py` | macOS のメディア再生状態。別途 `mediaremote-adapter` が必要 |
| `omron_read.py` | 特定の OMRON 環境センサー用。libusb と実機が必要で、汎用状態取得 API ではない |

## 導入・検証スクリプト

- `scripts/configure.py`：合言葉を検証し、Python/Haskell 共通のローカル値と空記録を作る。既存記録は上書きしない。
- `scripts/build.py`：独立した macOS コンパイル用サンドボックスでコアと OwnTime をビルド。
- `scripts/hikaru`：このインストール先のパスをツールに渡す。
- `tests/smoke.py`：一時インストールに合成入力を与え、主要経路を検証。
- `scripts/package.py`：公開許可リストだけを梱包し、ローカル生成データとバイナリを除外。
