# 導入と設定

[中文](../setup.md) · [English](../en/setup.md) · **日本語** · [トップ](../../README.ja.md)

## 環境と対象範囲

検証環境は macOS arm64、GHC 9.10.3、Cabal、Python 3.12.12、Apple の開発ツールです。`icucore`、一部の C 互換関数、サンドボックスは macOS の API を使います。他の OS では移植が必要で、サンドボックスを取り除くだけでは移植完了とはいえません。

新しいマシンでの全手順は未検証です。ローカルのビルドには既存の固定済み依存データベースを使いましたが、キャッシュやバイナリは公開物に含めません。

## 1. 自分のインスタンスを設定する

```bash
cp settings.example.json settings.local.json
```

ローカルの JSON を編集します。

| フィールド | 意味 |
|---|---|
| `display_name`、`assistant_name` | 生成するホスト指示内の呼び名。コアの形式では固定の話者マーカーを使う |
| `privacy_phrases` | 不記録の開始・終了の組を1組以上設定する。自分で入力すること。指定された句読点を除き、ユーザー入力全体に一致させる |
| `log_root` | 処理を許可した Claude Code プロジェクトの JSONL ディレクトリ。空欄ならこのインスタンスの空の `.local/logs` |
| `project_identity` | ログの `cwd` と対応するプロジェクトパス。空欄ならリポジトリの絶対パス |
| `weather.enabled` | 既定は `false`。有効にする場合は有効な緯度・経度が必要 |
| `backup` | restic 実行ファイル、保存先、パスワード取得コマンド、対象ディレクトリ。不足があれば実行しない |

```bash
python3 scripts/configure.py
```

`.local/privacy.json` と `.tools/haskell-src/.generated/Hikaru/Local/Settings.hs` を生成し、既存の記録がなければ空の記録と `CLAUDE.md` を作ります。**合言葉や天気設定を変えたら再コンパイルが必要です**。コアがコンパイル時定数を使うためです。設定の指紋が一致しない場合、Haskell・Python の両方が実行を拒否し、再ビルドを求めます。生成ソースとバイナリにもローカル設定が含まれるので、公開しないでください。

`settings.local.json`、`.local/`、生成ソース、記憶、索引は `.gitignore` の対象です。日付は日本時間、日界は05:00のままです。設定に変更可能なタイムゾーン項目はありません。

### 翻訳版テンプレートを使う

設定スクリプトが読むのは引き続き `templates/CLAUDE.md` と `templates/daily-review.md` で、言語は自動選択しません。初期化後、[日本語のホストテンプレート](../../templates/ja/CLAUDE.md)を基にローカルの `CLAUDE.md` を整え、`{{assistant_name}}` と `{{display_name}}` を置き換えてください。[毎日の整理テンプレート](../../templates/ja/daily-review.md)も必要に応じて `.local/daily-review.md` に反映できます。すでに追加した指示がある場合は保持し、既存ファイルを無条件に上書きしないでください。コマンド名、記録マーカー、実行時の出力は変わりません。

## 2. Haskell をコンパイルする

依存は `.tools/haskell-src/cabal.project.freeze` で固定しています。先に GHC 9.10.3 と Cabal を用意し、初回のマシンでは次を実行します。

```bash
cd .tools/haskell-src
cabal update
cabal build --only-dependencies
cd ../..
```

依存をダウンロードします。その後 GHC 9.10.3 用の Cabal package database を探します。通常は `~/.cabal/store/ghc-9.10.3-*/package.db` にあるので、実際のパスを渡します。

```bash
python3 scripts/build.py --package-db /path/to/ghc-9.10.3-xxxx/package.db
```

ビルドスクリプトはログと中間生成物を `.build/` に置きます。コンパイル子プロセスはネットワーク禁止、書き込みはビルドディレクトリのみです。`.tools/bin/hikaru-core` と `.tools/bin/hikaru-owntime` を生成し、コマンドのローカル別名を作ります。定期タスクの登録やサービス起動は行いません。

## 3. 基本確認と接続

```bash
python3 scripts/hikaru fuutou status
python3 scripts/hikaru fuutou show yuugao
python3 scripts/hikaru ochibo --dry-run
python3 scripts/hikaru omoide '例' --mode lexical
```

dry-run のログ範囲を確かめてから保存してください。`log_root` と `project_identity` は同じ許可済みプロジェクトに対応する必要があります。ユーザー発話が見つからない場合、ユーザーディレクトリ全体に範囲を広げず、ログのフィールドと識別条件を確認します。

現在のアダプターは Claude Code 用です。直接のユーザー発話には `origin.kind == "human"` を要求し、`cwd`、型、時刻なども照合します。他のクライアントには別途アダプターが必要で、拡張子の変更だけでは取り込めません。

ホスト hook から `python3 /absolute/path/scripts/hikaru ima`、`oshirase`、`taguri_hook` を呼び、必要な JSON を標準入力に渡せます。グローバルな Claude 設定は自動編集しません。利用中のクライアント版のイベントと入力形式を確かめてから有効にしてください。

## 4. 必要に応じて追加する機能

- **ハイブリッド検索**：`.tools/haskell-src/requirements-model-macos-arm64.lock` の依存を自分の Python 環境に導入し、モデル重みは別途取得します。`scripts/hikaru _embed` と `scripts/hikaru omoide ... --mode hybrid` は同じインタープリターで実行します。この lock は元の環境の記録であり、新規マシンの全ダウンロードを検証したものではありません。
- **言語辞書**：fugashi と UniDic は任意です。不在やタイムアウトでは、その部分が未完了と報告し、利用可能な字形・退役表現の規則は実行します。完全に確認済みとは扱いません。
- **一般頻度**：`_yonotsune.py` は既定で `~/.local/share/hikaru/wordfreq-venv/bin/python` の独立した wordfreq 環境を探し、事前計算結果をこのインスタンス内に保存します。頻度不明だけを理由に新しい別名は止めません。
- **自動連想の提示**：まずリプレイを観察し、明示的に `.tools/index/koyori/deliver.on` を作ります。削除すると観察専用に戻ります。既定では提示しません。
- **振り返りの参考資料**：[Biblia](biblia.md)は問題と研究方向の対応だけを示し、具体的な文献や完全な分析手順を含みません。資料と指示は自分で設定します。`biblia` は空の読書記録から始まり、設定スクリプトがガイドを取り込むことはありません。
- **自主活動**：Claude CLI、認証情報、資料、完全な指示を用意し、[OwnTime テンプレート](../../templates/ja/owntime.md)を整えてからスケジュールを設定します。現在のコードは Claude CLI に依存します。人格テンプレートだけでは接続は完了しません。
- **バックアップ・メディア・携帯位置・センサー**：独立した任意機能です。[ツール一覧](tools.md)を参照してください。

## 5. 合成データによる検証

`tests/smoke.py` は一時コピー、合成会話、ネットワーク禁止のサンドボックスを使います。テスト用ビルドには公開の合成合言葉を使い、私的な合言葉入りのバイナリを流用しないでください。実行方法と範囲は[検証記録](validation.md)にあります。

## 6. 共有する

`scripts/package.py` は `release-files.txt` の許可リストに従ってソースを梱包します。Git の除外ファイル、実際の記憶、ビルド生成物は集めません。公開ファイルを増やす際はリストと内容を確認し、作業ディレクトリ全体の圧縮で代用しないでください。
