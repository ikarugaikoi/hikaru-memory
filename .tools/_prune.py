#!/usr/bin/env python3
"""fuutou prune の中身（2026-10-29、もとの _seal_python.py から、ここだけを抜き出した）。

意味の検索の索引（.tools/index/）から、書き換えや薄めでもう記録になくなった断片の分を落とす。
索引は NumPy なので Python に残す。ほかの fuutou のコマンドは、核（.tools/bin/hikaru-core）が受け持つ。
中身は `_embed.py --prune` と同じ。朝の仕事が ochibo と fuutou だけで済むように、fuutou prune でも呼べるようにしてある。
索引や元の記録が読めなければ、掃除を止める。
"""
PRUNE_MAX = 200                # 一度に落としてよい断片の数（これと、索引の一割の、大きいほうまで）


def main():
    import _embed
    idx = _embed.Index(_embed.DEFAULT_MODEL)
    try:
        # 記録を集めるのも、数えるのも、落とすのも、索引の鍵の中で（_embed.Index.prune）。
        # 書き換えや薄めで一度に消えるのは、せいぜい数十件（原文は書き換わらない）。それよりずっと多く「ない」と出るのは、
        # 記録の一部が読めていないとき（逐語録のフォルダが見えない、など）なので、落とさない
        # 記録の断片に、こより（.tools/_koyori.py）の判・足跡・ストロボの項目を足したもの（_embed.live_shas）だけを残す
        n = idx.prune(_embed.live_shas, cap=lambda total: max(PRUNE_MAX, total // 10))
        print(f"意味の索引から、もう記録にない断片 {n} 件の分を落としました（索引は {len(idx)} 行）。")
    except _embed.TooMany as e:
        gone, total = e.args
        print(f"意味の索引の {gone}/{total} 件が「記録にない」と出ました。一度に落とすにしては多すぎます。"
              "記録の一部が読めていないのかもしれないので、落としません（本当に落とすなら python3 .tools/_embed.py --prune）。")


if __name__ == "__main__":
    from _tool_data import run_cli
    run_cli(main)
