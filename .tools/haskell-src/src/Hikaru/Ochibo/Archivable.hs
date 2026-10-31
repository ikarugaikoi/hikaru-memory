-- | 記録してよい一言。
--
-- タイムマシンの原文や、.おしばな の画像に書いてよいのは、この型の一言だけ。
-- 構成子は外に出していないので、作る道は 'archivable' 一つ（とばりの中の
-- 一言と、.exclude の範囲の一言を、ここで落とす）。原文を書く関数はこの型しか
-- 受け取らないので、とばりの中の一言を書く道が、型の上でない
-- （2026-10-22、重构清单の第 5 步。それまでは、毎回 Bool で選り分けていた）。
--
-- とばりの中かどうかは、同じ一言が別の記録ファイルにもあるとき、どれか一つで
-- 隠れていれば隠れる（loadAll がまとめてから決まる）。'CollectedMessage' は
-- Ochibo.Read の mergeSessions でしか作れない型なので、ここに来るのは、もう
-- まとめ終わった一言だけ。一つの記録ファイルを読んだだけのもの（SessionRow）は
-- 別の型で、ここには渡せない（2026-10-22、Codex の審査のあとで）。
module Hikaru.Ochibo.Archivable
  ( Archivable, archivable, archivedRow, archivedMessage ) where

import Hikaru.Ochibo.Read (CollectedMessage, collectedHidden, collectedMessage)
import Hikaru.Conversation.Line (Utterance)

newtype Archivable = Archivable CollectedMessage

-- | 記録してよい一言だけを、順番を変えずに残す。
archivable :: [CollectedMessage] -> [Archivable]
archivable rows = [Archivable row | row <- rows, not (collectedHidden row)]

archivedRow :: Archivable -> CollectedMessage
archivedRow (Archivable row) = row

archivedMessage :: Archivable -> Utterance
archivedMessage = collectedMessage . archivedRow
