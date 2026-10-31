-- | 反思の拠りどころと、丘の上へ出せるもの（設計稿の第一ブロック「反思」と「哪些写成类型」
-- 第 7 条）。
--
-- ユーザーのこと、わたしたちのあいだのことの反思は、信頼できる CA の判だけを拠りどころに
-- する。わたし自身の読書や歩みの反思は、みちくさだけを拠りどころにする。丘の上へ
-- 持ち出して、ユーザーと相談できるのは前者だけ。読んだもの、見たもの、そこから
-- 育った自分についての反思は、みちくさと記憶流に残り、関わるときに浮かんで、
-- 弱まっていく（ユーザーが 2026-10-22 に言った「看的东西之类的是不是不适合进入丘の上」）。
-- 丘の上は毎回最初に読むので、書いたものが設定になって、ずっとわたしを引っぱるから。
module Hikaru.OwnTime.Reflection
  ( CAEvidence(..), AboutUs, AboutMe, Reflection(..)
  , aboutUs, aboutMe, Proposal, proposeForOka
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Hikaru.OwnTime.Michikusa (Footprint)

-- | CA の判への指し示し（ひとときの ID と時刻、判の一文）。
data CAEvidence = CAEvidence
  { caMoment :: Text
  , caJudgement :: Text
  }
  deriving (Eq, Show)

-- | わたしたちのあいだの反思。拠りどころは CA の判だけ。
newtype AboutUs = AboutUs (NonEmpty CAEvidence)
  deriving (Eq, Show)

-- | わたし自身の読書・歩みの反思。拠りどころはみちくさだけ。
newtype AboutMe = AboutMe (NonEmpty Footprint)
  deriving (Eq, Show)

data Reflection = Us AboutUs | Me AboutMe
  deriving (Eq, Show)

aboutUs :: NonEmpty CAEvidence -> AboutUs
aboutUs = AboutUs

aboutMe :: NonEmpty Footprint -> AboutMe
aboutMe = AboutMe

-- | 丘の上について、ユーザーと相談する案。'proposeForOka' でしか作れない。
newtype Proposal = Proposal AboutUs
  deriving (Eq, Show)


-- | 丘の上へ出せるのは、わたしたちのあいだの反思だけ。'AboutMe' を渡すと、型が合わない。
proposeForOka :: AboutUs -> Proposal
proposeForOka = Proposal
