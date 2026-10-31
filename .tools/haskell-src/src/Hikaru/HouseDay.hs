-- | ひかるの一日。朝 5 時で切り替わる（夜中の 0 時すぎは、まだ前の日）。
--
-- 暦の日（Data.Time の 'Day'）とは別の型にしてある。タイムマシンのファイル名、
-- 日記やありあけの日付、バックアップのフォルダ、さざなみの「きょう」は、どれも
-- この一日で数える。now の「暦の上では N 日」のような暦の日の計算と混ぜると、
-- 型が合わずにコンパイルが通らない（2026-10-22、重构清单の第 1 步）。
--
-- 時刻から一日を出す道は 'houseDayFromJST' の一つだけ。ファイル名や引数に
-- もう書いてある日付を、そのまま一日として受け取るときは 'houseDayNamed'。
-- 暦の計算（曜日、節気、週の初め、何日前か）に使うときは 'houseDayDate' で
-- 暦の日に戻す。どちらへ行くにも、名前のついた関数を通る。
module Hikaru.HouseDay
  ( HouseDay, houseDayDate, houseDayNamed, houseDayFromJST, houseDayEntry
  ) where

import Data.Time

newtype HouseDay = HouseDay Day
  deriving (Eq, Ord)

-- | 表示は中の日付と同じ（バックアップのフォルダ名 YYYY-MM-DD など）。
instance Show HouseDay where
  show (HouseDay day) = show day

-- | 暦の日に戻す。曜日や節気、日数の差を計算するとき。
houseDayDate :: HouseDay -> Day
houseDayDate (HouseDay day) = day

-- | ファイル名・見出し・引数に YYYY-MM-DD と書いてある日を、一日として読む。
-- 時刻からではないので、5 時の切り替えはもう済んでいるものとして扱う。
houseDayNamed :: Day -> HouseDay
houseDayNamed = HouseDay

-- | 日本時間の時刻が、どの一日に属するか。5 時間引いた日付（固定 Python の
-- @hikaru_day@ と同じ）。結果が西暦 1〜9999 年の外なら 'Nothing'。
-- その場合の例外の形（OverflowError など）は、呼ぶ側が元のとおりに決める。
houseDayFromJST :: LocalTime -> Maybe HouseDay
houseDayFromJST local =
  let day = localDay (addLocalTime (-5 * 3600) local)
  in if validDay day then Just (HouseDay day) else Nothing

-- | 逆向き：一日の見出しと「時:分:秒」から、日本時間の時刻を組み立てる。
-- 0〜4 時台は、暦では次の日（タイムマシンの見出しは「5時〜翌4時59分」）。
-- 組み立てた日付が 1〜9999 年の外なら 'Nothing'。時分秒の範囲は呼ぶ側が見る。
houseDayEntry :: HouseDay -> Int -> Maybe Day
houseDayEntry (HouseDay day) hour =
  let target = if hour < 5 then addDays 1 day else day
  in if validDay target then Just target else Nothing

validDay :: Day -> Bool
validDay day = let (year, _, _) = toGregorian day in year >= 1 && year <= 9999
