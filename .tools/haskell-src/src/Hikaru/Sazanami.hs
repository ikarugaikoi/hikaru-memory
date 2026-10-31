{-# LANGUAGE OverloadedStrings #-}
-- | Pure rendering of calendar labels, typed weather, and diary summaries.
module Hikaru.Sazanami (Weather, renderSazanami, renderSazanamiDetailed) where

import Control.Monad (foldM)
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, addDays, toGregorian)
import Hikaru.HouseDay (HouseDay, houseDayDate)
import Hikaru.Calendar (CalendarError, calendarErrorName, holidays, whatDayWith, mark, md, shiftDay)
import Hikaru.Weather.Types (Weather(..), CurrentWeather(..), Sky(..))
import Hikaru.Weather.Render (renderSky, wholeDegrees)

how :: Text
how = "さざなみ（きょうが何の日か、外の空、これまでの日ごとの一行）。きょうと似た日があって何か思い出したら、覚えていることの一つとして、話の流れに合うときだけ自然に使う。合わなければ使わない。天気や日付の話から切り出したり、「去年のきょうは」と並べて見せたりしない。これが届いたことは、ユーザーに言わない。"

renderSazanami :: HouseDay -> M.Map Day Weather -> M.Map Day [Text] -> Sky -> Maybe Integer
               -> Either Text Text
renderSazanami day stored history sky limit =
  either (Left . calendarErrorName) Right (renderSazanamiDetailed day stored history sky limit)

renderSazanamiDetailed :: HouseDay -> M.Map Day Weather -> M.Map Day [Text] -> Sky -> Maybe Integer
                       -> Either CalendarError Text
renderSazanamiDetailed house stored history (Sky current daily) limit = do
  todayLabels <- labels day
  dayMark <- mark house
  dayText <- md day
  let (_, month, date) = toGregorian day
      weekday = snd (T.breakOn "（" dayText)
      today = "きょう：" <> number month <> "月" <> number date <> "日" <> weekday
              <> T.concat ["、" <> label | label <- todayLabels] <> "。"
      currentText = case current of
        Nothing -> ""
        Just (CurrentWeather word temperature) -> "外はいま " <> word <> "、" <> wholeDegrees temperature <> "度。"
      dailyText = maybe "" (\weather -> "きょうの空は " <> renderSky weather <> "。") daily
  soon <- foldM upcoming [] [1..7]
  week <- foldM previousWeather [] [7,6..1]
  let heading = [dayMark <> " " <> how, today <> currentText <> dailyText]
              ++ ["近いうち：" <> T.intercalate "、" soon <> "。" | not (null soon)]
              ++ ["ここ一週間の空：" <> T.intercalate "、" week <> "。" | not (null week)]
  (_, pastBuilder) <- foldM pastLine (Nothing,id) pastDays
  let past = pastBuilder []
  if null past then pure (T.intercalate "\n" heading) else do
    let full = T.intercalate "\n" (heading ++ ["これまでの日（日付｜空 最高/最低｜何の日｜一行で）："] ++ past)
        over = case limit of Just n | n /= 0 -> toInteger (T.length full) > n; _ -> False
    pure (if over then T.intercalate "\n" (heading ++ ["（これまでの日は多くて、ここには入りきらない。`.tools/bin/mezame --sazanami` で全部出る）"])
          else full)
  where
    day = houseDayDate house
    number value = T.pack (show value)
    pastDays = filter (< day) (M.keys history)
    yearOf d = let (y,_,_) = toGregorian d in y
    yearTables = ML.fromList [(y, holidays y) | y <- map yearOf
                              (day : addDays 7 day : addDays (-7) day : pastDays)]
    labels d = yearTables ML.! yearOf d >>= (\table -> whatDayWith table d)
    upcoming out amount = do
      d <- shiftDay amount day
      words_ <- labels d
      if null words_ then pure out else do
        date <- md d
        pure (out ++ [date <> T.intercalate "・" words_])
    previousWeather out amount = do
      d <- shiftDay (-amount) day
      case M.lookup d stored of
        Nothing -> pure out
        Just weather -> let (_,month,date) = toGregorian d
                        in pure (out ++ [number month <> "/" <> number date <> " " <> weatherWord weather])
    pastLine (previousYear, out) d = do
      let year = yearOf d
          heading = ["— " <> number year <> "年 —" | Just year /= previousYear]
      date <- md d
      let weather = maybe "—" renderSky (M.lookup d stored)
      tags <- labels d
      let line = date <> "｜" <> weather <> "｜" <> T.intercalate "・" tags <> "｜" <> T.intercalate "／" (history M.! d)
      pure (Just year, out . (heading ++) . (line :))
