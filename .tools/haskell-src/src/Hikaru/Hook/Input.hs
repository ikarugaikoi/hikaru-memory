{-# LANGUAGE OverloadedStrings #-}
-- | JSON stops at this boundary; reducers never inspect an Aeson Value.
module Hikaru.Hook.Input
  ( RowError(..), decodeRow, optionalText, flag, contentText, inProject
  , hookOutput, timestamp, jst, formatNowTime, gapParts
  ) where

import Data.Aeson hiding (object)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import Hikaru.Conversation.Input

-- Keep the established wire framing and key order; Aeson owns string escaping.
hookOutput :: Text -> Text -> BS.ByteString
hookOutput event context = BS.concat
  ["{\"hookSpecificOutput\": {\"hookEventName\": ", scalar event
  ,", \"additionalContext\": ", scalar context, "}}\n"]
  where scalar = BL.toStrict . encode

formatNowTime :: ZonedTime -> Text
formatNowTime value =
  let local = zonedTimeToLocalTime (utcToZonedTime jst (zonedTimeToUTC value))
      (year, month, day) = toGregorian (localDay local)
      (_, _, weekday) = toWeekDate (localDay local)
      tod = localTimeOfDay local
      showT = T.pack . show
      pad n = T.justifyRight 2 '0' (showT n)
  in T.justifyRight 4 '0' (showT year) <> "年" <> showT (toInteger month) <> "月" <> showT (toInteger day)
     <> "日（" <> T.singleton ("月火水木金土日" !! (weekday - 1)) <> "）"
     <> pad (toInteger (todHour tod)) <> ":" <> pad (toInteger (todMin tod))

gapParts :: ZonedTime -> UTCTime -> (Integer, Integer, Integer)
gapParts now previous =
  let minutes = floor (diffUTCTime (zonedTimeToUTC now) previous / 60) :: Integer
      day = localDay . zonedTimeToLocalTime . utcToZonedTime jst
  in (minutes `div` 60, minutes `mod` 60, diffDays (day (zonedTimeToUTC now)) (day previous))
