{-# LANGUAGE OverloadedStrings #-}

-- | Exact datetime values used by conversation parsing. Ordering never goes
-- through binary64, and naive values remain distinct from aware values.
-- The ISO lexer is shared with the already-validated hook implementation.
module Hikaru.Conversation.DateTime
  ( DateTime(..), DateZone(..), parseDateTime, parseDateTimeText, parseDateTimeValue
  , compareDateTime, equalDateTime, subtractDateTime, localMicros
  , dateTimeToJST, dateTimeToJSTWith, hostLocalToJST
  , entryDateTime, formatDateTime, dateTimeDay
  ) where

import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Hikaru.Compat.Json
import Hikaru.Compat.Time
  (clockParts, datePrefix, jst, localMktimeWith, localCivilChecked, localOffset)
import Hikaru.HouseDay (HouseDay, houseDayEntry, houseDayFromJST)

-- ProjectJST preserves the identity shortcut of dt.astimezone(_hk.JST).
-- fromisoformat("...+09:00") makes a distinct timezone, even with equal offset.
data DateZone = NaiveZone | FixedOffset !Integer | ProjectJST
  deriving (Eq, Show)

data DateTime = DateTime
  { dateTimeLocal :: !LocalTime
  , dateTimeZone :: !DateZone
  } deriving Show

parseDateTimeValue :: Value -> Either Text DateTime
parseDateTimeValue (String value) = parseDateTime value
parseDateTimeValue _ = Left "timestamp replace/fromisoformat requires string"

-- A lone surrogate is permitted only in the arbitrary one-code-point datetime
-- separator. Replacing it here preserves both length and byte-parser rejection
-- elsewhere; no returned user text is normalized or replaced.
parseDateTime :: PyString -> Either Text DateTime
parseDateTime = parseDateTimeText . T.pack . map safe . pyStringToChars
  where safe c | ord c >= 0xd800 && ord c <= 0xdfff = '\xfffd'
               | otherwise = c

parseDateTimeText :: Text -> Either Text DateTime
parseDateTimeText raw = do
  (day, rest) <- datePrefix (T.replace "Z" "+00:00" raw)
  if T.null rest then Right (DateTime (LocalTime day midnight) NaiveZone) else do
    let (clock, zone) = T.break (\c -> c == '+' || c == '-') (T.drop 1 rest)
    ((h,m,s,micro), trailingClock) <- clockParts (if T.null zone then 0 else 43) clock
    if T.null zone && trailingClock then Left "trailing ISO clock input" else pure ()
    if h > 23 || m > 59 || s > 59 then Left "clock value out of range" else pure ()
    let local = LocalTime day (TimeOfDay h m (fromIntegral s + fromIntegral micro / 1000000))
    if T.null zone then Right (DateTime local NaiveZone) else do
      ((zh,zm,zs,zf), trailingZone) <- clockParts 0 (T.drop 1 zone)
      if trailingZone then Left "trailing ISO timezone input" else pure ()
      let seconds = zh * 3600 + zm * 60 + zs
          unsigned = if seconds == 0 then 0 else toInteger seconds * 1000000 + toInteger zf
          offset = if T.head zone == '-' then negate unsigned else unsigned
      if unsigned >= 86400 * 1000000 then Left "timezone offset must be less than 24 hours"
        else Right (DateTime local (FixedOffset offset))

zoneOffset :: DateZone -> Maybe Integer
zoneOffset NaiveZone = Nothing
zoneOffset (FixedOffset offset) = Just offset
zoneOffset ProjectJST = Just (32400 * 1000000)

localMicros :: LocalTime -> Integer
localMicros = round . (*1000000) . utcTimeToPOSIXSeconds . localTimeToUTC utc

comparisonMicros :: DateTime -> Integer
comparisonMicros (DateTime local zone) = localMicros local - maybe 0 id (zoneOffset zone)

compareDateTime :: DateTime -> DateTime -> Either Text Ordering
compareDateTime left right
  | (dateTimeZone left == NaiveZone) /= (dateTimeZone right == NaiveZone) =
      Left "cannot compare offset-naive and offset-aware datetimes"
  | otherwise = Right (compare (comparisonMicros left) (comparisonMicros right))

equalDateTime :: DateTime -> DateTime -> Bool
equalDateTime a b = compareDateTime a b == Right EQ

subtractDateTime :: DateTime -> DateTime -> Either Text Integer
subtractDateTime left right = do
  _ <- compareDateTime left right
  pure (comparisonMicros left - comparisonMicros right)

validDay :: Day -> Bool
validDay day = let (y,_,_) = toGregorian day in y >= 1 && y <= 9999

dateTimeToJSTWith :: Monad m
  => (LocalTime -> m (Either Text LocalTime)) -> DateTime -> m (Either Text LocalTime)
dateTimeToJSTWith localResolver (DateTime local NaiveZone) = localResolver local
dateTimeToJSTWith _ (DateTime local ProjectJST) = pure (Right local)
dateTimeToJSTWith _ stamp = pure $
  let instant = posixSecondsToUTCTime (fromInteger (comparisonMicros stamp) / 1000000)
      result = utcToLocalTime jst instant
  in if validDay (utctDay instant) && validDay (localDay result)
     then Right result else Left "datetime conversion outside year range"

hostLocalToJST :: LocalTime -> IO (Either Text LocalTime)
hostLocalToJST local = do
  let civil = localMicros local `div` 1000000
  -- datetime.astimezone resolves both fold candidates and chooses the earlier
  -- instant. Every local_to_seconds probe must fit Python's datetime years,
  -- including probes that lie one day beyond a valid input near year bounds.
  first <- localMktimeWith localCivilChecked False civil
  second <- localMktimeWith localCivilChecked True civil
  offset <- localOffset (min first second)
  let instant = addUTCTime (negate (fromInteger offset)) (localTimeToUTC utc local)
      result = utcToLocalTime jst instant
  pure (if validDay (utctDay instant) && validDay (localDay result)
        then Right result else Left "datetime conversion outside year range")

dateTimeToJST :: DateTime -> IO (Either Text LocalTime)
dateTimeToJST = dateTimeToJSTWith hostLocalToJST

entryDateTime :: HouseDay -> Int -> Int -> Int -> Either Text DateTime
entryDateTime day hh mm ss = case houseDayEntry day hh of
  Just target | hh >= 0 && hh < 24 && mm >= 0 && mm < 60 && ss >= 0 && ss < 60 ->
    Right (DateTime (LocalTime target (TimeOfDay hh mm (fromIntegral ss))) ProjectJST)
  _ -> Left "entry datetime out of range"

-- `hikaru_day` converts to JST, subtracts five hours, then calls date().
dateTimeDay :: LocalTime -> Either Text HouseDay
dateTimeDay = maybe (Left "hikaru day outside datetime year range") Right . houseDayFromJST

formatDateTime :: DateTime -> Text
formatDateTime (DateTime local zone) =
  let (year,month,day) = toGregorian (localDay local)
      TimeOfDay h minute seconds = localTimeOfDay local
      micros = round (seconds * 1000000) :: Integer
      (second, fraction) = micros `divMod` 1000000
      pad n = T.justifyRight n '0' . T.pack . show
      fractionText n = if n == 0 then "" else "." <> pad 6 n
      zoneText offset =
        let (whole, frac) = abs offset `divMod` 1000000
            (hours, rest) = whole `divMod` 3600
            (minutes, secs) = rest `divMod` 60
        in (if offset < 0 then "-" else "+") <> pad 2 hours <> ":" <> pad 2 minutes
           <> (if secs /= 0 || frac /= 0 then ":" <> pad 2 secs <> fractionText frac else "")
  in pad 4 year <> "-" <> pad 2 month <> "-" <> pad 2 day <> "T"
     <> pad 2 h <> ":" <> pad 2 minute <> ":" <> pad 2 second <> fractionText fraction
     <> maybe "" zoneText (zoneOffset zone)
