{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | The timestamp subset required by the first hook. Fractional seconds are
-- truncated to Python's six microsecond digits. Sub-minute UTC offsets are
-- represented by their exact UTC instant. Naive DST fold/gap choice follows
-- CPython's local-time inversion with precise host-local offset seconds from
-- a small POSIX shim. No Python process is invoked here.
module Hikaru.Compat.Time
  ( jst, parseDateOnly, formatNowTime, gapParts
  , parseClockInZone, datePrefix, clockParts
  , localMktimeWith, localCivilChecked, localOffset ) where

import Data.Char (ord)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Data.Word (Word8)
import Hikaru.Compat.Diagnostic (dateDiagnostic, raiseDiagnostic)

-- C fmod is the primitive used by CPython Objects/floatobject.c. Replacing
-- it with x - floor(x/y)*y is not equivalent near rounded multiples of y.
foreign import ccall unsafe "fmod" c_fmod :: CDouble -> CDouble -> CDouble

foreign import ccall unsafe "hikaru_local_offset_seconds"
  c_localOffsetSeconds :: Int64 -> Ptr Int64 -> IO CInt

jst :: TimeZone
jst = minutesToTimeZone 540

data ParsedTimestamp = Zoned ZonedTime | Naive LocalTime

decimal :: Text -> Either Text Int
decimal value
  | T.null value || T.any (\c -> c < '0' || c > '9') value = Left "invalid ASCII decimal"
  | otherwise = Right (T.foldl' (\n c -> n * 10 + ord c - ord '0') 0 value)

asciiDigit :: Char -> Bool
asciiDigit c = c >= '0' && c <= '9'

-- CPython has special disambiguation for a numeric date/time separator next
-- to an ISO week date. Do not greedily consume any following digit as weekday.
isoSeparator :: Text -> Either Text Int
isoSeparator value
  | size < 7 = Left "invalid ISO date"
  | size == 7 = Right 7
  | at 4 == '-' =
      if at 5 /= 'W' then Right 10
      else if size > 8 && at 8 == '-'
           then if size == 9 then Left "incomplete ISO week date"
                else if size > 10 && asciiDigit (at 10) then Right 8 else Right 10
           else Right 8
  | at 4 == 'W' =
      let end = 7 + T.length (T.takeWhile asciiDigit (T.drop 7 value))
      in Right (if end < 9 then end else if even end then 7 else 8)
  | otherwise = Right 8
  where
    size = T.length value
    at = T.index value

datePrefix :: Text -> Either Text (Day, Text)
datePrefix input = do
  count <- isoSeparator input
  let value = T.take count input
      size = T.length value
  if size `notElem` [7,8,10] then Left "invalid ISO date length" else pure ()
  day <- parseDate value
  pure (day, T.drop count input)

parseDate :: Text -> Either Text Day
parseDate value = do
  y <- year
  if at componentStart == 'W' then weekDate y else gregorian y
  where
    size = T.length value
    at = T.index value
    separated = at 4 == '-'
    componentStart = if separated then 5 else 4
    part start count = decimal (T.take count (T.drop start value))
    year = do
      y <- part 0 4
      if y >= 1 && y <= 9999 then Right (fromIntegral y) else Left "year out of range"
    gregorian y = do
      if size /= (if separated then 10 else 8)
        then Left "invalid Gregorian date length" else pure ()
      if separated && at 7 /= '-' then Left "inconsistent ISO separator" else pure ()
      m <- part componentStart 2
      d <- part (if separated then 8 else 6) 2
      case fromGregorianValid y m d of
        Nothing -> Left "invalid Gregorian date"
        Just day -> Right day
    weekDate y = do
      let weekPos = componentStart + 1
          afterWeek = weekPos + 2
          fullSize = if separated then 10 else 8
      if size /= afterWeek && size /= fullSize
        then Left "invalid ISO week date length" else pure ()
      w <- part weekPos 2
      d <- if size == afterWeek then Right 1 else do
        if separated && at afterWeek /= '-'
          then Left "inconsistent ISO week separator" else pure ()
        part (if separated then afterWeek + 1 else afterWeek) 1
      let day = fromWeekDate y w d
      if w >= 1 && w <= 53 && d >= 1 && d <= 7 && toWeekDate day == (y,w,d) && validYear day
        then Right day
        else Left "invalid ISO week date"

-- date.fromisoformat has no timezone conversion or datetime year underflow.
-- Its date shapes are the same Gregorian/week productions used above.
parseDateOnly :: Text -> Either Text Day
parseDateOnly value
  | T.length value `elem` [7, 8, 10] = parseDate value
  | otherwise = Left "invalid ISO date length"

validYear :: Day -> Bool
validYear day = let (year,_,_) = toGregorian day in year >= 1 && year <= 9999

-- CPython 3.12.12's accelerated parse_hh_mm_ss_ff has a byte-level cursor and
-- a separate end boundary. The byte after that boundary is a timezone sign
-- or NUL. Its trailing-input flag is intentionally ignored for the clock
-- when a timezone exists, but must be zero for the timezone itself. This
-- preserves odd accepted forms such as "05:12:34.+09:00", rather than silently
-- narrowing the input language to ISO syntax. Source and synthetic oracle:
-- https://github.com/python/cpython/blob/v3.12.12/Modules/_datetimemodule.c
clockParts :: Word8 -> Text -> Either Text ((Int, Int, Int, Int), Bool)
clockParts following input = component 0 0 [] False
  where
    bytes = TE.encodeUtf8 input
    end = BS.length bytes
    byte pos | pos < end = BS.index bytes pos
             | pos == end = following
             | otherwise = 0
    isDigit value = value >= 48 && value <= 57
    digits count pos =
      let values = [byte i | i <- [pos .. pos + count - 1]]
      in if count < 1 || any (not . isDigit) values
         then Left "invalid ISO clock decimal"
         else Right (foldl (\n value -> n * 10 + fromIntegral value - 48) 0 values, pos + count)
    finish values micros trailing =
      let padded = values ++ replicate (3 - length values) 0
      in Right ((padded !! 0, padded !! 1, padded !! 2, micros), trailing)
    component index pos values wasSeparated = do
      (value, next) <- digits 2 pos
      let values' = values ++ [value]
          separator = byte next
          after = next + 1
          separated = if index == 0 then separator == 58 else wasSeparated
          continue cursor = if index == 2 then fractional cursor values'
                            else component (index + 1) cursor values' separated
      if after >= end then finish values' 0 (separator /= 0)
        else if separated && separator == 58 then continue after
        else if separator == 46 || separator == 44 then fractional after values'
        else if not separated then continue next
        else Left "invalid ISO clock separator"
    fractional pos values = do
      let count = min 6 (end - pos)
      (value, next) <- digits count pos
      let micros = value * 10 ^ (6 - count)
          skip p = if isDigit (byte p) then skip (p + 1) else p
      finish values micros (byte (skip next) /= 0)

timestampParts :: Text -> Either Text ParsedTimestamp
timestampParts raw = do
  -- The baseline replaces every Z before datetime.fromisoformat.
  (day, rest) <- datePrefix (T.replace "Z" "+00:00" raw)
  if T.null rest then Right (Naive (LocalTime day midnight)) else do
    let timeAndZone = T.drop 1 rest
        (clock, zone) = T.break (\c -> c == '+' || c == '-') timeAndZone
    ((h,m,s,micros), trailingClock) <- clockParts (if T.null zone then 0 else 43) clock
    if T.null zone && trailingClock then Left "trailing ISO clock input" else pure ()
    if h > 23 || m > 59 || s > 59 then Left "clock value out of range" else pure ()
    let tod = TimeOfDay h m (fromIntegral s + fromIntegral micros / 1000000)
        local = LocalTime day tod
    if T.null zone then Right (Naive local) else do
      ((zh,zm,zs,zf), trailingZone) <- clockParts 0 (T.drop 1 zone)
      if trailingZone then Left "trailing ISO timezone input" else pure ()
      let seconds = zh * 3600 + zm * 60 + zs
          -- CPython 3.12's accelerated parser special-cases integer offset
          -- zero as UTC even when a nonzero fractional offset was supplied.
          unsignedMicros = if seconds == 0 then 0 else toInteger seconds * 1000000 + toInteger zf
          signedMicros = if T.head zone == '-' then negate unsignedMicros else unsignedMicros
      if unsignedMicros >= 86400 * 1000000 then Left "timezone offset must be less than 24 hours"
        else let offset = fromInteger signedMicros / 1000000
                 instant = addUTCTime (negate offset) (localTimeToUTC utc local)
             in Right (Zoned (utcToZonedTime utc instant))

-- | The explicitly injected clock uses fromisoformat(value).astimezone(zone).
-- Keep this separate from timestamp(): astimezone must construct a bounded UTC
-- datetime before the destination, and a naive input consults the host zone.
-- The ordinary system clock does not pass through this replay-only adapter.
parseClockInZone :: TimeZone -> Text -> IO (Either Text ZonedTime)
parseClockInZone destination text = case timestampParts text of
  Left err -> pure (Left err)
  Right (Zoned value) -> pure (convert (zonedTimeToUTC value))
  Right (Naive local) -> do
    let civil = floor (utcTimeToPOSIXSeconds (localTimeToUTC utc local))
    first <- localMktimeWith localCivilChecked False civil
    second <- localMktimeWith localCivilChecked True civil
    offset <- localOffset (min first second)
    pure (convert (addUTCTime (negate (fromInteger offset)) (localTimeToUTC utc local)))
  where
    convert instant =
      let result = utcToZonedTime destination instant
      in if validYear (utctDay instant) && validYear (localDay (zonedTimeToLocalTime result))
         then Right result else Left "datetime conversion outside year range"

-- Consult the host/TZ setting at the requested instant, not the contemporary
-- offset. The C boundary returns seconds, preserving historical LMT offsets;
-- no Haskell code assumes a platform layout for struct tm or time_t.
localOffset :: Integer -> IO Integer
localOffset epochSeconds
  | epochSeconds < toInteger (minBound :: Int64) || epochSeconds > toInteger (maxBound :: Int64) =
      ioError (userError "host localtime epoch out of range")
  | otherwise = alloca $ \out -> do
      status <- c_localOffsetSeconds (fromInteger epochSeconds) out
      if status == 0 then toInteger <$> peek out
        else ioError (userError ("host localtime failed: " ++ show status))

localCivil :: Integer -> IO Integer
localCivil epochSeconds = (epochSeconds +) <$> localOffset epochSeconds

-- datetime.timestamp's accelerated local_to_seconds rejects localtime
-- structures outside Python's datetime year range, including probe instants
-- used to resolve a fold. Keep this separate from the already-validated
-- astimezone path's host offset lookup.
localCivilChecked :: Integer -> IO Integer
localCivilChecked epochSeconds = do
  civil <- localCivil epochSeconds
  let (year,_,_) = toGregorian (utctDay (posixSecondsToUTCTime (fromInteger civil)))
  if year >= 1 && year <= 9999
    then pure civil else raiseDiagnostic (dateDiagnostic year 1 1)

-- CPython 3.12 datetime._mktime/local_to_seconds algorithm, including its
-- 24-hour search and opposite min/max rule for an impossible local gap.
localMktimeWith :: (Integer -> IO Integer) -> Bool -> Integer -> IO Integer
localMktimeWith toLocal foldValue civil = do
  a <- subtract civil <$> toLocal civil
  let u1 = civil - a
  t1 <- toLocal u1
  if t1 == civil
    then do
      let probe = u1 + if foldValue then 86400 else -86400
      b <- subtract probe <$> toLocal probe
      if a == b then pure u1 else finish u1 t1 b
    else finish u1 t1 (t1 - u1)
  where
    finish u1 t1 b = do
      let u2 = civil - b
      t2 <- toLocal u2
      if t2 == civil then pure u2
        else if t1 == civil then pure u1
        else pure ((if foldValue then min else max) u1 u2)

weekdayText :: Day -> Text
weekdayText day = case dayOfWeek day of
  Monday -> "月"; Tuesday -> "火"; Wednesday -> "水"; Thursday -> "木"
  Friday -> "金"; Saturday -> "土"; Sunday -> "日"

formatNowTime :: ZonedTime -> Text
formatNowTime value =
  let local = zonedTimeToLocalTime (utcToZonedTime jst (zonedTimeToUTC value))
      (year, month, day) = toGregorian (localDay local)
      tod = localTimeOfDay local
      showT :: Show a => a -> Text
      showT = T.pack . show
      pad2 :: Int -> Text
      pad2 n = T.justifyRight 2 '0' (showT n)
  in T.justifyRight 4 '0' (showT year) <> "年" <> showT month <> "月" <> showT day <> "日（"
     <> weekdayText (localDay local) <> "）" <> pad2 (todHour tod) <> ":" <> pad2 (todMin tod)

-- CPython's _float_div_mod, for the finite values and nonzero divisors used
-- by datetime gaps. The quotient is derived from fmod and then snapped to the
-- nearest integral quotient, matching the C algorithm rather than floor(x/y).
pythonFloatDivMod :: Double -> Double -> (Double, Double)
pythonFloatDivMod x y =
  let modulus0 = realToFrac (c_fmod (realToFrac x) (realToFrac y))
      quotient0 = (x - modulus0) / y
      (quotient, modulus)
        | modulus0 /= 0 && ((y < 0) /= (modulus0 < 0)) = (quotient0 - 1, modulus0 + y)
        | modulus0 /= 0 = (quotient0, modulus0)
        | otherwise = (quotient0, signedZero y)
      floorQuotient
        | quotient /= 0 =
            let floored = fromInteger (floor quotient)
            in if quotient - floored > 0.5 then floored + 1 else floored
        | otherwise = signedZero (x / y)
  in (floorQuotient, modulus)
  where
    signedZero value = if value < 0 || isNegativeZero value then -0.0 else 0.0

-- | Python-style elapsed full hours, nonnegative remainder minutes and JST
-- calendar-day difference. No 05:00 day boundary is involved in now.py.
gapParts :: ZonedTime -> ZonedTime -> (Integer, Integer, Integer)
gapParts now previous =
  let secs = realToFrac (diffUTCTime (zonedTimeToUTC now) (zonedTimeToUTC previous)) :: Double
      (hoursValue, remainder) = pythonFloatDivMod secs 3600
      (minutesValue, _) = pythonFloatDivMod remainder 60
      hours = truncate hoursValue :: Integer
      minutes = truncate minutesValue :: Integer
      asDay = localDay . zonedTimeToLocalTime . utcToZonedTime jst . zonedTimeToUTC
  in (hours, minutes, diffDays (asDay now) (asDay previous))
