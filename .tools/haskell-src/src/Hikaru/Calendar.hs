{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure calendar rules copied from the fixed _koyomi baseline. This is the
-- baseline's proleptic calculation, not a historically corrected holiday API.
-- In particular the lunar table ends at 2030 and holiday labels keep their
-- original insertion order. Dates outside Python's 1..9999 range fail rather
-- than silently using Data.Time's larger range.
module Hikaru.Calendar
  ( CalendarError(..), calendarErrorName, CalendarTime(..)
  , sun, sekki, sekkiAt, holidays, sekku, whatDay, whatDayWith
  , dayOf, dayOfJST, mark, md, shiftDay
  ) where

import Control.Monad (foldM)
import Data.List (find, sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import Foreign.C.Types (CDouble(..))
import Hikaru.HouseDay (HouseDay, houseDayDate, houseDayFromJST)

-- These correspond to the Python exception classes, not their platform-
-- dependent text. Callers decide which existing exception boundary catches
-- them (notice's worker catches all three).
data CalendarError = CalendarValueError | CalendarOverflowError | CalendarStopIteration
  deriving (Eq, Show)

calendarErrorName :: CalendarError -> Text
calendarErrorName CalendarValueError = "ValueError"
calendarErrorName CalendarOverflowError = "OverflowError"
calendarErrorName CalendarStopIteration = "StopIteration"

-- | Naive means attach the shared JST object first; AtJST represents a
-- datetime whose tzinfo /is/ that object (e.g. datetime.now(JST)). Both avoid
-- astimezone's intermediate UTC construction, exactly as Python does. Aware
-- represents an ordinary ZonedTime with a distinct timezone object. Offset
-- also supports second/microsecond offsets which TimeZone cannot represent.
data CalendarTime
  = Naive LocalTime
  | AtJST LocalTime
  | Aware ZonedTime
  | Offset LocalTime NominalDiffTime
  deriving (Show)

validDay :: Day -> Bool
validDay d = let (y, _, _) = toGregorian d in y >= 1 && y <= 9999

checkDay :: Day -> Either CalendarError Day
checkDay d = if validDay d then Right d else Left CalendarValueError

shiftDay :: Integer -> Day -> Either CalendarError Day
shiftDay n d = let out = addDays n d
               in if validDay out then Right out else Left CalendarOverflowError

checkLocal :: LocalTime -> Either CalendarError LocalTime
checkLocal value = do
  _ <- checkDay (localDay value)
  let TimeOfDay h m s = localTimeOfDay value
  if h >= 0 && h < 24 && m >= 0 && m < 60 && s >= 0 && s < 60
    then Right value else Left CalendarValueError

checkConverted :: LocalTime -> Either CalendarError LocalTime
checkConverted value = if validDay (localDay value)
                       then Right value else Left CalendarOverflowError

-- | Logical day changes at 05:00 JST. No host timezone is read.
dayOf :: CalendarTime -> Either CalendarError HouseDay
dayOf input = do
  local <- case input of
    Naive value -> checkLocal value
    AtJST value -> checkLocal value
    Aware value -> convert (zonedTimeToLocalTime value)
                    (fromIntegral (timeZoneMinutes (zonedTimeZone value) * 60))
    Offset value seconds -> convert value seconds
  maybe (Left CalendarOverflowError) Right (houseDayFromJST local)
  where
    convert value seconds = do
      _ <- checkLocal value
      if abs seconds < 86400 then pure () else Left CalendarValueError
      -- Python datetime.astimezone first constructs a UTC datetime, whose
      -- date must itself fit 1..9999, before adding the target offset.
      utcValue <- checkConverted (addLocalTime (-seconds) value)
      checkConverted (addLocalTime (9 * 3600) utcValue)

dayOfJST :: LocalTime -> Either CalendarError HouseDay
dayOfJST = dayOf . AtJST

yearText :: Integer -> Text
yearText year = T.justifyRight 4 '0' (T.pack (show year))

isoDay :: Day -> Text
isoDay d = let (y, m, k) = toGregorian d
               two n = T.justifyRight 2 '0' (T.pack (show n))
           in yearText y <> "-" <> two m <> "-" <> two k

mark :: HouseDay -> Either CalendarError Text
mark house = do
  let d = houseDayDate house
  _ <- checkDay d
  pure ("[sazanami " <> isoDay d <> "]")

weekday :: Day -> Int
weekday d = let (_, _, w) = toWeekDate d in w - 1

md :: Day -> Either CalendarError Text
md d = do
  _ <- checkDay d
  let (_, m, k) = toGregorian d
  pure (T.pack (show m) <> "/" <> T.pack (show k) <> "（"
        <> T.singleton (T.index "月火水木金土日" (weekday d)) <> "）")

foreign import ccall unsafe "fmod" c_fmod :: CDouble -> CDouble -> CDouble

-- CPython float remainder uses fmod, then corrects its sign. Substituting
-- x - floor(x/y)*y changes the low bits of the solar longitude calculation.
pyModulo :: Double -> Double -> Double
pyModulo x y =
  let CDouble r = c_fmod (CDouble x) (CDouble y)
  in if r /= 0 then if (r < 0) /= (y < 0) then r + y else r
     else if y < 0 then -0.0 else 0.0

-- Division part of CPython's _float_div_mod; it intentionally snaps the
-- quotient nearest an integer after removing the fmod remainder.
pyFloorDiv :: Double -> Double -> Double
pyFloorDiv x y =
  let CDouble r = c_fmod (CDouble x) (CDouble y)
      q = (x - r) / y
      q' = if r /= 0 && (r < 0) /= (y < 0) then q - 1 else q
  in if isNaN q' || isInfinite q' then q'
     else if q' /= 0
          then let n = fromInteger (floor q')
               in if q' - n > 0.5 then n + 1 else n
          else if (x < 0 || isNegativeZero x) /= (y < 0 || isNegativeZero y)
               then -0.0 else 0.0

-- | Apparent solar longitude at local midnight. Every multiplication and
-- addition retains the Python source's association; do not algebraically
-- simplify, enable fast-math, or replace degrees-to-radians with x*pi/180.
sun :: Day -> Double -> Either CalendarError Double
sun d tz = do
  _ <- checkDay d
  let ordinal = diffDays d (fromGregorian 1 1 1) + 1
      jd = fromIntegral ordinal + 1721424.5 - tz / 24 + 69.2 / 86400
      t = (jd - 2451545.0) / 36525
      radians x = x * (pi / 180)
      m = radians (357.52911 + 35999.05029 * t - 0.0001537 * t * t)
      c = (1.914602 - 0.004817 * t - 0.000014 * t * t) * sin m
          + (0.019993 - 0.000101 * t) * sin (2 * m) + 0.000289 * sin (3 * m)
      om = radians (125.04 - 1934.136 * t)
      longitude = 280.46646 + 36000.76983 * t + 0.0003032 * t * t + c
                  - 0.00569 - 0.00478 * sin om
  -- math.sin rejects infinities but permits NaN. Very large synthetic tz
  -- arguments can overflow an intermediate even though the date is valid.
  if any isInfinite [m, 2 * m, 3 * m, om]
    then Left CalendarValueError else pure (pyModulo longitude 360)

sekkiNames :: [Text]
sekkiNames = ["春分", "清明", "穀雨", "立夏", "小満", "芒種", "夏至", "小暑", "大暑", "立秋", "処暑", "白露",
              "秋分", "寒露", "霜降", "立冬", "小雪", "大雪", "冬至", "小寒", "大寒", "立春", "雨水", "啓蟄"]

sekki :: Day -> Either CalendarError (Maybe Text)
sekki d = sekkiAt d 9

sekkiAt :: Day -> Double -> Either CalendarError (Maybe Text)
sekkiAt d tz = do
  first <- sun d tz
  tomorrow <- shiftDay 1 d
  second <- sun tomorrow tz
  let a = pyFloorDiv first 15
      b = pyFloorDiv second 15
  if a == b then pure Nothing
  else if isNaN b then Left CalendarValueError
  else if isInfinite b then Left CalendarOverflowError
  else pure (Just (sekkiNames !! truncate b))

-- Ordered dictionary assignment: overwrite keeps the old insertion slot.
assign :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
assign key value pairs
  | any ((== key) . fst) pairs = map (\(k, v) -> (k, if k == key then value else v)) pairs
  | otherwise = pairs ++ [(key, value)]

-- | Python dict insertion order is retained. For repeated lookups in a
-- rendering, obtain this once per year and pass it to 'whatDayWith'; the
-- storage/renderer layer owns the memoization that Python's lru_cache did.
holidays :: Integer -> Either CalendarError [(Day, Text)]
holidays y
  | y < 1 || y > 9999 = Left CalendarValueError
  | otherwise = do
      spring <- equinox "春分" 3
      autumn <- equinox "秋分" 9
      let base =
            [ (date 1 1, "元日"), (mon 1 2, "成人の日"), (date 2 11, "建国記念の日")
            , (date 2 23, "天皇誕生日"), (spring, "春分の日"), (date 4 29, "昭和の日")
            , (date 5 3, "憲法記念日"), (date 5 4, "みどりの日"), (date 5 5, "こどもの日")
            , (mon 7 3, "海の日"), (date 8 11, "山の日"), (mon 9 3, "敬老の日")
            , (autumn, "秋分の日"), (mon 10 2, "スポーツの日"), (date 11 3, "文化の日")
            , (date 11 23, "勤労感謝の日") ]
          initial = foldl' (\out (d, text) -> assign d text out) [] base
      -- Membership and Sunday replacement searches use the original h,
      -- deliberately not out. Existing substitutions are not skipped.
      foldM (addRest initial) initial (sortOn fst initial)
  where
    date = fromGregorian y
    mon m n = let d = date m 1
              in addDays (fromIntegral ((7 - weekday d) `mod` 7 + 7 * (n - 1))) d
    equinox name month = choose [18 .. 25]
      where
        choose [] = Left CalendarStopIteration
        choose (k : ks) = do
          label <- sekki (date month k)
          if label == Just name then pure (date month k) else choose ks
    addRest base out (d, _) = do
      afterSunday <- if weekday d == 6
        then do
          start <- shiftDay 1 d
          replacement <- nextFree base start
          pure (assign replacement "振替休日" out)
        else pure out
      mid <- shiftDay 1 d
      if lookup mid base == Nothing
        then do
          after <- shiftDay 1 mid
          pure (if lookup after base /= Nothing && lookup mid afterSunday == Nothing
                then assign mid "国民の休日" afterSunday else afterSunday)
        else pure afterSunday
    nextFree base d = if lookup d base == Nothing then pure d
                     else shiftDay 1 d >>= nextFree base

lunar :: [(Integer, [(Int, (Int, Int))])]
lunar =
  [ (2026, [(1,(2,17)), (5,(6,15)), (7,(8,13)), (8,(9,11)), (9,(10,10))])
  , (2027, [(1,(2,6)), (5,(6,5)), (7,(8,2)), (8,(9,1)), (9,(9,30))])
  , (2028, [(1,(1,26)), (5,(5,24)), (7,(8,20)), (8,(9,19)), (9,(10,18))])
  , (2029, [(1,(2,13)), (5,(6,12)), (7,(8,10)), (8,(9,8)), (9,(10,8))])
  , (2030, [(1,(2,3)), (5,(6,1)), (7,(7,30)), (8,(8,29)), (9,(9,27))]) ]

sekku :: Day -> Either CalendarError (Maybe Text)
sekku d = do
  _ <- checkDay d
  let (y, month, day) = toGregorian d
  if (month, day) == (10, 1) then pure (Just "国慶節")
  else case lookup y lunar of
    Nothing -> pure Nothing
    Just months ->
      let occasions = [(1,-1,"除夕"), (1,0,"春節"), (1,14,"元宵"), (5,4,"端午"),
                       (7,6,"七夕（旧暦）"), (8,14,"中秋"), (9,8,"重陽")]
          matches (m, k, _) = case lookup m months of
            Just (a, b) -> addDays k (fromGregorian y a b) == d
            Nothing -> False
      in pure (case find matches occasions of Just (_, _, name) -> Just name; Nothing -> Nothing)

whatDay :: Day -> Either CalendarError [Text]
whatDay d = do
  _ <- checkDay d
  let (y, _, _) = toGregorian d
  yearHolidays <- holidays y
  whatDayWith yearHolidays d

-- | Holiday lookup precedes solar/lunar labels; both latter computations
-- still run before an equinox's redundant solar label is removed.
whatDayWith :: [(Day, Text)] -> Day -> Either CalendarError [Text]
whatDayWith yearHolidays d = do
  _ <- checkDay d
  solar <- sekki d
  lunarLabel <- sekku d
  let holiday = lookup d yearHolidays
      solarLabel = if holiday `elem` [Just "春分の日", Just "秋分の日"] then Nothing else solar
  pure (catMaybes [holiday, solarLabel, lunarLabel])
