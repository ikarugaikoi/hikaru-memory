{-# LANGUAGE OverloadedStrings #-}
-- | Weather values after the JSON/text boundary. Calendar days name readings;
-- the cache is explicitly keyed by the house day (which starts at 05:00).
module Hikaru.Weather.Types
  ( Weather(..), CurrentWeather(..), Sky(..), Forecast(..), noSky
  , WeatherCache(..), cacheIsFresh, cachedSky, missingDays, additions
  , WeatherRequest, forecastRequest, pastDays, requestURL
  ) where

import qualified Hikaru.Local.Settings as Local
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Time (Day, addDays, diffDays, fromGregorian)
import Hikaru.HouseDay (HouseDay, houseDayDate)

data Weather = Weather
  { weatherWord :: !Text
  , weatherHigh :: !Double
  , weatherLow :: !Double
  , weatherRain :: !Double
  } deriving (Eq, Show)

data CurrentWeather = CurrentWeather !Text !Double deriving (Eq, Show)
data Sky = Sky !(Maybe CurrentWeather) !(Maybe Weather) deriving (Eq, Show)
data Forecast = Forecast !(Maybe CurrentWeather) !(Map Day Weather) deriving (Eq, Show)

noSky :: Sky
noSky = Sky Nothing Nothing

data WeatherCache = WeatherCache
  { cacheEpoch :: !Double
  , cacheDay :: !HouseDay
  , cacheSky :: !Sky
  } deriving (Eq, Show)

-- Future timestamps intentionally remain hits, as in the original policy.
cachedSky :: Double -> HouseDay -> WeatherCache -> Maybe Sky
cachedSky epoch day cache
  | cacheIsFresh epoch day (cacheEpoch cache) (cacheDay cache) = Just (cacheSky cache)
  | otherwise = Nothing

cacheIsFresh :: Double -> HouseDay -> Double -> HouseDay -> Bool
cacheIsFresh epoch day timestamp storedDay = day == storedDay && epoch - timestamp < 3600

missingDays :: Day -> HouseDay -> Map Day Weather -> [Day]
missingDays calendarDay house have =
  [day | day <- takeWhile (< houseDayDate house) (iterate (addDays 1) first)
       , M.notMember day have, diffDays calendarDay day <= 92]
  where first = fromGregorian 2026 9 28

additions :: [Day] -> Map Day Weather -> Map Day Weather
additions missing received = M.restrictKeys received (M.keysSet (M.fromList [(d,()) | d <- missing]))

-- There is no path, text, coordinate, header or body supplied by the caller.
-- The only varying outbound information is a clamped count of past days.
newtype WeatherRequest = WeatherRequest Integer deriving (Eq, Show)

forecastRequest :: Integer -> WeatherRequest
forecastRequest = WeatherRequest . max 1 . min 92

pastDays :: WeatherRequest -> Integer
pastDays (WeatherRequest count) = count

requestURL :: WeatherRequest -> String
requestURL request =
  Local.weatherPrefix
  ++ "&daily=weather_code,temperature_2m_max,temperature_2m_min,precipitation_sum"
  ++ "&current=weather_code,temperature_2m&timezone=Asia%2FTokyo&past_days="
  ++ show (pastDays request) ++ "&forecast_days=1"
