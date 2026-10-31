{-# LANGUAGE OverloadedStrings #-}
module Hikaru.Weather.Render (renderStoredWeather, renderSky, wholeDegrees, encodeCache) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Hikaru.Weather.Types
import Numeric (showFFloat)

-- Round the binary measurement once, with even ties. Keep a negative sign
-- when a sub-zero measurement rounds to zero.
fixed :: Int -> Double -> Text
fixed places value
  | isNaN value = "nan"
  | isInfinite value = if value < 0 then "-inf" else "inf"
  | otherwise = sign <> if places == 0 then digits else T.dropEnd places digits <> "." <> T.takeEnd places digits
  where
    sign = if value < 0 || isNegativeZero value then "-" else ""
    units = round (toRational (abs value) * fromInteger (10 ^ places)) :: Integer
    digits = T.justifyRight (places + 1) '0' (T.pack (show units))

wholeDegrees :: Double -> Text
wholeDegrees = fixed 0

renderSky :: Weather -> Text
renderSky (Weather word high low rain) = word <> " " <> wholeDegrees high <> "/" <> wholeDegrees low
  <> if rain >= 1 then " " <> wholeDegrees rain <> "mm" else ""

renderStoredWeather :: M.Map Day Weather -> Text
renderStoredWeather rows =
  "# 虹天気\n\nその日の空（多摩のあたり）。mezame と毎朝の ochibo が、足りない日を書き足す。出どころは Open-Meteo。\n日付｜空｜最高/最低（度）｜雨（mm）\n\n"
  <> T.concat [T.pack (show day) <> "｜" <> word <> "｜" <> fixed 1 high <> "/" <> fixed 1 low <> "｜" <> fixed 1 rain <> "\n"
              | (day,Weather word high low rain) <- M.toAscList rows]

-- The four keys, tuple shapes, UTF-8 words, separators and absence of a final
-- newline are part of the existing temporary-cache format.
encodeCache :: WeatherCache -> B.ByteString
encodeCache (WeatherCache epoch day (Sky current daily)) =
  "{\"t\": " <> decimal epoch <> ", \"day\": " <> encode (show day)
  <> ", \"cur\": " <> maybe "null" currentBytes current
  <> ", \"daily\": " <> maybe "null" dailyBytes daily <> "}"
  where
    currentBytes (CurrentWeather word temperature) = array [encode word, decimal temperature]
    dailyBytes (Weather word high low rain) = array [encode word,decimal high,decimal low,decimal rain]
    array values = "[" <> B.intercalate ", " values <> "]"
    decimal value | value == 0 || (abs value >= 1e-4 && abs value < 1e16) = B8.pack (showFFloat Nothing value "")
                  | otherwise = encode value
