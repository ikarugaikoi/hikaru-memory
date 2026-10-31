{-# LANGUAGE OverloadedStrings #-}
-- | I/O ordering for Sazanami: stored sky, all diary summaries, current
-- weather/cache, then rendering. The hook supplies its own clock and limit.
module Hikaru.Koyomi.Service (SazanamiEnv(..), sazanami, sazanamiAt) where

import Data.Time
import Control.Exception (throwIO)
import Data.Text (Text)
import Hikaru.Calendar (CalendarTime(..), dayOf)
import Hikaru.Diary.OneLine (readOneLines)
import Hikaru.Sazanami (renderSazanamiDetailed)
import Hikaru.Weather (WeatherEnv, WeatherError(..), todaySkyAt, readStoredWeather)

data SazanamiEnv = SazanamiEnv
  { sazanamiSora :: FilePath
  , sazanamiDiary :: FilePath
  , sazanamiWeather :: WeatherEnv
  }

sazanami :: SazanamiEnv -> ZonedTime -> Maybe Integer -> Double -> IO Text
sazanami env now = sazanamiAt env (AtJST (zonedTimeToLocalTime (utcToZonedTime (minutesToTimeZone 540) (zonedTimeToUTC now))))

sazanamiAt :: SazanamiEnv -> CalendarTime -> Maybe Integer -> Double -> IO Text
sazanamiAt env clock limit timeout = do
  day <- either (throwIO . WeatherCalendarError) pure (dayOf clock)
  stored <- readStoredWeather (sazanamiWeather env) (sazanamiSora env)
  history <- readOneLines (sazanamiDiary env)
  sky <- todaySkyAt (sazanamiWeather env) (sazanamiSora env) clock timeout
  either (throwIO . WeatherCalendarError) pure (renderSazanamiDetailed day stored history sky limit)
