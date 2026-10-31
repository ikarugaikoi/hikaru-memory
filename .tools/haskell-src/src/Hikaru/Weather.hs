{-# LANGUAGE OverloadedStrings #-}
-- | Thin I/O shell around typed weather parsing and pure cache/fill policy.
module Hikaru.Weather
  ( WeatherEnv(..), WeatherError(..), configuredWeather
  , readStoredWeather, todaySkyAt, fillWeather, fillWeatherAt
  ) where

import Control.Exception (Exception(..), IOException, bracket, throwIO, try)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Time
import Data.Void (absurd)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hikaru.Calendar (CalendarTime(..), CalendarError, calendarErrorName, dayOf)
import Hikaru.Config (Config, configuredEpoch)
import Hikaru.HouseDay (houseDayDate)
import Hikaru.Store.Sealed (sealText)
import Hikaru.RecordRead (readRecord)
import Hikaru.Store.Actions (Lock(..), replacement)
import Hikaru.Store.Coordinator (StoreEnv)
import Hikaru.Store.Execute (execute, locked)
import Hikaru.Weather.Parse
import Hikaru.Weather.Render (encodeCache, renderStoredWeather)
import Hikaru.Weather.Transport
import Hikaru.Weather.Types
import System.Directory (doesPathExist, getCurrentDirectory, makeAbsolute, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.IO (OpenMode(WriteOnly), defaultFileFlags, fdToHandle, openFd, trunc, creat, cloexec)

data WeatherError = StoredWeatherError String | CachedWeatherError String | WeatherCalendarError CalendarError
                  | ForecastError String
  deriving Show
instance Exception WeatherError where
  displayException (StoredWeatherError reason) = "ValueError: invalid stored weather: " ++ reason
  displayException (CachedWeatherError reason) = "ValueError: invalid weather cache: " ++ reason
  displayException (WeatherCalendarError reason) = T.unpack (calendarErrorName reason) ++ ": weather date outside supported range"
  displayException (ForecastError reason) = "ValueError: invalid weather API response: " ++ reason

data WeatherEnv = WeatherEnv
  { weatherCachePath :: FilePath
  , weatherEpoch :: IO Double
  , weatherTransport :: Transport
  }

configuredWeather :: Config -> IO WeatherEnv
configuredWeather _ = do
  explicit <- lookupEnv "HIKARU_WEATHER_CACHE"
  cache <- maybe ((</> "hikaru_sora.json") <$> temporaryDirectory) makeAbsolute explicit
  replay <- lookupEnv "HIKARU_WEATHER_REPLAY"
  pure (WeatherEnv cache configuredEpoch (maybe httpTransport replayTransport replay))

-- Retain the existing temp-directory selection when no cache is explicit:
-- TMPDIR, TEMP, TMP, then the standard POSIX directories and current folder.
-- A candidate has to support creating/writing a temporary file, just as before.
temporaryDirectory :: IO FilePath
temporaryDirectory = do
  candidates <- catMaybes <$> traverse lookupEnv ["TMPDIR","TEMP","TMP"]
  current <- getCurrentDirectory
  choose (candidates ++ ["/tmp","/var/tmp","/usr/tmp",current])
  where
    choose [] = ioError (userError "no usable temporary directory for weather cache")
    choose (path:rest) = do
      result <- try $ bracket (openBinaryTempFile path "weather-cache-check")
        (\(temporary,handle) -> hClose handle >> removeFile temporary)
        (\(_,handle) -> B.hPut handle "weather")
      case result of
        Left (err :: IOException) | isDoesNotExistError err -> choose rest
                                 | otherwise -> ioError err
        Right () -> makeAbsolute path

readStoredWeather :: WeatherEnv -> FilePath -> IO (M.Map Day Weather)
readStoredWeather _ path = do
  content <- (Just <$> readRecord path) `catchIOError` \err ->
    if isDoesNotExistError err then pure Nothing else ioError err
  case content of
    Nothing -> pure M.empty
    Just text -> either (throwIO . StoredWeatherError . ((path ++ ": ") ++)) pure (parseStoredWeather text)

fetch :: WeatherEnv -> Integer -> Double -> IO (Maybe Forecast)
fetch env past seconds = do
  result <- weatherTransport env (forecastRequest past) seconds
  case result of
    Left TimedOut -> pure Nothing
    Left Unavailable -> pure Nothing
    Left failure -> throwIO failure
    Right bytes -> either (throwIO . ForecastError) (pure . Just) (decodeForecast bytes)

remember :: WeatherEnv -> WeatherCache -> IO ()
remember env cache =
  -- Retain truncate-in-place, 0600 on creation, and unchanged existing mode.
  -- Permission/disk/close failures propagate before a history commit.
  bracket (openFd (weatherCachePath env) WriteOnly defaultFileFlags {trunc=True,creat=Just 0o600,cloexec=True} >>= fdToHandle)
          hClose (\handle -> BL.hPut handle (encodeCache cache))

todaySkyAt :: WeatherEnv -> FilePath -> CalendarTime -> Double -> IO Sky
todaySkyAt env _ clock seconds = do
  day <- either (throwIO . WeatherCalendarError) pure (dayOf clock)
  content <- (Just <$> B.readFile (weatherCachePath env)) `catchIOError` \err ->
    if isDoesNotExistError err then pure Nothing else ioError err
  epoch <- weatherEpoch env
  let cached = content >>= decodeCache epoch day . BL.fromStrict
  case cached of
    Just result -> either (throwIO . CachedWeatherError) pure result
    Nothing -> do
      received <- fetch env (diffDays (calendarDate clock) (houseDayDate day) + 1) seconds
      case received of
        Nothing -> pure noSky
        Just (Forecast current daily) -> do
          let sky = Sky current (M.lookup (houseDayDate day) daily)
          now <- weatherEpoch env
          remember env (WeatherCache now day sky)
          pure sky

fillWeather :: StoreEnv -> WeatherEnv -> FilePath -> ZonedTime -> Double -> IO Int
fillWeather store env path now = fillWeatherAt store env path
  (AtJST (zonedTimeToLocalTime (utcToZonedTime (minutesToTimeZone 540) (zonedTimeToUTC now))))

fillWeatherAt :: StoreEnv -> WeatherEnv -> FilePath -> CalendarTime -> Double -> IO Int
fillWeatherAt store env path clock seconds = locked store (TargetLock path) $ do
  day <- either (throwIO . WeatherCalendarError) pure (dayOf clock)
  have <- readStoredWeather env path
  case missingDays (calendarDate clock) day have of
    [] -> pure 0
    missing@(first:_) -> do
      received <- fetch env (diffDays (calendarDate clock) first) seconds
      case received of
        Nothing -> pure 0
        Just (Forecast current daily) -> do
          case M.lookup (houseDayDate day) daily of
            Nothing -> pure ()
            Just today -> do
              epoch <- weatherEpoch env
              remember env (WeatherCache epoch day (Sky current (Just today)))
          let extra = additions missing daily
          if M.null extra then pure 0 else do
            -- The whole read/modify/write uses the editor's lock domain.
            -- Its open-time fingerprint will therefore reject an older draft
            -- after this replacement; force never bypasses that check.
            present <- doesPathExist path
            sealed <- sealText path (renderStoredWeather (M.union extra have))
            execute store absurd TIO.putStr (replacement present path sealed)
            pure (M.size extra)

-- fetch/fill count days against the caller's calendar date, not the converted
-- house date. In particular an explicitly offset clock keeps its local date.
calendarDate :: CalendarTime -> Day
calendarDate (Naive value) = localDay value
calendarDate (AtJST value) = localDay value
calendarDate (Aware value) = localDay (zonedTimeToLocalTime value)
calendarDate (Offset value _) = localDay value
