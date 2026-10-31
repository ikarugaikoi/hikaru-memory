{-# LANGUAGE OverloadedStrings #-}
module Hikaru.Config
  ( Config (..), loadConfig, configuredLogScan, configuredNow, configuredEpoch ) where

import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString as B
import qualified Hikaru.Local.Settings as Local
import Control.Monad (unless)
import Data.Char (isAscii, isAlphaNum)
import qualified Data.Text as T
import Data.Time (TimeZone, ZonedTime, getCurrentTime, utc, utcToZonedTime, zonedTimeToUTC, minutesToTimeZone)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Hikaru.Compat.Time (parseClockInZone)
import Hikaru.Compat.Diagnostic (namedDiagnostic, raiseDiagnostic)
import Hikaru.Choubo.Types (ScanEnv(..), ScanMode(..))
import System.Directory (getHomeDirectory, makeAbsolute)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath ((</>), takeDirectory)

-- These boundaries are separate even when a deployment keeps them together.
-- The executable lives at <root>/.tools/bin/hikaru-core; its sibling symlinks
-- resolve to the same directory. Overrides remain available for isolated tests.
data Config = Config
  { codeRoot :: FilePath
  , dataRoot :: FilePath
  , logRoot :: FilePath
  , projectIdentity :: T.Text
  } deriving (Eq, Show)

environmentOr :: String -> String -> IO String
environmentOr key fallback = maybe fallback id <$> lookupEnv key

loadConfig :: IO Config
loadConfig = do
  executable <- getExecutablePath
  code <- environmentOr "HIKARU_CODE_ROOT" (takeDirectory (takeDirectory (takeDirectory executable))) >>= makeAbsolute
  settings <- B.readFile (code </> ".local" </> "privacy.json")
  unless (show (hash settings :: Digest SHA256) == Local.settingsHash) $
    ioError (userError "Local settings changed; rebuild Hikaru before using the tools")
  root <- environmentOr "HIKARU_ROOT" code >>= makeAbsolute
  records <- environmentOr "HIKARU_DATA_ROOT" root >>= makeAbsolute
  home <- getHomeDirectory
  let slug = map (\c -> if isAscii c && isAlphaNum c then c else '-') root
  logs <- environmentOr "HIKARU_LOG_ROOT" (records </> ".local" </> "logs") >>= makeAbsolute
  identity <- environmentOr "HIKARU_PROJECT_IDENTITY" root
  pure (Config code records logs (T.pack identity))

-- Rebuildable, text-free log state is separate from business records. Loading
-- this setting does not create directories or inspect any source transcript.
configuredLogScan :: Config -> IO ScanEnv
configuredLogScan config = do
  modeName <- environmentOr "HIKARU_LOG_SCAN_MODE" "append"
  mode <- case modeName of
    "off" -> pure ScanOff
    "append" -> pure ScanAppend
    "verify" -> pure ScanVerify
    _ -> ioError (userError "invalid HIKARU_LOG_SCAN_MODE: expected off, append or verify")
  root <- environmentOr "HIKARU_LOG_SCAN_ROOT" (dataRoot config </> ".log-scan") >>= makeAbsolute
  pure (ScanEnv root mode)

configuredNow :: IO ZonedTime
configuredNow = do
  fixed <- lookupEnv "HIKARU_FIXED_NOW"
  case fixed of
    Nothing -> utcToZonedTime (minutesToTimeZone 540) <$> getCurrentTime
    Just value -> replayClock (minutesToTimeZone 540) value

configuredEpoch :: IO Double
configuredEpoch = do
  fixed <- lookupEnv "HIKARU_FIXED_NOW"
  case fixed of
    Nothing -> realToFrac <$> getPOSIXTime
    Just value -> realToFrac . utcTimeToPOSIXSeconds . zonedTimeToUTC <$> replayClock utc value

-- Match the explicit Runtime clock's independent now(JST) and now(UTC) calls.
-- Do not obtain epoch through JST: UTC can still be valid when JST overflows.
-- A timestamp() call on the original aware value would also wrongly admit an
-- unrepresentable intermediate UTC datetime near year 1 or 9999.
replayClock :: TimeZone -> String -> IO ZonedTime
replayClock zone value = parseClockInZone zone (T.pack value) >>= either failure pure
  where failure "datetime conversion outside year range" = raiseDiagnostic (namedDiagnostic "OverflowError")
        failure _ = ioError (userError "invalid fixed clock")
