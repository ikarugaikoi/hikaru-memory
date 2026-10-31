{-# LANGUAGE OverloadedStrings #-}
-- | Deployment path/clock selection. The storage and business modules use
-- explicit environments and never consult machine-specific absolute paths.
module Hikaru.Store.Environment
  ( configuredStore, configuredWork, configuredPictures
  , configuredConversationUTC, configuredConversationJST, outputPyString ) where
import Data.Char (isAscii,isAlphaNum)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds,posixSecondsToUTCTime)
import Hikaru.Calendar (CalendarTime(AtJST),dayOf,calendarErrorName)
import Hikaru.Compat.Exception (fromPython)
import Hikaru.Compat.Diagnostic (encodeUtf8Diagnostic,raiseDiagnostic)
import Hikaru.Compat.Json
import Hikaru.Config
import Hikaru.Conversation.DateTime
import Hikaru.Store.Coordinator
import Hikaru.Store.Work
import System.Directory (makeAbsolute)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (stdout)
import System.Posix.User (getRealUserID)

environmentOr :: String -> String -> IO String
environmentOr key fallback=maybe fallback id <$> lookupEnv key
pathOr :: String -> FilePath -> IO FilePath
pathOr key fallback=environmentOr key fallback >>= makeAbsolute

configuredStore :: Config -> IO StoreEnv
configuredStore config=do
  runtime<-pathOr "HIKARU_RUNTIME_ROOT"(dataRoot config </> ".tools")
  locks<-pathOr "HIKARU_LOCK_ROOT"(runtime </> "locks")
  backups<-pathOr "HIKARU_BACKUP_ROOT"(runtime </> "bak")
  tm<-pathOr "HIKARU_TM_ROOT"(dataRoot config </> ".タイムマシン")
  let day=configuredNow >>= fromPython . either(Left . calendarErrorName)Right . dayOf . AtJST . zonedTimeToLocalTime
  pure(StoreEnv locks backups tm (tm </> ".sessions.json")(tm </> ".progress.json")day)

configuredWork :: Config -> IO WorkEnv
configuredWork config=do
  override<-lookupEnv "HIKARU_WORK"
  sid<-environmentOr "CLAUDE_CODE_SESSION_ID" ""
  uid<-getRealUserID
  scratch<-pathOr "HIKARU_SCRATCH_ROOT"("/private/tmp/claude-"++show uid)
  slug<-environmentOr "HIKARU_PROJECT_SLUG"(map(\c->if isAscii c&&isAlphaNum c then c else '-')(T.unpack(projectIdentity config)))
  temporary<-environmentOr "TMPDIR" "/tmp"
  pure(WorkEnv override sid scratch slug temporary)

configuredPictures :: Config -> IO FilePath
configuredPictures config=pathOr "HIKARU_PICTURE_ROOT"(dataRoot config </> ".おしばな")

-- Python datetime has microsecond resolution even when the OS clock is finer.
configuredConversationUTC :: IO DateTime
configuredConversationUTC=do
  now<-zonedTimeToUTC <$> configuredNow
  let micros=floor(utcTimeToPOSIXSeconds now*1000000)::Integer
      rounded=posixSecondsToUTCTime(fromRational(toRational micros/1000000))
  pure(DateTime(utcToLocalTime utc rounded)(FixedOffset 0))

configuredConversationJST :: IO DateTime
configuredConversationJST=do
  now<-configuredConversationUTC
  local<-dateTimeToJST now >>= fromPython
  pure(DateTime local ProjectJST)

outputPyString :: PyString -> IO ()
outputPyString value=either raiseDiagnostic(BS.hPut stdout)(encodeUtf8Diagnostic value)
