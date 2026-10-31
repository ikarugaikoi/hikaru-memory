{-# LANGUAGE OverloadedStrings #-}
-- | Explicit dependencies of seal commands. No default paths or clock are
-- chosen here; the entrypoint composes the already-configured storage layers.
module Hikaru.Fuutou.Core
  ( SealEnv(..), say, outputText, sayText, requireMark
  , editEnvironment, validationEnvironment, notifyCommitted, perform, loadProgress, updateProgress
  , loadMarkSessions, loadSessions, readState, withProgress, readMarkIds
  , stopEdit, showPageText, readUtf8, makeDirectory ) where
import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Bifunctor (first)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Encoding as TE
import Data.Time (ZonedTime(..), minutesToTimeZone)
import qualified Data.Text as T
import Hikaru.Compat.Exception (fromPython)
import Hikaru.Compat.Hash (HashContext)
import Hikaru.Compat.FileSystem (pathIsDirectory)
import Hikaru.Compat.Json (PyString, Value, decodeJson, pyStringFromChars, pyStringFromText, pyStringToText)
import Hikaru.Shirushi.Id (Sessions, BlockId)
import qualified Hikaru.Shirushi.Types as Mark
import Hikaru.Shirushi.Workflow (MarkEnv(..))
import qualified Hikaru.Memory.Validation as Validation
import Hikaru.Fuutou.Record (RecordEnv(..),resolve,readOrNew,readOptional)
import qualified Hikaru.Store.State as Progress
import Hikaru.Store.Coordinator (StoreEnv)
import qualified Hikaru.Store.Coordinator as Store
import Hikaru.Store.Edit (EditEnv(..))
import qualified Hikaru.Store.Edit as Edit
import qualified Hikaru.Store.Paging as Paging
import Hikaru.Sealed (Plain(..))
import qualified Hikaru.Fuutou.Check as Check
import Hikaru.Store.Actions (Action(..), Lock(..), CommitCommand)
import Hikaru.Store.Execute (execute, locked, Committed, JournalResult(..), appendJournal)
import Hikaru.Store.Work (WorkEnv)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isAlreadyExistsError)

data SealEnv = SealEnv
  { sealRecords :: RecordEnv, sealStore :: StoreEnv, sealWork :: WorkEnv
  , sealMark :: MarkEnv, sealHashes :: HashContext
  , sealOutput :: PyString -> IO () }

-- Only these adapters cross the unchanged interfaces owned by other lanes.
say :: SealEnv -> String -> IO ()
say env text = sayText env (T.pack text)
outputText :: SealEnv -> T.Text -> IO ()
outputText env = sealOutput env . pyStringFromText
sayText :: SealEnv -> T.Text -> IO ()
sayText env text = outputText env (text <> "\n")
requireMark :: Mark.Result a -> IO a
requireMark = either throwIO pure
loadMarkSessions :: StoreEnv -> IO (Maybe Sessions)
loadMarkSessions store = loadSessions store >>= fmap Just . markValue
markValue :: (A.ToJSON a, A.FromJSON b) => a -> IO b
markValue value = case A.fromJSON (A.toJSON value) of
  A.Success result -> pure result
  A.Error message -> throwIO (Mark.MarkError (T.pack message))
readMarkIds :: Progress.Progress -> IO (S.Set BlockId)
readMarkIds = markValue . Progress.progressReadIds
stopEdit :: String -> IO a
stopEdit = Edit.stopEdit . pyStringFromChars
readUtf8 :: FilePath -> IO T.Text
readUtf8 path = Store.readUtf8 path >>= fromPython . pyStringToText
showPageText :: SealEnv -> T.Text -> Integer -> IO ()
showPageText env text page = fromPython (Paging.showPage (pyStringFromText text) page) >>= sealOutput env
makeDirectory :: FilePath -> IO ()
makeDirectory path = createDirectory path `catchIOError` (\err ->
  if isAlreadyExistsError err then pathIsDirectory path >>= \present -> unless present (ioError err) else ioError err)

validationEnvironment :: SealEnv -> Validation.ValidationEnv
validationEnvironment env=Validation.ValidationEnv(Store.storeDay(sealStore env))
  (resolve(sealRecords env))(recordRoot(sealRecords env) </> ".夕顔.md.b64")(sealOutput env)

editEnvironment :: SealEnv -> EditEnv
editEnvironment env = EditEnv (sealStore env) (sealWork env)
  (\path -> Plain <$> readOrNew (sealRecords env) path)
  (Check.review (validationEnvironment env)) Check.reviewAppend (outputText env)
  (notifyCommitted env)

notifyCommitted :: SealEnv -> String -> CommitCommand -> Committed -> IO ()
notifyCommitted env name command receipt = do
  result <- either (ioError . userError) pure (Check.afterCommit name receipt)
  case result of
    Nothing -> pure ()
    Just notice -> do
      let emit text = sayText env text
      emit (Check.kizukiText notice)
      saved <- appendJournal (sealStore env) (recordRoot (sealRecords env) </> ".tools/kizuki/close.jsonl.b64") $ do
        now <- ZonedTime <$> markNow (sealMark env) <*> pure (minutesToTimeZone 540)
        pure (Check.kizukiRows now command notice)
      case saved of
        JournalSaved -> pure ()
        JournalFailed -> emit "（この知らせを .tools/kizuki に書き留められませんでした）"

perform :: SealEnv -> [Action Progress.Progress] -> IO ()
perform env = execute (sealStore env)
  (Store.saveProgress (sealHashes env) (sealStore env) . Progress.encodeProgress)
  (outputText env)

-- Missing files initialize state; dangling links, unreadable bytes and invalid
-- JSON are errors. Only this boundary knows the persisted Value representation.
readState :: T.Text -> FilePath -> a -> (Value -> Progress.Result a) -> IO a
readState _message path initial decode = do
  bytes <- readOptional "cannot read state file" path (B.readFile path)
  case bytes of
    Nothing -> pure initial
    Just raw ->
      Progress.require (first (const ("UnicodeDecodeError: " <> T.pack path <> ": invalid UTF-8")) (TE.decodeUtf8' raw)
        >>= first (\reason -> "JSONDecodeError: " <> T.pack path <> ": " <> reason) . decodeJson >>= decode)
loadSessions :: StoreEnv -> IO Progress.Sessions
loadSessions store = readState Progress.sessionsError (Store.storeSessions store) M.empty Progress.decodeSessions
loadProgress :: SealEnv -> IO Progress.Progress
loadProgress env = readState Progress.progressError (Store.storeProgress (sealStore env)) Progress.initialProgress Progress.decodeProgress
withProgress :: SealEnv -> (Progress.Progress -> IO a) -> IO a
withProgress env action = locked (sealStore env) (TargetLock (Store.storeProgress (sealStore env))) (loadProgress env >>= action)

updateProgress :: SealEnv -> (Progress.Progress -> Progress.Result (Progress.Progress, a)) -> IO a
updateProgress env update = withProgress env $ \current -> do
  (next,result) <- Progress.require (update current)
  perform env [SaveProgress next]
  pure result
