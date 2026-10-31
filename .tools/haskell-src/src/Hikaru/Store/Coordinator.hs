{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Central storage mechanics and ordered lock/backup/state operations.
-- All paths and day samples are explicit. This module never chooses a live
-- directory. Versioned editing composes these operations in Store.Edit.
module Hikaru.Store.Coordinator
  ( StoreEnv(..), withTargetLock, withRecordLock, backup, pruneBackups
  , readUtf8, writeUtf8, loadSessions, saveSessions, loadProgress, saveProgress
  , updateProgress, readJson, writeJson ) where

import Control.Exception (bracket, evaluate, finally)
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time (diffDays)
import Hikaru.HouseDay (HouseDay, houseDayDate)
import Foreign.C.Error (throwErrnoIfMinus1_, throwErrnoIfMinus1Retry_)
import Foreign.C.Types (CInt(..))
import Hikaru.Compat.Exception (fromPython)
import Hikaru.Compat.Diagnostic (Diagnostic(..),decodeUtf8Diagnostic,encodeUtf8Diagnostic,raiseDiagnostic)
import Hikaru.Compat.Hash (HashContext)
import Hikaru.Compat.FileSystem (pathExists,pathIsDirectory)
import Hikaru.Compat.Json
import Hikaru.Compat.Render (dumpJson)
import Hikaru.Compat.Time (parseDateOnly)
import Hikaru.Compat.Unicode (pyIsDecimal)
import qualified Hikaru.Store.State as State
import Hikaru.Store.Backup (saveOriginal)
import Hikaru.Store.Prune (removeTreeIgnoringErrors)
import System.Directory (createDirectoryIfMissing,listDirectory,renameFile)
import System.FilePath ((</>), takeDirectory, takeFileName, replaceExtension, normalise, dropTrailingPathSeparator)
import System.IO (IOMode(WriteMode), withBinaryFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.Files (getSymbolicLinkStatus)
import System.Posix.IO (OpenMode(WriteOnly), defaultFileFlags, trunc, creat, cloexec, openFd, closeFd)
import System.Posix.Types (Fd(..))

-- On Darwin/POSIX flock constants are LOCK_EX=2 and LOCK_UN=8. The binding
-- is only a syscall transport; lock naming/order/policy stay in Haskell.
foreign import ccall interruptible "flock" c_flock :: CInt -> CInt -> IO CInt

data StoreEnv = StoreEnv
  { storeLocks :: FilePath
  , storeBackups :: FilePath
  , storeTimeMachine :: FilePath
  , storeSessions :: FilePath
  , storeProgress :: FilePath
  , storeDay :: IO HouseDay
  }

-- pathlib removes trailing slashes and lexical '.' components before .name.
-- normalise deliberately retains '..' rather than resolving symlinks.
pathName :: FilePath -> FilePath
pathName = takeFileName . dropTrailingPathSeparator . normalise

withTargetLock :: StoreEnv -> FilePath -> (Fd -> IO a) -> IO a
withTargetLock env target action = do
  createDirectoryIfMissing True (storeLocks env)
  let path = storeLocks env </> (dropWhile (=='.') (pathName target) ++ ".lock")
      acquire = openFd path WriteOnly defaultFileFlags { trunc=True, creat=Just 0o666, cloexec=True }
  bracket acquire closeFd $ \fd@(Fd raw) ->
    (do throwErrnoIfMinus1Retry_ "flock LOCK_EX" (c_flock raw 2)
        action fd)
    `finally` throwErrnoIfMinus1_ "flock LOCK_UN" (c_flock raw 8)

-- The ordinary record lock is always the innermost lock. Callbacks finish
-- all borrowed numeric transactions before returning and releasing this FD.
withRecordLock :: StoreEnv -> Bool -> FilePath -> (Fd -> IO a) -> IO a
withRecordLock env timeMachine target action
  | timeMachine = withTargetLock env (storeTimeMachine env </> ".collect") $ \_ -> withTargetLock env target action
  | otherwise = withTargetLock env target action

backup :: StoreEnv -> FilePath -> IO ()
backup env target = do
  exists <- pathExists target
  when exists $ do
    day <- storeDay env
    saveOriginal (storeBackups env) (takeDirectory (storeTimeMachine env)) day target
    -- The baseline calls its clock again inside prune, even if today's
    -- backup was already present. Preserve that separate day-boundary sample.
    pruneBackups env 7

pruneBackups :: StoreEnv -> Integer -> IO ()
pruneBackups env keep = do
  exists <- pathExists (storeBackups env)
  when exists $ do
    today <- storeDay env
    names <- listDirectory (storeBackups env)
    forM_ names $ \name -> do
      let path = storeBackups env </> name
      directory <- pathIsDirectory path
      when (directory && ymd name) $ case parseDateOnly (T.pack name) of
        Left _ -> pure ()
        Right day -> when (diffDays (houseDayDate today) day > keep) $
          -- shutil.rmtree(ignore_errors=True) refuses a top-level symlink;
          -- it does not remove that link or recurse through its target.
          removeTreeIgnoringErrors path
  where
    ymd name = length name==10 && all pyIsDecimal (take 4 name)
               && name!!4=='-' && all pyIsDecimal (take 2 (drop 5 name))
               && name!!7=='-' && all pyIsDecimal (drop 8 name)

-- Python Path.read_text uses universal newline translation after strict UTF-8
-- decoding. No BOM stripping, Unicode normalization or replacement decoding.
readUtf8 :: FilePath -> IO PyString
readUtf8 path = do
  bytes <- BS.readFile path
  text <- either raiseDiagnostic pure (decodeUtf8Diagnostic bytes)
  pure (pyStringFromText (T.replace "\r" "\n" (T.replace "\r\n" "\n" text)))

writeUtf8 :: FilePath -> PyString -> IO ()
writeUtf8 path value = do
  -- A bad value must not truncate the previous file.
  bytes <- either raiseDiagnostic pure (encodeUtf8Diagnostic value)
  withBinaryFile path WriteMode (`BS.hPut` bytes)

readJson :: FilePath -> IO Value
readJson path = do
  text <- readUtf8 path >>= either unreadable pure . pyStringToText
  either unreadable pure (decodeJson text)
  where unreadable reason = raiseDiagnostic (Diagnostic "JSONDecodeError"
          (pyStringFromText ("invalid JSON file: " <> T.pack path <> ": " <> reason)))

writeJson :: Bool -> Maybe Int -> FilePath -> Value -> IO ()
writeJson sorted indentation path value = do
  -- Rendering and strict UTF-8 encoding both precede opening the file.
  -- This Value domain has JSON-string keys and no arbitrary encoder callbacks.
  let rendered = dumpJson False indentation sorted value
  _ <- evaluate (length (pyStringToChars rendered))
  writeUtf8 path rendered

loadSessions :: StoreEnv -> IO Value
loadSessions env = loadState (storeSessions env) emptyObject

-- Only ENOENT initializes persistent state. Parse errors, unreadable paths
-- and non-object JSON are failures, even when the JSON value is "empty".
loadState :: FilePath -> Value -> IO Value
loadState path initial = do
  exists <- (getSymbolicLinkStatus path >> pure True) `catchIOError` \err ->
    if isDoesNotExistError err then pure False else ioError err
  value <- if exists then readJson path else pure initial
  case value of
    Object _ -> pure value
    _ -> ioError (userError ("invalid persistent state (expected JSON object): " ++ path))

saveSessions :: StoreEnv -> Value -> IO ()
saveSessions env value = saveState env (storeSessions env) True 0 (pure value)

loadProgress :: StoreEnv -> IO Value
loadProgress env = State.loadProgressValue <$> loadState (storeProgress env) State.emptyProgress

saveProgress :: HashContext -> StoreEnv -> Value -> IO ()
saveProgress context env value =
  saveState env (storeProgress env) False 1 (fromPython (State.prepareProgress context value))

-- Preparation stays after directory creation and before opening the fixed
-- temporary file, including when malformed progress makes it fail.
saveState :: StoreEnv -> FilePath -> Bool -> Int -> IO Value -> IO ()
saveState env target sorted indentation prepare = do
  createDirectoryIfMissing True (storeTimeMachine env)
  value <- prepare
  let temporary = replaceExtension target "tmp"
  writeJson sorted (Just indentation) temporary value
  renameFile temporary target

updateProgress :: HashContext -> StoreEnv -> (Value -> IO Value) -> IO ()
updateProgress context env update = withTargetLock env (storeProgress env) $ \_ ->
  loadProgress env >>= update >>= saveProgress context env
