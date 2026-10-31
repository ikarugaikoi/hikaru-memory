{-# LANGUAGE OverloadedStrings #-}
module Hikaru.OwnTime.Files
  ( stateDir, missingAsNothing, withLock, writeAtomic, writeIfChanged, appendTimedLog, runLog ) where

import Control.Monad (unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Maybe (fromMaybe)
import Data.Time
import GHC.IO.Handle.Lock (LockMode(ExclusiveLock), hLock)
import System.Directory (getHomeDirectory, renameFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (IOMode(ReadWriteMode), withFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.Process (getProcessID)

jstZone :: TimeZone
jstZone = minutesToTimeZone 540

stateDir :: IO FilePath
stateDir = do
  home <- getHomeDirectory
  fromMaybe (home </> ".local" </> "state" </> "hikaru" </> "owntime") <$> lookupEnv "HIKARU_OWNTIME_STATE"


writeAtomic :: FilePath -> BL.ByteString -> IO ()
writeAtomic path bytes = do
  pid <- getProcessID
  let tmp = path ++ ".tmp." ++ show pid
  BL.writeFile tmp bytes
  renameFile tmp path


missingAsNothing :: IO a -> IO (Maybe a)
missingAsNothing action = (Just <$> action) `catchIOError` \err ->
  if isDoesNotExistError err then pure Nothing else ioError err


-- | ほかの入口と同時に「読む・比べる・書く」をしないためのロック（そのファイルを閉じると外れる）。
withLock :: FilePath -> IO a -> IO a
withLock path action = withFile path ReadWriteMode $ \h -> hLock h ExclusiveLock >> action


appendTimedLog :: String -> FilePath -> UTCTime -> String -> IO ()
appendTimedLog format path at message =
  B.appendFile path (TE.encodeUtf8 (T.pack (formatTime defaultTimeLocale format (utcToLocalTime jstZone at) ++ " " ++ message ++ "\n")))


runLog :: FilePath -> UTCTime -> String -> IO ()
runLog dir = appendTimedLog "%Y-%m-%d %H:%M:%S" (dir </> "run.log")

-- | 中身が変わったときだけ書く（待っているあいだの十秒ごとの書き込みを減らす。Codex の審査 C6）。
writeIfChanged :: FilePath -> BL.ByteString -> IO ()
writeIfChanged path bytes = do
  old <- missingAsNothing (B.readFile path)
  unless (old == Just (BL.toStrict bytes)) (writeAtomic path bytes)

