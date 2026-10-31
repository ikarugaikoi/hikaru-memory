{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hikaru.OwnTime.Runtime
  ( RunEnv(..), stateDir, dokoFile, homeWifi, writeAtomic, writeIfChanged, Load(..), loadObject, loadParsed, loadOr,
    withLock, jstZone, clockJST, dayStartOf, houseDayOf, runLog, appendLog, appendTimedLog, runDirOf,
    userInboxDir, withRunRecord, withRecordBytes, writeSealed, writeTextRecord, readRecordBytes, readSealedJson, PieceRead(..),
    loadedOr, noteTalk, noteTalkLocked, loadMarks, readTalk, missingAsNothing, loadOrFail, filePresent, mergeTalk
  ) where

import Control.Monad (unless, void)
import Data.List (nub, sort)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Hikaru.Compat.Base64 (decodeBase64)
import Hikaru.Config (Config(dataRoot))
import Hikaru.HouseDay (HouseDay, houseDayDate, houseDayFromJST)
import Hikaru.OwnTime.Trigger
import Hikaru.OwnTime.Store
import Hikaru.Sealed (Plain(..), SealError(..), sealedBytes, unsealBytes)
import qualified Hikaru.Store.Coordinator as Store
import Hikaru.Store.Environment (configuredStore)
import Hikaru.Store.Sealed (sealText)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.Posix.Files (getFileStatus, isDirectory)
import Hikaru.OwnTime.Errors (invalidFile)
import Hikaru.OwnTime.Files

-- 置き場所 ---------------------------------------------------------------------

dokoFile :: IO FilePath
dokoFile = do
  home <- getHomeDirectory
  fromMaybe (home </> ".local" </> "state" </> "hikaru" </> "doko.json") <$> lookupEnv "HIKARU_DOKO_FILE"

homeWifi :: IO T.Text
homeWifi = maybe "uchi" T.pack <$> lookupEnv "HIKARU_HOME_WIFI"

-- | ファイルが、ない／読めた／読めない（壊れている）。ないことと読めないことは分ける。
data Load a = Absent | Got a | Bad String

-- 初めてのファイルだけは、まだなくてよい。読めないファイルとは区別する。
filePresent :: FilePath -> IO Bool
filePresent path = maybe False (not . isDirectory) <$> missingAsNothing (getFileStatus path)

loadObject :: FilePath -> IO (Load A.Object)
loadObject path = do
  raw <- missingAsNothing (B.readFile path)
  pure $ case raw of
      Nothing -> Absent
      Just bytes -> case A.eitherDecodeStrict bytes of
        Right (A.Object o) -> Got o
        Right _ -> Bad "expected a JSON object"
        Left err -> Bad ("invalid JSON: " ++ err)

loadParsed :: (A.Object -> Either String a) -> FilePath -> IO (Load a)
loadParsed parse path = do
  loaded <- loadObject path
  pure $ case loaded of
    Absent -> Absent
    Bad why -> Bad why
    Got object -> either Bad Got (parse object)

loadOr :: a -> Load a -> Either String a
loadOr absent Absent = Right absent
loadOr _ (Got value) = Right value
loadOr _ (Bad why) = Left why

loadOrFail :: FilePath -> a -> Load a -> IO a
loadOrFail path initial loaded =
  either (invalidFile path) pure (loadOr initial loaded)

jstZone :: TimeZone
jstZone = minutesToTimeZone 540

clockJST :: UTCTime -> String
clockJST = formatTime defaultTimeLocale "%H:%M" . utcToLocalTime jstZone

appendLog :: FilePath -> UTCTime -> String -> IO ()
appendLog dir = appendTimedLog "%Y-%m-%d %H:%M" (dir </> "dryrun.log")

-- | その時刻の HouseDay の始まり（日本時間の 5:00）。
dayStartOf :: UTCTime -> IO UTCTime
dayStartOf now = do
  hd <- houseDayOf now
  pure (localTimeToUTC jstZone (LocalTime (houseDayDate hd) (TimeOfDay 5 0 0)))

houseDayOf :: UTCTime -> IO HouseDay
houseDayOf now = maybe exitFailure pure (houseDayFromJST (utcToLocalTime jstZone now))

runDirOf :: FilePath -> Run -> FilePath
runDirOf work r = let RunId n = runId r in work </> "runs" </> show n

data RunEnv = RunEnv
  { reDir    :: FilePath
  , reWork   :: FilePath
  , reConfig :: Config
  , reTejun  :: FilePath   -- ^ 手順書
  , reToken  :: FilePath   -- ^ 令牌のファイル
  }

userInboxDir :: RunEnv -> FilePath
userInboxDir env = dataRoot (reConfig env) </> "inbox"


-- runner.lock is outermost; ordinary record locks are innermost. OwnTime
-- never acquires .collect, and core writers never acquire runner.lock.
withRunRecord :: RunEnv -> FilePath -> (Store.StoreEnv -> IO a) -> IO a
withRunRecord env path action = do
  store <- configuredStore (reConfig env)
  Store.withRecordLock store False path (const (action store))

-- | ない／読めた／読めない（封が解けない、JSON でない）を分ける。読めないものを「ない」として上書きしないため
-- （2026-10-28、Codex の審査 38）。
data PieceRead = PieceMissing | PieceLoaded A.Object | PieceBroken String

readSealedJson :: FilePath -> IO PieceRead
readSealedJson path = do
  raw <- missingAsNothing (B.readFile path)
  pure $ case raw of
    Nothing -> PieceMissing
    Just b -> case unsealBytes b of
      Left (BadBase64 _) -> PieceBroken "invalid base64 encoding"
      Left (BadUtf8 _) -> PieceBroken "invalid UTF-8 in sealed JSON"
      Right (Plain text) -> case A.decodeStrict (TE.encodeUtf8 text) of
        Just (A.Object o) -> PieceLoaded o
        _ -> PieceBroken "expected a JSON object in sealed record"

loadedOr :: PieceRead -> Maybe A.Object
loadedOr (PieceLoaded o) = Just o
loadedOr _ = Nothing

writeSealed :: FilePath -> B.ByteString -> IO ()
writeSealed path plain = writeAtomic path (BL.fromStrict (Base64.encode plain))

-- Footprints historically accept arbitrary bytes, including invalid UTF-8.
-- Keep that byte contract here; text records use Hikaru.Sealed.
readRecordBytes :: FilePath -> IO (Either () B.ByteString)
readRecordBytes path = either (const (Left ())) Right . decodeBase64 <$> B.readFile path

writeTextRecord :: FilePath -> T.Text -> IO ()
writeTextRecord path text = do
  sealed <- sealText path text
  writeAtomic path (BL.fromStrict (sealedBytes sealed))

-- | 見た一言の時刻を進める（戻さない）。別の会話に同じ時刻の一言があれば tied。直近一日ぶんの時刻も残す。
-- talk.json が壊れていたら、書き換えずに止まる。
noteTalk :: FilePath -> T.Text -> [UTCTime] -> IO ()
noteTalk dir sid times = unless (null times) $ withLock (dir </> "talk.lock") $
  void (noteTalkLocked dir sid times)

-- Caller holds talk.lock. Even with no new times, read before publishing a cache
-- or presence derived from this talk state.
noteTalkLocked :: FilePath -> T.Text -> [UTCTime] -> IO TalkFile
noteTalkLocked dir sid times = do
  old <- readTalk dir
  if null times then pure old else do
    let pruned = mergeTalk sid times old
    writeAtomic (dir </> "talk.json") (A.encode (talkValue pruned))
    pure pruned

mergeTalk :: T.Text -> [UTCTime] -> TalkFile -> TalkFile
mergeTalk sid times old
  | null times = old
  | otherwise =
      let merged = foldl merge old (sort times)
          latest = maximum (tfRecent merged)
      in merged { tfRecent = filter (>= addUTCTime (-86400) latest) (tfRecent merged) }
  where
    merge tf t =
      let recent = sort (nub (t : tfRecent tf))
      in case tfLast tf of
        Just (Heard s0 t0)
          | t < t0 -> tf { tfRecent = recent }
          | t == t0 -> tf { tfTied = tfTied tf || s0 /= Just sid, tfRecent = recent }
        _ -> tf { tfLast = Just (Heard (Just sid) t), tfTied = False, tfRecent = recent }



loadMarks :: FilePath -> IO (Load (Marks, [String], Bool))
loadMarks dir = loadParsed parseMarks (dir </> "presence.json")

-- | talk.json を読む（なければ空）。ロックの中で読むために分けてある。
readTalk :: FilePath -> IO TalkFile
readTalk dir = do
  l <- loadParsed parseTalk (dir </> "talk.json")
  loadOrFail (dir </> "talk.json") emptyTalk l


withRecordBytes :: RunEnv -> FilePath -> B.ByteString -> IO a -> (Store.StoreEnv -> B.ByteString -> IO a) -> IO a
withRecordBytes env path fresh broken action = withRunRecord env path $ \storeEnv -> do
  createDirectoryIfMissing True (takeDirectory path)
  old <- missingAsNothing (readRecordBytes path)
  let body = maybe (Right fresh) id old
  either (const broken) (action storeEnv) body
