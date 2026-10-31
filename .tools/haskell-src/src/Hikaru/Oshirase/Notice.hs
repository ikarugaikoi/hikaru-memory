{-# LANGUAGE OverloadedStrings #-}
-- | IO shell for typed tool correlation and the independent daily marker.
module Hikaru.Oshirase.Notice
  ( NoticeEnv(..), NoticeInfo, NoticePath(..), runNotice, runNoticeWithScan
  , noticeWithInfo, noticeWithInfoWithScan, lastRead, lastReadWithScan
  , okaNote, sazanamiNote, markerGiven, markerGivenWithScan
  , lastReadOpened, markerGivenOpened, pathExists, fileMtime ) where

import Control.Concurrent (forkFinally, killThread, newEmptyMVar, readMVar, putMVar, tryReadMVar)
import Control.Exception (SomeException, SomeAsyncException, AsyncException(ThreadKilled),
  catch, evaluate, mask, onException, fromException, throwIO)
import Data.Aeson hiding (object)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.FilePath ((</>))
import System.IO (stdin, stdout, hFlush)
import System.Posix.Files (getFileStatus, modificationTimeHiRes)
import System.Timeout (timeout)
import System.Posix.Types (Fd)
import qualified Hikaru.OwnTime.Health as Health
import Hikaru.Hook.Path
import Hikaru.HouseDay (houseDayFromJST, houseDayDate)
import Hikaru.Hook.Input
import Hikaru.Hook.Log (checkedConsumer)
import Hikaru.Oshirase.NoticeScan
import qualified Hikaru.Choubo.Reader as Scan
import qualified Hikaru.Choubo.Store as Scan
import Hikaru.Choubo.Types

data NoticeEnv = NoticeEnv
  { noticeLogsRoot :: FilePath
  , noticeOkaPath :: FilePath
  , noticeClock :: IO ZonedTime
  , noticeSazanami :: ZonedTime -> Int -> Double -> IO Text
  , noticePageCount :: IO Int
  }

data NoticePath = NamedPath FilePath | DescriptorPath Fd deriving (Eq, Show)
data NoticeInfo = NoticeInfo (Maybe HookPath) (Maybe Text)
instance FromJSON NoticeInfo where
  parseJSON = withObject "UserPromptSubmit" $ \object ->
    NoticeInfo <$> (object .:? "transcript_path" .!= Null >>= optionalPath) <*> optionalText object "session_id"

pathExists :: NoticePath -> IO Bool
pathExists = pathPresent False . hookPath

hookPath :: NoticePath -> HookPath
hookPath (NamedPath path) = FilePathRef path
hookPath (DescriptorPath fd) = DescriptorRef fd

withNoticePath :: NoticePath -> (Scan.OpenedLog -> IO a) -> IO a
withNoticePath path action = consumePath ref (Scan.withLogSource (pathName ref) action)
  where ref = hookPath path

choosePath :: NoticeEnv -> NoticeInfo -> IO (Maybe NoticePath)
choosePath env (NoticeInfo preferred sid) = choose
  (maybe [] (pure . fromPath) preferred ++ maybe [] (\ident -> [NamedPath (noticeLogsRoot env </> T.unpack ident ++ ".jsonl")]) sid)
  where
    fromPath (FilePathRef path) = NamedPath path
    fromPath (DescriptorRef fd) = DescriptorPath fd
    choose [] = pure Nothing
    choose (path:rest) = do
      present <- pathExists path
      if present then pure (Just path) else choose rest

lastRead :: NoticePath -> IO (Maybe Double)
lastRead = lastReadWithScan (ScanEnv "" ScanOff)

lastReadWithScan :: ScanEnv -> NoticePath -> IO (Maybe Double)
lastReadWithScan env path = lastComplete 1 <$> readStateWithScan env path

readStateWithScan :: ScanEnv -> NoticePath -> IO ReadState
readStateWithScan env path = do
  digest <- Scan.contextDigest ["notice-pages-v6"]
  withNoticePath path $ \source -> scanState <$> readPagesOpened env digest source

lastReadOpened :: ScanEnv -> Text -> Scan.OpenedLog -> IO (ScanResult (Maybe Double))
lastReadOpened env digest source = do
  result <- readPagesOpened env digest source
  pure result {scanState = lastComplete 1 (scanState result)}

readPagesOpened :: ScanEnv -> Text -> Scan.OpenedLog -> IO (ScanResult ReadState)
readPagesOpened env digest source =
  Scan.scanConsumer env source (checkedConsumer (Scan.sourcePath source) (readConsumer digest))

fileMtime :: FilePath -> IO Double
fileMtime path = do
  value <- modificationTimeHiRes <$> getFileStatus path
  let whole = floor value :: Integer
      nanos = round ((value - fromInteger whole) * 1000000000) :: Integer
  pure (fromInteger whole + fromInteger nanos * 1e-9)

okaNote :: NoticeEnv -> Double -> IO Text
okaNote env seen = do
  present <- pathExists (NamedPath (noticeOkaPath env))
  if not present then pure "" else do
    changed <- fileMtime (noticeOkaPath env)
    if changed <= seen + 2 then pure "" else do
      let local = zonedTimeToLocalTime (utcToZonedTime jst (posixSecondsToUTCTime (realToFrac changed)))
          (_,month,day) = toGregorian (localDay local)
          tod = localTimeOfDay local
          showT = T.pack . show
          pad n = T.justifyRight 2 '0' (showT n)
      pure $
        "丘の上（.丘の上.md.b64）が、この会話で最後に読んだあとに書き換えられています"
        <> "（更新：" <> showT month <> "月" <> showT day <> "日 " <> pad (todHour tod) <> ":" <> pad (todMin tod)
        <> "）。ほかの会話で、ユーザーと一緒に決めたことが足されたのだと思います。"
        <> "返事をする前に `.tools/bin/fuutou show oka` で読み直して、変わったところに合わせてください。"
        <> "ページが分かれていたら、「これで最後」と出るまで続けて読んでください。`| tail` や `| sed` などにつないで途中を切ったものは、読んだことになりません。"
        <> "読み直したことは、ユーザーに言わなくていいです（裏方の話は会話に出さない）。"

markerGiven :: NoticePath -> Text -> IO Bool
markerGiven = markerGivenWithScan (ScanEnv "" ScanOff)

markerGivenWithScan :: ScanEnv -> NoticePath -> Text -> IO Bool
markerGivenWithScan env path marker =
  withNoticePath path $ \source -> scanState <$> markerGivenOpened env source marker

markerGivenOpened :: ScanEnv -> Scan.OpenedLog -> Text -> IO (ScanResult Bool)
markerGivenOpened env source marker = do
  digest <- Scan.contextDigest ["notice-marker-typed-v2", TE.encodeUtf8 marker]
  Scan.scanConsumer env source (checkedConsumer (Scan.sourcePath source) (markerConsumer marker digest))

sazanamiNote :: NoticeEnv -> NoticePath -> IO Text
sazanamiNote = sazanamiNoteUsing markerGiven

sazanamiNoteUsing :: (NoticePath -> Text -> IO Bool) -> NoticeEnv -> NoticePath -> IO Text
sazanamiNoteUsing reader env path = do
  clock <- noticeClock env
  let now = utcToZonedTime jst (zonedTimeToUTC clock)
  day <- maybe (ioError (userError "house day outside supported range")) pure
    (houseDayFromJST (zonedTimeToLocalTime now))
  let marker = "[sazanami " <> T.pack (show (houseDayDate day)) <> "]"
  given <- reader path marker
  if given then pure "" else noticeSazanami env now 9000 3

noticeWithInfo :: NoticeEnv -> NoticeInfo -> IO (Maybe Text)
noticeWithInfo = noticeWithInfoWithScan (ScanEnv "" ScanOff)

noticeWithInfoWithScan :: ScanEnv -> NoticeEnv -> NoticeInfo -> IO (Maybe Text)
noticeWithInfoWithScan scanEnv env info = do
  (note, acknowledged) <- prepareNotice scanEnv env info
  acknowledged
  pure note

prepareNotice :: ScanEnv -> NoticeEnv -> NoticeInfo -> IO (Maybe Text, IO ())
prepareNotice scanEnv env info@(NoticeInfo _ sid) = do
  health <- Health.hookNotice (maybe "unknown-session" id sid)
  case health of
    -- Deliver this one-time alert even when the ordinary transcript scan is
    -- the very input that stopped OwnTime. The next hook resumes ordinary notes.
    Just note -> pure (Just (Health.text note), Health.acknowledge note)
    Nothing -> do
      ordinary <- ordinaryNotice scanEnv env info
      pure (ordinary, pure ())

ordinaryNotice :: ScanEnv -> NoticeEnv -> NoticeInfo -> IO (Maybe Text)
ordinaryNotice scanEnv env info = do
  chosen <- choosePath env info
  case chosen of
    Nothing -> pure Nothing
    Just path -> do
      reading <- readStateWithScan scanEnv path
      seen <- case unpagedReading reading of
        Just known -> pure known
        Nothing -> do
          exists <- pathExists (NamedPath (noticeOkaPath env))
          total <- if exists then noticePageCount env else pure 1
          pure (lastComplete total reading)
      case seen of
        Nothing -> pure Nothing
        Just instant -> do
          oka <- okaNote env instant
          later <- workerNote (sazanamiNoteUsing (markerGivenWithScan scanEnv) env path)
          let notes = filter (not . T.null) (oka : maybe [] pure later)
          pure (if null notes then Nothing else Just (T.intercalate "\n\n" notes))

-- The worker's failure is an explicit result delivered to the outer hook.
-- No inner catch-and-continue handler is installed.
workerNote :: IO Text -> IO (Maybe Text)
workerNote work = do
  done <- newEmptyMVar
  mask $ \restore -> do
    worker <- forkFinally (restore (work >>= \text -> evaluate (T.length text) >> pure text)) (putMVar done)
    let stop = do
          killThread worker
          result <- readMVar done
          case result of
            Left err | Just ThreadKilled <- fromException err -> pure ()
                     | Just _ <- (fromException err :: Maybe SomeAsyncException) -> throwIO err
            _ -> pure ()
        outcome (Right text) = pure (Just text)
        outcome (Left err) = throwIO err
    waited <- restore (timeout 5000000 (readMVar done)) `onException` stop
    case waited of
      Just value -> outcome value
      Nothing -> do
        completed <- tryReadMVar done
        case completed of
          Just value -> outcome value
          Nothing -> stop >> pure Nothing

runNotice :: NoticeEnv -> IO ()
runNotice = runNoticeWithScan (ScanEnv "" ScanOff)

runNoticeWithScan :: ScanEnv -> NoticeEnv -> IO ()
runNoticeWithScan scanEnv env = action `catch` silence
  where
    action = do
      raw <- BS.hGetContents stdin
      case eitherDecodeStrict' raw of
        Left _ -> pure ()
        Right info -> do
          (note, acknowledged) <- prepareNotice scanEnv env info
          maybe (pure ()) (BS.hPut stdout . hookOutput "UserPromptSubmit") note
          maybe (pure ()) (const (hFlush stdout)) note
          acknowledged
    silence (err :: SomeException)
      | Just _ <- (fromException err :: Maybe SomeAsyncException) = throwIO err
      | otherwise = pure ()
