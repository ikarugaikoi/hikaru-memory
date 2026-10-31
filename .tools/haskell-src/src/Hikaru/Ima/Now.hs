{-# LANGUAGE OverloadedStrings #-}
module Hikaru.Ima.Now (runNow, runNowWithClock, runNowWithLogScan, scanNowFile, isTask) where

import Control.Exception (throwIO)
import Control.Monad (foldM)
import Data.Aeson hiding (object)
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import System.IO (stdin, stdout, stderr, hIsTerminalDevice)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Posix.Types (Fd(..))
import Hikaru.Hook.Path
import Hikaru.Hook.Input
import Hikaru.Ima.NowScan (isTask, parseNowLine, advanceNow, nowResult)
import Hikaru.Hook.Log (checkedConsumer)
import Hikaru.Mezame.Input (InputError(..))
import Hikaru.Choubo.Consumer (Consumer(..))
import qualified Data.Text.Encoding as TE
import qualified Hikaru.Choubo.Reader as Scan
import qualified Hikaru.Choubo.Store as Scan
import qualified Hikaru.Choubo.Now as Scan
import Hikaru.Choubo.Types
import System.IO.Error (catchIOError, isDoesNotExistError)

data StartSource = Startup | Compact | Resume deriving Eq
data StartInfo = StartInfo (Maybe Text) StartSource (Maybe HookPath)
instance FromJSON StartInfo where
  parseJSON = withObject "SessionStart" $ \object -> do
    rawCurrent <- object .:? "session_id" :: Parser (Maybe Value)
    rawSource <- object .:? "source" :: Parser (Maybe Value)
    let current = case rawCurrent of
          Just (String value) | not (T.null value) -> Just value
          _ -> Nothing
        source = case rawSource of
          Just (String "compact") -> Compact
          Just (String "resume") -> Resume
          _ -> Startup
    path <- object .:? "transcript_path" .!= Null
    -- Ill-typed optional metadata means no usable reference. This does not
    -- catch failures of scanning, the clock or output after input typing.
    let fallback = either (const Nothing) id (parseEither optionalPath path)
    pure (StartInfo current source fallback)

scanNowFile :: Text -> FilePath -> IO (Maybe Bool, Maybe Text)
scanNowFile = scanNowCached (ScanEnv "" ScanOff)

scanNowCached :: ScanEnv -> Text -> FilePath -> IO (Maybe Bool, Maybe Text)
scanNowCached env identity path = Scan.withLogSource path $ \source -> do
  context <- Scan.contextDigest ["now-typed-v2", TE.encodeUtf8 identity]
  let consumer = (Scan.nowConsumer identity context)
        { consumerInput = either (throwIO . InputError) pure . parseNowLine identity . locatedText
        , consumerStep = \state line -> Right (Continue (advanceNow state line)) }
  nowResult . scanState <$> Scan.scanConsumer env source (checkedConsumer path consumer)

runNow :: FilePath -> Text -> ZonedTime -> IO ()
runNow logs identity now = runNowWithClock logs identity (pure now)

runNowWithClock :: FilePath -> Text -> IO ZonedTime -> IO ()
runNowWithClock = runNowWithLogScan (ScanEnv "" ScanOff)

runNowWithLogScan :: ScanEnv -> FilePath -> Text -> IO ZonedTime -> IO ()
runNowWithLogScan env logs identity clock = do
  raw <- BS.hGetContents stdin
  let decoded = case eitherDecodeStrict' raw :: Either String Value of
        Right value@(Object _) -> parseEither parseJSON value
        _ -> Right (StartInfo Nothing Startup Nothing)
  StartInfo current source fallback <- either (ioError . userError) pure decoded
  names <- listDirectory logs `catchIOError` \err ->
    if isDoesNotExistError err then pure [] else ioError err
  let paths = [logs </> name | name@(firstChar:_) <- names, firstChar /= '.', takeExtension name == ".jsonl"]
      scanPath state@(lastStamp, mine) path = do
        let mineHere = maybe False (`T.isInfixOf` T.pack path) current
        if mineHere && source == Startup then pure state else do
          (first, latest) <- scanNowCached env identity path
          pure (if first == Just False then max lastStamp latest else lastStamp,
                if mineHere then first else mine)
  (lastStamp, mine) <- foldM scanPath (Nothing, Nothing) paths
  now <- clock
  previous <- case lastStamp of
    Nothing -> pure []
    Just stamp | Right instant <- timestamp stamp -> do
      let (hours, minutes, days) = gapParts now instant
          showT = T.pack . show
      pure ["ユーザーが前回ここで最後に話したのは " <> formatNowTime (utcToZonedTime jst instant)
        <> "。そこから " <> showT hours <> "時間" <> showT minutes <> "分（暦の上では " <> showT days <> " 日）たっている。"]
    -- The agreed SessionStart contract still delivers the current clock when
    -- the prior timestamp cannot be read (2026-10-28 specification).
    Just _ -> pure []
  (compact, closedOutput) <- if source /= Compact then pure ([], Nothing) else do
    (first, closed) <- case mine of
      Just _ -> pure (mine, Nothing)
      Nothing -> do
        case fallback of
          Nothing -> pure (Nothing, Nothing)
          Just ref -> do
            exists <- pathPresent True ref
            if not exists then pure (Nothing, Nothing) else do
              terminal <- case ref of
                DescriptorRef (Fd 1) -> Just <$> hIsTerminalDevice stdout
                _ -> pure Nothing
              first <- consumePath ref (fst <$> scanNowFile identity (pathName ref))
              pure (first, terminal)
    pure ([if first == Just True
      then "定時タスクの続き：手引（SKILL.md）と .tools/bin/fuutou status を読み直して、そこから続ける。"
      else "圧縮のあとなので、.tools/bin/mezame をもう一度実行して全部読み直してから返事をする。"], closed)
  case closedOutput of
    Just False -> BS.hPut stderr "hook output descriptor was consumed\n" >> exitWith (ExitFailure 120)
    Just True -> ioError (userError "hook output descriptor was consumed")
    Nothing -> pure ()
  BS.hPut stdout (hookOutput "SessionStart" (T.intercalate "\n"
    (["今は " <> formatNowTime now <> "（日本時間）。"] ++ previous ++
     ["ユーザーの「今日」「明日」は寝る前後を基準にしていて、0時で切り替わるとは限らない。日付があいまいなときは確かめること。",
      "会話のルールは " <> identity <> "/.claude/CLAUDE.md にある。まだ読み込まれていなければ、話し始める前に読むこと。"] ++ compact)))
