{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Hikaru.Entry (Invocation(..), Usage(..), parseInvocation)
import Hikaru.Config (Config (..), configuredLogScan, configuredNow, loadConfig)
import Hikaru.Ima.Now (runNowWithLogScan)
import Hikaru.Oshirase.Notice (NoticeEnv(..), runNoticeWithScan)
import Hikaru.Compat.Json (pyStringNull,pyStringFromChars,pyStringFromText,pyStringToText)
import Hikaru.Compat.Stderr (writeStderrLine)
import Hikaru.Compat.Hash (loadHashContext)
import Hikaru.Ochibo.CLI (CollectEnv(..),runCollect)
import Hikaru.Ochibo.Read (CollectReadEnv(..))
import Hikaru.Shirushi.Workflow (MarkEnv(..),runArguments)
import Hikaru.Store.Coordinator (StoreEnv(..))
import Hikaru.Store.Edit (EditStopped(..))
import Hikaru.Store.Environment
import qualified Hikaru.Store.Paging as Paging
import Hikaru.RecordRead (readRecord)
import Hikaru.Koyomi.Service (SazanamiEnv(..), sazanami)
import Hikaru.Weather (WeatherError, configuredWeather, fillWeather)
import Hikaru.Weather.Transport (TransportFailure)
import qualified Hikaru.Fuutou.CLI as Seal
import qualified Hikaru.Fuutou.Entry as SealEntry
import Hikaru.Fuutou.Core (SealEnv(..), loadMarkSessions)
import Hikaru.Shirushi.Types (MarkError(..))
import Hikaru.Conversation.DateTime (dateTimeLocal)
import Hikaru.Conversation.JSONL (LogError(..))
import Hikaru.Compat.Exception (fromPython)
import Data.Time (localTimeToUTC, utc)
import qualified Data.Text as T
import Hikaru.Fuutou.Record (RecordEnv(..))
import qualified Hikaru.Mezame.Workflow as Wake
import Hikaru.Output (outputText)
import Hikaru.Mezame.Input (InputError(..))
import qualified Hikaru.Mezame.Progress as WakeProgress
import Hikaru.Choubo.Context (consumerContext)
import Hikaru.Compat.FileSystem (pathExists)
import Hikaru.Compat.Diagnostic (Diagnostic(..),exceptionDiagnostic,renderDiagnostic)
import qualified Hikaru.Choubo.Store as LogScan
import Control.Exception (Handler(..),catches,throwIO,SomeException,SomeAsyncException,fromException,displayException)
import System.Environment (getArgs,getProgName,lookupEnv)
import System.Exit (ExitCode(ExitFailure),die,exitWith,exitFailure)
import System.FilePath ((</>))
import System.Directory (makeAbsolute)
import System.IO (hSetEncoding, hPutStrLn, stdin, stdout, stderr, utf8)

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  dispatch `catches`
    [ Handler(\(EditStopped message)->writeStderrLine message >> exitFailure)
    , Handler(\(Wake.WakeExit message)->stop message)
    , Handler(\(InputError message)->stop message)
    , Handler(\(LogError message)->stop message)
    , Handler(\(err :: WeatherError)->stop (T.pack (displayException err)))
    , Handler(\(err :: TransportFailure)->stop (T.pack (displayException err)))
    , Handler(\(MarkError detail)->stop ("mark: " <> detail))
    , Handler commandFailure ]

stop :: T.Text -> IO ()
stop message=writeStderrLine (pyStringFromText message) >> exitFailure

-- Language stack frames are implementation details; retain the original
-- exception category/message and process failure instead of leaking GHC's
-- generic runtime stack. Cancellation and explicit exit codes pass through.
commandFailure :: SomeException -> IO ()
commandFailure err
  | Just code <- (fromException err :: Maybe ExitCode) = exitWith code
  | Just _ <- (fromException err :: Maybe SomeAsyncException) = throwIO err
  | otherwise = do
      diagnostic<-exceptionDiagnostic err
      let originalClass=diagnosticClass diagnostic
          -- Python's uncaught display includes binascii.Error's module; its
          -- structured exception type remains the unqualified name Error.
          cls=if originalClass=="Error" then "binascii.Error" else originalClass
          message | originalClass=="SystemExit"=diagnosticMessage diagnostic
                  | pyStringNull(diagnosticMessage diagnostic)=pyStringFromText cls
                  | otherwise=renderDiagnostic(Diagnostic cls(diagnosticMessage diagnostic))
      writeStderrLine message
      exitFailure

dispatch :: IO ()
dispatch = do
  program <- getProgName
  invocation <- getArgs >>= either usage pure . parseInvocation program
  case invocation of
    InvalidateAll -> do
      config <- loadConfig
      scan <- configuredLogScan config
      LogScan.invalidateAll scan
    InvalidateConversation path -> do
      config <- loadConfig
      scan <- configuredLogScan config
      absolute <- makeAbsolute path
      LogScan.invalidateConversation scan absolute
    Ima -> do
      config <- loadConfig
      scan <- configuredLogScan config
      runNowWithLogScan scan (logRoot config) (projectIdentity config) configuredNow
    Oshirase -> do
      config <- loadConfig
      scan <- configuredLogScan config
      runNoticeWithScan scan (NoticeEnv (logRoot config) (dataRoot config </> ".丘の上.md.b64") configuredNow
                 (\now limit timeout -> do
                    weather <- configuredWeather config
                    let service = SazanamiEnv (dataRoot config </> ".虹天気.md.b64")
                                  (dataRoot config </> ".一週間フレンズ") weather
                    sazanami service now (Just (toInteger limit)) timeout)
                 (do text <- readRecord (dataRoot config </> ".丘の上.md.b64")
                     chunks <- either (ioError . userError . show) pure (Paging.pages 16000 (pyStringFromText text))
                     pure (length chunks)))
    Koyomi rest -> do
      config <- loadConfig
      weather <- configuredWeather config
      now <- configuredNow
      let path = dataRoot config </> ".虹天気.md.b64"
          service = SazanamiEnv path (dataRoot config </> ".一週間フレンズ") weather
      if rest == ["fill"] then do
        store <- configuredStore config
        count <- fillWeather store weather path now 8
        putStrLn ("虹天気に " ++ show count ++ " 日ぶん書き足しました。")
      else do
        text<-sazanami service now Nothing 4
        outputText text
        outputText "\n"
    Ochibo rest -> do
      config<-loadConfig
      store<-configuredStore config
      pictures<-configuredPictures config
      let reader=CollectReadEnv(logRoot config)(storeTimeMachine store </> ".exclude")
                   (storeTimeMachine store </> ".last")(projectIdentity config)
          fill=do weather<-configuredWeather config
                  now<-configuredNow
                  _<-fillWeather store weather (dataRoot config </> ".虹天気.md.b64") now 8
                  pure()
          env=CollectEnv store reader pictures (localTimeToUTC utc . dateTimeLocal <$> configuredConversationUTC) fill outputText
      runCollect env(map T.pack rest)
    Shirushi rest -> do
      config<-loadConfig
      store<-configuredStore config
      runArguments (markEnvironment store) (map T.pack rest)
    Mezame rest -> do
      config<-loadConfig
      scan<-configuredLogScan config
      let root=dataRoot config
          path=root </> ".虹天気.md.b64"
          session=T.pack . maybe "" id <$> lookupEnv "CLAUDE_CODE_SESSION_ID"
          native=either (throwIO . InputError) pure . pyStringToText
          fill _=pure ()
          sky limit timeout=do
            weather<-configuredWeather config
            fillNow<-configuredNow
            store<-configuredStore config
            _<-fillWeather store weather path fillNow timeout
            let service=SazanamiEnv path(root </> ".一週間フレンズ")weather
            skyNow<-configuredNow
            sazanami service skyNow limit timeout
          env=Wake.WakeEnv root(root </> ".一週間フレンズ")(root </> ".しおり.md.b64")
                (logRoot config)(projectIdentity config)session fill sky outputText
                (configuredStore config >>= WakeProgress.readProgress . storeProgress)
                readRecord
                (\err -> exceptionDiagnostic err >>= native . renderDiagnostic)
                (consumerContext "wake-signal-v3" (pyStringFromText $ projectIdentity config))
                pathExists
      Wake.runWakeWithScan scan env(map T.pack rest)
    Fuutou rest -> do
      plan <- either (const (hPutStrLn stderr SealEntry.languageOptionMessage >> exitWith (ExitFailure 2))) pure
        (SealEntry.planFuutou rest)
      config<-loadConfig
      SealEntry.runFuutou config plan $ \arguments -> do
        store<-configuredStore config
        work<-configuredWork config
        hashes<-loadHashContext
        let root=dataRoot config
            records=RecordEnv root(root </> ".一週間フレンズ")(storeTimeMachine store)(root </> ".ありあけ")(root </> ".しおり.md.b64")
            mark=markEnvironment store
            env=SealEnv records store work mark hashes outputPyString
        Seal.runArguments env(map pyStringFromChars arguments)
  where
    usage LedgerUsage = die "usage: hikaru-core choubo invalidate (--all | --conversation PATH)"
    usage ToolUsage = die "usage: hikaru-core {fuutou|mezame|ima|oshirase|ochibo|shirushi|koyomi|choubo} [arguments]"

-- Only this adapter knows the legacy clock/output/paging representations.
markEnvironment :: StoreEnv -> MarkEnv
markEnvironment store = MarkEnv
  (storeTimeMachine store) (loadMarkSessions store)
  (dateTimeLocal <$> configuredConversationJST)
  outputText
  (\body page -> fromPython (Paging.showPage (pyStringFromText body) page) >>= fromPython . pyStringToText)
