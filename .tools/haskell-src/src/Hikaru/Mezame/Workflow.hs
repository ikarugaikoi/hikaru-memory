{-# LANGUAGE OverloadedStrings #-}
-- | Read and type each section, assemble with pure functions, then render the
-- completed document. A failed read stops before any document is output.
module Hikaru.Mezame.Workflow
  ( WakeEnv(..), WakeExit(..), WakeDocument, Section, SectionName(..)
  , runWake, runWakeWithScan, readWake, readWakeWithScan, renderWake
  , prepareYuugao, noted, signal, signalWithScan
  ) where

import Control.Exception (Exception, SomeException, evaluate, throwIO)
import Data.List (isSuffixOf, sort)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Hikaru.RecordText as Record
import qualified Data.Text as T
import Hikaru.Choubo.Types (ScanEnv(..), ScanMode(..), scanState)
import qualified Hikaru.Choubo.Wake as Scan
import qualified Hikaru.Choubo.Reader as Scan
import qualified Hikaru.Choubo.Store as Scan
import Hikaru.Conversation.Hidden (phraseOpen)
import qualified Hikaru.OwnTime.Health as Health
import Hikaru.Hook.Log (checkedConsumer)
import qualified Hikaru.Memory.Correction.Annotate as Notes
import qualified Hikaru.Memory.Correction.Parse as Correction
import qualified Hikaru.Memory.Correction.Render as Correction
import qualified Hikaru.Memory.Correction.Types as Correction
import qualified Hikaru.Shiori as Shiori
import Hikaru.Mezame.Input (InputError(..))
import Hikaru.Mezame.Items (withoutForm, withoutMoved)
import Hikaru.Mezame.Progress (Progress, movedDays)
import Hikaru.Mezame.Signal (parseSignalLine)
import System.Directory (listDirectory)
import System.FilePath ((</>), normalise)
import System.IO.Error
import System.IO (stdout, hFlush)

-- Existing service APIs are adapted at the CLI boundary. No Python-shaped
-- values, arbitrary JSON or output callbacks enter the pure display functions.
data WakeEnv = WakeEnv
  { wakeRoot :: FilePath, wakeDiary :: FilePath, wakeShiori :: FilePath
  , wakeLogs :: FilePath, wakeIdentity :: Text, wakeSession :: IO Text
  , wakeFill :: Double -> IO (), wakeSazanami :: Maybe Integer -> Double -> IO Text
  , wakeOutput :: Text -> IO (), wakeProgress :: IO (Either Text Progress)
  , wakeReadRecord :: FilePath -> IO Text
  , wakeFailure :: SomeException -> IO Text
  , wakeScanContext :: IO (Maybe Text)
  , wakePathExists :: FilePath -> IO Bool
  }
newtype WakeExit = WakeExit Text deriving Show
instance Exception WakeExit

data SectionName = Hill | Evening | Strobe | Diary | Shiori | Sky | Recording | Tokimori
  deriving (Eq, Show)
data Section = Section SectionName [Text] deriving (Eq, Show)
data WakeDocument = FullWake Section Section Section Section Section Section Section (Maybe Health.Notice)
                  | SazanamiOnly Section
  deriving (Eq, Show)

title :: SectionName -> Text
title Hill = "丘の上"
title Evening = "夕顔"
title Strobe = "ストロボメモリー"
title Diary = "一週間フレンズ（最新）"
title Shiori = "しおり"
title Sky = "さざなみ"
title Recording = "記録の状態"
title Tokimori = "時守の状態"

renderWake :: WakeDocument -> Text
renderWake document = T.concat (map render ordered)
  where
    ordered = case document of
      FullWake a b c d e f g note -> [a,b,c,d,e,f,g] ++ maybe [] (\n -> [Section Tokimori (T.lines (Health.text n))]) note
      SazanamiOnly sky -> [sky]
    render (Section name body) =
      "\n======== " <> title name <> " ========\n\n" <> T.concat (map (<> "\n") body)

readSection :: SectionName -> [Text] -> IO a -> (a -> ([Text], b))
            -> IO (Section, b)
readSection name prefix readInput assemble = do
  input <- readInput
  let (body, extra) = assemble input
  _ <- evaluate (sum (map T.length body))
  pure (Section name (prefix ++ body), extra)

section :: SectionName -> [Text] -> IO a -> (a -> [Text]) -> IO Section
section name prefix action assemble = fst <$> readSection name prefix action (\a -> (assemble a, ()))

runWake :: WakeEnv -> [Text] -> IO ()
runWake env args = readWake env args >>= outputWake env
runWakeWithScan :: ScanEnv -> WakeEnv -> [Text] -> IO ()
runWakeWithScan scan env args = readWakeWithScan scan env args >>= outputWake env

outputWake :: WakeEnv -> WakeDocument -> IO ()
outputWake env document = do
  wakeOutput env (renderWake document)
  case document of
    FullWake _ _ _ _ _ _ _ note -> mapM_ (\n -> hFlush stdout >> Health.acknowledge n) note
    SazanamiOnly _ -> pure ()
readWake :: WakeEnv -> [Text] -> IO WakeDocument
readWake = readWakeUsing signal
readWakeWithScan :: ScanEnv -> WakeEnv -> [Text] -> IO WakeDocument
readWakeWithScan scan = readWakeUsing (signalWithScan scan)

readWakeUsing :: (WakeEnv -> IO Text) -> WakeEnv -> [Text] -> IO WakeDocument
readWakeUsing signalAction env args
  | "--kikkake" `elem` args = throwIO (WakeExit "--kikkake は、いまは --sazanami です。何も出していません。")
  | "--sazanami" `elem` args = SazanamiOnly <$> sky
  | otherwise = do
      hill <- section Hill [] (record ".丘の上.md.b64") (:[])
      (evening, index) <- readSection Evening [] (record ".夕顔.md.b64") prepareYuugao
      strobe <- section Strobe [] (record ".ストロボメモリー.md.b64")
        (\text -> [noted index "strobe" text Nothing])
      diary <- readDiary env index
      shiori <- section Shiori [] (do
        input <- optionalFile (wakeShiori env) (wakeReadRecord env (wakeShiori env))
        either (\(InputError reason) -> throwIO (InputError
          ("ValueError: invalid shiori file: " <> T.pack (wakeShiori env) <> ": " <> reason)))
          pure (renderShiori index input)) id
      -- Finish record reads before the weather service can update history.
      recording <- section Recording [] (signalAction env) (:[])
      note <- Health.wakeNotice
      skySection <- sky
      pure (FullWake hill evening strobe diary shiori skySection recording note)
  where
    record name = wakeReadRecord env (wakeRoot env </> name)
    sky = section Sky [] (wakeFill env 4 >> wakeSazanami env Nothing 4) (:[])

optionalFile :: FilePath -> IO a -> IO (Maybe a)
optionalFile path action = (Just <$> action) `catchIOError` \err ->
  if isDoesNotExistError err && ioeGetFileName err == Just path
    then pure Nothing else ioError err

prepareYuugao :: Text -> ([Text], Maybe Correction.Yuugao)
prepareYuugao text = case prepared of
  Left reason -> ([text, "\n（「みせけち」を畳めませんでした: " <> memoryError reason <> "。夕顔は元のまま出しています）"], Nothing)
  Right (index, (folded, tail_, kept)) ->
    let body = [withoutForm (T.pack folded)]
          ++ ["\n" <> T.pack tail_ | not (null tail_)]
          ++ ["（形が読めなかったので、畳まずにそのまま出した項目：" <> T.intercalate "、" (map T.pack kept)
              <> "。`.tools/bin/fuutou misekechi` で、どこが読めないかが出る）" | not (null kept)]
    in length (Correction.indexLines index) `seq` (body, Just index)
  where
    prepared = do
      index <- Correction.parse (T.unpack text)
      result <- Correction.fold index
      pure (index,result)

memoryError :: Correction.CorrectionError -> Text
memoryError Correction.SinceAfterCalendar = "ValueError: year 10000 is out of range"
memoryError Correction.BeforeCalendar = "OverflowError: date value out of range"
memoryError (Correction.YuugaoSyntax reason) = T.pack reason <> ": "
memoryError (Correction.RecordSyntax reason) = T.pack reason <> ": "

-- Memory's current pure interface remains the single implementation of ⤷.
-- Its explicit Left preserves the original annotation fallback, without an IO
-- exception handler or a mutation of the correction index.
noted :: Maybe Correction.Yuugao -> String -> Text -> Maybe (Text -> Bool -> Text) -> Text
noted index name text pick = finishNotes (maybe False (const True) pick) annotation selected
  where
    annotation@(Annotation annotated _ hasNotes _) = prepareNotes index name text
    selected = maybe annotated (\choose -> choose annotated hasNotes) pick

data Annotation = Annotation Text Int Bool Text

prepareNotes :: Maybe Correction.Yuugao -> String -> Text -> Annotation
prepareNotes index name text = case index of
      Nothing -> Annotation text 0 False ""
      Just ix -> case Notes.annotate ix name (T.unpack text) False of
        Right (out, count_) -> Annotation (T.pack out) count_ True ""
        Left reason -> Annotation text 0 False ("\n（注を添えられませんでした: " <> memoryError reason <> "）")

finishNotes :: Bool -> Annotation -> Text -> Text
finishNotes filtered (Annotation _ count_ _ tail_) selected = preface <> selected <> tail_
  where
    n | filtered && count_ /= 0 = length [line | line <- T.splitOn "\n" selected, "⤷" `T.isPrefixOf` Record.stripStart line]
      | otherwise = count_
    preface | n == 0 = ""
            | otherwise = "（⤷ で始まる行は、夕顔の「みせけち」から添えた注。記録そのものには書かれていない。"
                          <> T.pack (show n) <> " か所）\n\n"

readDiary :: WakeEnv -> Maybe Correction.Yuugao -> IO Section
readDiary env index = do
  (_, selected) <- readSection Diary [] names (\xs -> ([], reverse xs))
  case selected of
    [] -> pure (Section Diary ["（日記はまだありません）"])
    name:_ -> section Diary ["[" <> T.pack name <> "]\n"] (do
      text <- wakeReadRecord env (wakeDiary env </> name)
      progress <- wakeProgress env
      days <- either (throwIO . InputError) pure
        (progress >>= movedDays (T.pack (take 10 name)))
      pure (text, days)) (renderDiary index name)
  where
    names = sort . filter (isSuffixOf ".b64") <$> (listDirectory (wakeDiary env) `catchIOError` \err ->
      if isDoesNotExistError err
        then pure [] else ioError err)

renderDiary :: Maybe Correction.Yuugao -> FilePath -> (Text, S.Set (Int, Int)) -> [Text]
renderDiary index name (text, days) =
  let select value _ = fst (withoutMoved days value)
      count_ = snd (withoutMoved days text)
  in [noted index ("ichifure:" ++ take 10 name) text (Just select)]
     ++ ["\n（「問いたいこと」と「候補」は、しおりに移してある " <> T.pack (show count_)
         <> " 日分を、ここには出していない。すぐ下のしおりに出る。記録には残っている。全部は `.tools/bin/fuutou show ichifure`）" | count_ /= 0]

renderShiori :: Maybe Correction.Yuugao -> Maybe Text -> Either InputError [Text]
renderShiori _ Nothing = Right ["（しおりはまだありません）"]
renderShiori index (Just text) = do
  let annotation@(Annotation annotated _ hasNotes _) = prepareNotes index "shiori" text
      parse = either (Left . InputError . T.pack . show) Right . Shiori.parseShiori
  original <- parse text
  shown <- if annotated == text then pure original else parse annotated
  let (selected, _) = Shiori.openItems hasNotes shown
      (_, count_) = Shiori.openItems False original
  pure $ [finishNotes True annotation selected]
     ++ ["\n（済んだ " <> T.pack (show count_)
         <> " 項目は、ここには出していない。記録には残っている。全部は `.tools/bin/fuutou show shiori`）" | count_ /= 0]

signal :: WakeEnv -> IO Text
signal env = signalUsing (readSignal (ScanEnv "" ScanOff) "" env) env

signalWithScan :: ScanEnv -> WakeEnv -> IO Text
signalWithScan scan env
  | scanMode scan == ScanOff = signal env
  | otherwise = do
      context <- wakeScanContext env
      maybe (signal env) (\digest -> signalUsing (readSignal scan digest env) env) context

readSignal :: ScanEnv -> Text -> WakeEnv -> FilePath -> IO Bool
readSignal scan digest env path = Scan.withLogSource path $ \source ->
  phraseOpen . scanState <$> Scan.scanConsumer scan source
    (checkedConsumer path (Scan.signalConsumer digest (parseSignalLine (wakeIdentity env))))

signalUsing :: (FilePath -> IO Bool) -> WakeEnv -> IO Text
signalUsing reader env = do
  sid <- wakeSession env
  let path = normalise (wakeLogs env </> (T.unpack sid ++ ".jsonl"))
  hidden <- if T.null sid then pure Nothing else optionalFile path (reader path)
  pure $ case hidden of
    Nothing -> "この会話がどれか分からなかった。とばりが下りているかどうかは、この会話の中の合言葉で判断すること。"
    Just yes -> if yes
      then "この会話は、いま、とばりの中。この会話の内容は、どの記録にも書かないこと（仕組みを直す編集はしてよい）。合言葉は会話ごとに別々。"
      else "この会話は、いま通常の時間（記録してよい）。合言葉は会話ごとに別々。"

