{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-- | IO boundary: observe under the existing locks, call pure plans, execute.
-- Legacy Store values are converted only here; no JSON reaches the decisions.
module Hikaru.Ochibo.CLI
  ( CollectEnv(..), OldFormat(..), runCollect, runCollectUnlocked, appendDay ) where

import Control.Exception (Exception,catch,throwIO)
import Control.Monad (forM,forM_,when,unless)
import Data.IORef
import Data.List (elemIndex)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Typeable
import System.Directory (removeFile)
import System.FilePath ((</>),takeFileName)
import System.IO.Error (catchIOError, isDoesNotExistError)
import Hikaru.Compat.Exception (requireEither)
import Hikaru.Compat.Diagnostic (readSealedDiagnostic)
import qualified Hikaru.Compat.Json as Legacy
import Hikaru.Ochibo.Archivable
import Hikaru.Ochibo.Files
import Hikaru.Ochibo.Format
import Hikaru.Ochibo.Read
import Hikaru.Ochibo.Plan hiding (OldFormat)
import qualified Hikaru.Ochibo.Plan as Plan
import Hikaru.Conversation.Input (stripArchive, archiveSpace)
import Hikaru.Conversation.Line
import Hikaru.HouseDay (HouseDay, houseDayDate, houseDayNamed)
import qualified Hikaru.Store.Coordinator as Store
import qualified Hikaru.Store.Paging as Paging
import Hikaru.Store.Sealed

data CollectEnv = CollectEnv
  { collectStore :: !Store.StoreEnv
  , collectReader :: !CollectReadEnv
  , collectPictures :: !FilePath
  , collectNow :: IO UTCTime
  , collectFillSky :: IO ()
  , collectOutput :: Text -> IO ()
  }
newtype OldFormat = OldFormat Text deriving (Show,Typeable)
instance Exception OldFormat
num :: Show a => a -> Text
num = T.pack . show
join :: [Text] -> Text
join = T.concat
comma :: [Text] -> Text
comma = T.intercalate "、"
say :: CollectEnv -> Text -> IO ()
say env text = collectOutput env (text <> "\n")
tmRoot :: CollectEnv -> FilePath
tmRoot = Store.storeTimeMachine . collectStore
dayPath :: CollectEnv -> HouseDay -> FilePath
dayPath env day = tmRoot env </> T.unpack (tmFileName day)
readOld :: FilePath -> IO Text
readOld path = readSealed path `catchIOError` \err ->
  if isDoesNotExistError err then pure "" else ioError err

-- The write plan's constructor is hidden, and every planning input has archive
-- rights. Keep backup/verification/atomic write delegated to the central store.
executeDay :: CollectEnv -> FilePath -> DayWrite -> IO ()
executeDay env path plan = do
  Store.backup (collectStore env) path
  sealText path (writeText plan) >>= writeSealed path

appendDay :: CollectEnv -> IORef Sessions -> HouseDay -> [Archivable] -> IO Int
appendDay env table day rows = Store.withTargetLock (collectStore env) path $ \_ -> do
  old <- readOld path
  current <- readIORef table
  let (decision,next) = planAppend day old current rows
  writeIORef table next
  case decision of
    Plan.OldFormat -> throwIO (OldFormat (T.pack (takeFileName path)))
    Unchanged -> pure 0
    Append plan -> executeDay env path plan >> pure (writeCount plan)
  where path = dayPath env day

loadTable :: CollectEnv -> IO Sessions
loadTable env = do
  value <- Store.loadSessions (collectStore env)
  case value of
    Legacy.Object object -> sessions <$> traverse pair (Legacy.objectPairs object)
    _ -> ioError (userError "sessions must be an object")
  where
    pair (name,Legacy.String full) = (,) <$> requireEither (Legacy.pyStringToText name) <*> requireEither (Legacy.pyStringToText full)
    pair _ = ioError (userError "session ids must be strings")
saveTableIfChanged :: CollectEnv -> Sessions -> IORef Sessions -> IO ()
saveTableIfChanged env original table = do
  current <- readIORef table
  when (current /= original) $ do
    Store.backup (collectStore env) (Store.storeSessions (collectStore env))
    Store.saveSessions (collectStore env) (Legacy.objectFromPairs
      [(Legacy.pyStringFromText name,Legacy.String (Legacy.pyStringFromText full)) | (name,full) <- sessionPairs current])
writeProgress :: FilePath -> Text -> IO ()
writeProgress path = Store.writeUtf8 path . Legacy.pyStringFromText
showPage :: CollectEnv -> Text -> Integer -> IO ()
showPage env body page = requireEither (Paging.showPage (Legacy.pyStringFromText body) page >>= Legacy.pyStringToText) >>= collectOutput env
renderRows :: IORef Sessions -> [Archivable] -> IO Text
renderRows table rows = do
  current <- readIORef table
  (rendered,next) <- requireEither (renderMessages current rows)
  writeIORef table next
  pure rendered

pageNumber :: Text -> IO Integer
pageNumber value = requireEither $ do
  let stripped=stripArchive value
      (sign,body)=case T.uncons stripped of Just ('-',rest)->(-1,rest); Just ('+',rest)->(1,rest); _->(1,stripped)
      digits=T.filter (/='_') body
  if T.null body || T.isPrefixOf "_" body || T.isSuffixOf "_" body || "__" `T.isInfixOf` body
    then Left "invalid page number" else do
      ns <- traverse (maybe (Left "invalid page number") Right . decimalDigit) (T.unpack digits)
      pure (sign * foldl (\n d -> n*10+toInteger d) 0 ns)

runCollect :: CollectEnv -> [Text] -> IO ()
runCollect env args = do
  when (null args) (collectFillSky env)
  Store.withTargetLock (collectStore env) (tmRoot env </> ".collect") $ \_ -> runCollectUnlocked env args

runCollectUnlocked :: CollectEnv -> [Text] -> IO ()
runCollectUnlocked env args = do
  now <- collectNow env
  original <- loadTable env
  table <- newIORef original
  (loaded,passwordOpen) <- loadAll (collectReader env)
  let closure = markClosed loaded now
      rows = closureMessages closure
      opened = closureOpen closure
      pendingText word tailText = pending (toInteger opened) (toInteger (closureWaiting closure)) word tailText
      pendingNotice = if opened/=0 then pendingText "が" "次回に回す" <> "。" else ""
      lastRaw_ = lastRaw (collectReader env)
      lastTime = lastRaw_ >>= traverse parseCollectTimestamp
      fill = do (n,m,f) <- fillPictures (tmRoot env) (collectPictures env) (archivable rows)
                reportPictures (collectOutput env) n m f "補いました"
      reportOpen = when (passwordOpen/=0) (say env ("合言葉が開いたままの会話: " <> num passwordOpen))
      has option = option `elem` args
  if has "--page" then do
    n <- case elemIndex "--page" args of
      Just i | i+1<length args -> pageNumber (args!!(i+1))
      _ -> ioError (userError "missing page number")
    let batchPath=tmRoot env </> ".batch"
    batchText <- (Just <$> Store.readUtf8 batchPath) `catchIOError` \err ->
      if isDoesNotExistError err then pure Nothing else ioError err
    case batchText of
      Nothing -> say env "まだ保存した分がありません。先に .tools/bin/ochibo を実行してください。"
      Just raw -> do
        text <- requireEither (Legacy.pyStringToText raw)
        (after,before) <- case filter (not . T.null) (T.split archiveSpace text) of
          [a,b] -> (,) <$> (if a=="-" then pure Nothing else Just <$> parseCollectTimestamp a) <*> parseCollectTimestamp b
          _ -> ioError (userError "batch must have two timestamps")
        let batch = [row | row <- archivable rows, let t=utteranceTime (archivedMessage row), maybe True (t>) after, t<=before]
        renderRows table batch >>= \rendered -> showPage env rendered n
  else if has "--pictures" then fill
  else if has "--audit" then do
    done <- lastTime
    groups <- requireEither (dayGroups (archivedPool closure done now))
    have <- timeMachineDays (tmRoot env)
    say env "日付（5時〜翌4時59分）  いまのファイル  作り直した場合"
    forM_ (S.toAscList (S.fromList (map fst groups ++ map fst have))) $ \day -> do
      count <- case lookup day have of Nothing -> pure 0; Just path -> countEntries <$> readSealedDiagnostic path
      let padded n = T.justifyRight 8 ' ' (num n)
      say env (join [num day,"              ",padded count," 件   ",padded (length (maybe [] id (lookup day groups)))," 件"])
    when (opened/=0) (say env (pendingText ":" "数えていません"))
  else if has "--rebuild" then do
    let targets=case elemIndex "--rebuild" args of Nothing -> []; Just i -> filter (not . T.isPrefixOf "--") (drop (i+1) args)
    if null targets then say env "作り直す日付を YYYY-MM-DD で（全部なら all を）指定してください。" else do
      rebuild env original table targets closure now (has "--dry-run") (has "--force")
  else do
    done <- lastTime
    let batch=planBatch closure done now
        keep=batchVisible batch
    groups <- requireEither (dayGroups keep)
    if has "--dry-run" then do
      current <- lastRaw_
      say env ("前回の位置: " <> maybe "（なし）" id current)
      say env (join ["保存される発話: ",num (length keep)," 件（画像 ",num (sum (map (length . utterancePictures . archivedMessage) keep))," 枚）"])
      forM_ groups $ \(day,selected) -> say env (join ["  ",num day,": ",num (length selected)," 件"])
      when (opened/=0) (say env (pendingText ":" "次回に回す"))
      reportOpen
    else do
      let signal=tmRoot env </> ".signal"
      removeFile signal `catchIOError` \err ->
        if isDoesNotExistError err then pure () else ioError err
      case batchNewest batch of
        Nothing -> do say env ("新しい会話はありません。" <> pendingNotice); reportOpen; fill
        Just (_,raw) -> do
          result <- (Right . sum <$> mapM (\(day,selected) -> appendDay env table day selected) groups)
            `catch` (\(OldFormat name) -> pure (Left name))
          case result of
            Left name -> do
              saveTableIfChanged env original table
              say env (join [name," は旧形式（1 行目に「（5時〜翌4時59分）」の見出しが無い）なので追記せず、ここで止めました。",
                "その日を先に --rebuild してください：.tools/bin/ochibo --rebuild ",T.take 10 name])
            Right added -> do
              (pictures,failed) <- savePictures (collectPictures env) keep
              saveTableIfChanged env original table
              previous <- lastRaw_
              writeProgress (tmRoot env </> ".batch") (join [maybe "-" id previous," ",raw,"\n"])
              writeProgress (collectLastPath (collectReader env)) raw
              if null keep then say env ("新しい会話はありません。" <> pendingNotice) else do
                let labels=comma [let (_,m,d)=toGregorian (houseDayDate day) in join [num m,"月",num d,"日"] | (day,_) <- groups]
                    headline=if added/=0 then join ["タイムマシンに保存しました：",num added," 件（",labels,"）"] else join ["もう全部入っていました（",labels,"）"]
                say env (join [headline,if added>0 && added<length keep then join ["。",num (length keep-added)," 件はもう入っていたので飛ばした"] else "",
                  if pictures/=0 then join ["。画像 ",num pictures," 枚を .おしばな/ に置きました"] else "",
                  if opened/=0 then "。" <> pendingText "が" "次回に回す" else "","。"])
              reportOpen
              reportPictures (collectOutput env) 0 [] failed "補いました"
              fill
              unless (null keep) $ do
                say env "以下がその会話です。\n"
                renderRows table keep >>= \rendered -> showPage env rendered 1

rebuild :: CollectEnv -> Sessions -> IORef Sessions -> [Text] -> Closure -> UTCTime -> Bool -> Bool -> IO ()
rebuild env original table targets closure now dry force
  | "all" `elem` targets && length targets>1 = say env "all と日付は一緒に指定できません。全部なら all だけ、いくつかの日ならその日付だけを。"
  | otherwise = do
      origin <- rebuildOrigin <$> readIORef table
      done <- lastRaw (collectReader env) >>= traverse parseCollectTimestamp
      let pool=archivedPool closure done now
      groups <- requireEither (dayGroups pool)
      allGroups <- requireEither (dayGroups (archivable (closureMessages closure)))
      let byDay=map fst groups
          unsaved=S.fromList (map fst allGroups) S.\\ S.fromList byDay
      (days,stale) <- if targets==["all"] then do
        have <- timeMachineDays (tmRoot env)
        pure (byDay,S.toAscList (S.fromList (map fst have) S.\\ S.fromList byDay))
        else do
          selected <- forM targets $ \text -> case collectDay text of
            Just day -> pure [houseDayNamed day]
            Nothing -> say env (text <> ": 日付は YYYY-MM-DD で。飛ばします。") >> pure []
          pure (concat selected,[])
      outcomes <- forM days $ \day -> do
        let selected=maybe [] id (lookup day groups)
        if null selected then do
          say env (num day <> if S.member day unsaved then ": まだ保存していない範囲なので作り直しません。先に引数なしで実行してください。"
                    else ": 残っている会話記録に、この日（5時〜翌4時59分）の発話がありません。何もしません。")
          pure (0,[])
        else do
          let path=dayPath env day
          result <- Store.withTargetLock (collectStore env) path $ \_ -> do
            old <- readOld path
            current <- readIORef table
            let (decision,next)=planRebuild force day old origin current selected
                had=countEntries old
            case decision of
              WouldLose lost -> do
                let example=comma [t <> " " <> who | (t,who) <- take 3 lost] <> if length lost>3 then " など" else ""
                say env (join [num day,": いまのファイルには、作り直すと消える発話が ",num (length lost)," 件あります（",example,"）。",
                  "会話記録がもう残っていないか、あとから記録しない範囲に入ったものです。この日は作り直しません（--force は、ユーザーがこの発話の削除をはっきり認めたときだけ）。"])
                pure Nothing
              Replace plan lost -> do
                let note=if null lost then "" else join ["（うち、いまのファイルから消える発話 ",num (length lost)," 件）"]
                if dry then do
                  say env (join [num day,": ",num had," 件 → ",num (length selected)," 件",note,"（--dry-run なので書きません）"])
                  pure Nothing
                else do
                  executeDay env path plan
                  writeIORef table next
                  pure (Just (had,note))
          case result of
            Nothing -> pure (0,[])
            Just (had,note) -> do
              (n,f) <- savePictures (collectPictures env) selected
              say env (join [num day,": ",num had," 件 → ",num (length selected)," 件で作り直しました",note,
                if n/=0 then join ["（画像 ",num n," 枚を新しく置いた）"] else "","。"])
              pure (n,f)
      unless (null stale) (say env ("残っている会話記録に発話が無い日のファイル（そのまま残しています）: " <> comma (map num stale)))
      unless dry $ do
        -- Every successful day commit must retain its session aliases even
        -- when the later picture report stops this command.
        saveTableIfChanged env original table
        reportPictures (collectOutput env) 0 [] (concatMap snd outcomes) "補いました"
        when (targets==["all"] || sum (map fst outcomes)/=0) $ do
          old <- oldPictures (tmRoot env) (collectPictures env)
          when (old/=0) (say env (join [".おしばな/ の前の名のファイル ",num old," 枚は消していません。"]))
