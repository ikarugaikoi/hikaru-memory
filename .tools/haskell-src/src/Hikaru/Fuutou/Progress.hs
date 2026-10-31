{-# LANGUAGE OverloadedStrings #-}
-- | Session-scoped status snapshots and explicit completion updates. Sealed
-- commits and progress updates deliberately remain separate, ordered writes,
-- matching recovery behavior when an intermediate operation fails.
module Hikaru.Fuutou.Progress
  ( commandStatus, commandDiary, commandRead, commandReadCheck, commandDone, chunkHeads, commandMark ) where
import Control.Applicative ((<|>))
import Control.Monad (filterM,forM,forM_,unless,when,void,guard)
import Data.List (intercalate,stripPrefix)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import Hikaru.Compat.Unicode (pyIsDecimal)
import Hikaru.Shirushi.Analyze
import Hikaru.Shirushi.Workflow
import Hikaru.HouseDay (houseDayDate, houseDayNamed)
import qualified Hikaru.Shirushi.Json as Json
import qualified Hikaru.Shirushi.Syntax as Syntax
import qualified Hikaru.Shirushi.Parse as Parse
import qualified Hikaru.Shirushi.Render as Render
import Hikaru.Shirushi.Repeats
import Hikaru.Shirushi.Types hiding (Result)
import Hikaru.Fuutou.Core
import Hikaru.Store.State
import Hikaru.Fuutou.Record
import Hikaru.Fuutou.Text (usage,space)
import qualified Hikaru.Store.Coordinator as Store
import Hikaru.Store.Sealed (readSealed,sealText)
import Hikaru.Store.Actions (Action(..))
import qualified Hikaru.Store.Actions as Actions
import qualified Hikaru.Store.Execute as Execute
import Hikaru.Sealed (Plain(..))
import qualified Hikaru.Store.Work as Work
import System.Directory (removeFile)
import System.FilePath ((</>),takeFileName,takeExtension,normalise,dropTrailingPathSeparator)

loadAsked :: SealEnv -> IO Asked
loadAsked env = do
  directory <- Work.workDir (sealWork env)
  readState askedError (directory </> "asked.json") emptyAsked decodeAsked
saveAsked :: SealEnv -> Asked -> IO ()
saveAsked env = Work.saveAsked (sealWork env) . encodeAsked

plainSource :: String -> IO (FilePath,T.Text)
plainSource value=do
  let path=dropTrailingPathSeparator(normalise(value))
  when(takeExtension path==".b64")$stopEdit(concat["封をしたファイルは足せません: ",takeFileName path,"（平文を渡してください）"])
  text<-readUtf8 path
  when(T.null(T.dropWhile space text))(stopEdit "足す中身が空です。")
  pure(path,text)

joinedBody :: T.Text -> T.Text -> T.Text
joinedBody old addition = T.dropWhileEnd (=='\n') old <> "\n\n"
  <> T.dropAround (=='\n') addition <> "\n"

appendBody :: SealEnv -> FilePath -> IO T.Text -> T.Text -> IO Execute.Committed
appendBody env path initial addition = Store.withTargetLock (sealStore env) path $ \_ -> do
  existing <- readOptional "cannot read record" path (readSealed path)
  old <- case existing of
    Just body -> Store.backup (sealStore env) path >> pure body
    Nothing -> initial
  plan <- either (ioError . userError . T.unpack) pure (Actions.prepareWrite path (Plain (joinedBody old addition)))
  Execute.commit (sealStore env) plan

commandDiary :: SealEnv -> String -> String -> IO ()
commandDiary env date source=do
  day<-parseDay date
  (src,addition)<-plainSource source
  (target,mon,sun)<-weekFile(sealRecords env)day
  receipt <- withProgress env $ \progress -> do
    asked<-loadAsked env
    let key=show day
    state<-maybe (dayState env day) (pure . Just) (lookup key (askedDays asked))
    next<-require $ case state of
      Nothing->Right progress
      Just cp->fst <$> advanceDiary key cp progress
    let (y,m,d)=toGregorian mon; (_,sm,sd)=toGregorian sun
        initial=T.pack(concat["# 一週間フレンズ　",show y,"年",show m,"月",show d,"日〜",show sm,"月",show sd,"日\n"])
    committed <- appendBody env target (pure initial) addition
    forM_ state $ \cp -> do
      perform env [SaveProgress next]
      saveAsked env (rememberDiary key cp asked)
    pure committed
  removeFile src
  say env(concat["日記に書き足しました: ",takeFileName target,"（",takeFileName src," は消しました）"])
  notifyCommitted env "ichifure" Actions.Diary receipt

-- CHUNK_HEAD findall's codepoint grammar; timestamps are stored literally,
-- without an extra calendar validity check. Whitespace in the short id is
-- forbidden, and the separating space is exactly ASCII U+0020.
chunkHeads :: String -> [(String,String)]
chunkHeads=scan
  where
    scan []=[]
    scan text@(_:rest)=case one text of Just(a,b,tail_)->(a,b):scan tail_;Nothing->scan rest
    one ('〔':rest)=do
      let (name,tail_)=span(not.space)rest
      guard (not (null name))
      after<-stripPrefix " " tail_
      let (time,end)=splitAt 19 after
      guard $ case time of
        [a,b,c,d,'-',e,f,'-',g,h,'T',i,j,':',k,l,':',m,n]->all pyIsDecimal[a,b,c,d,e,f,g,h,i,j,k,l,m,n]
        _->False
      more<-stripPrefix "〕" end <|> stripPrefix "+09:00〕" end
      pure(name,time,more)
    one _=Nothing

commandRead :: SealEnv -> String -> String -> IO ()
commandRead env date source=do
  day<-parseDay date
  (src,addition)<-plainSource source
  let found=chunkHeads (T.unpack addition)
  when(null found)$stopEdit "ひとときの見出し（## ひととき 10-03 10:39〜12:11 〔a3f 2026-10-03T10:39:55〕）が一つもありません。\nどのひとときを読んだか記せないので止めました（ファイルは残してあります）。"
  sessions<-loadSessions(sealStore env)
  let (ids,unknown)=sessionsForRead sessions found
  unless(S.null unknown)$stopEdit(concat["会話の短い名が .sessions.json にありません: ",intercalate "、"(S.toAscList unknown),"。止めました（ファイルは残してあります）。"])
  let target=recordAnalysis(sealRecords env) </> show day++".read.md.b64"
  withProgress env $ \progress -> do
    let initial=do
          makeDirectory(recordAnalysis(sealRecords env))
          let(y,m,d)=toGregorian day
          pure(T.pack(concat["# ",show y,"年",show m,"月",show d,"日（5時〜翌4時59分）\n\n",
            "ここに書くのは、わたしが原文を読んで判断したこと（第二段階）。しるしと数字（第一段階）は同じ日付の .mark.json にある。\n",
            "ひとときの見出しは「## ひととき 10-03 10:39〜12:11 〔a3f 2026-10-03T10:39:55〕」（会話の短い名と開始時刻。完全な ID は .sessions.json で引ける）。\n",
            "一度書いた判断は消さない。あとで変わったら、その下に日付をつけて続ける。ユーザーが会話の中で言ってくれたことも、その箇所の下に「ふ（日付）」として続ける。\n"]))
    void (appendBody env target initial addition)
    perform env [SaveProgress (appendRead (S.toAscList ids) progress)]
  removeFile src
  say env(concat["書き足しました: ",takeFileName target,"（",takeFileName src," は消しました）。読んだひととき ",show(S.size ids)," を進捗に書き留めました。"])
  -- Reread notice (not in the Python baseline): after everything is committed.
  case rereadReport (T.unpack addition) of
    []->pure()
    notes->say env(intercalate "\n" notes)

-- seal.py read DATE FILE --check: the same notice, without committing anything.
commandReadCheck :: SealEnv -> String -> String -> IO ()
commandReadCheck env date source=do
  _<-parseDay date
  (_,addition)<-plainSource source
  case rereadReport (T.unpack addition) of
    []->say env "読み直し：判が一つもありません。（--check なので、足していません）"
    notes->say env(concat[intercalate "\n" notes,"\n（--check なので、足していません。ファイルもそのまま）"])

commandDone :: SealEnv -> String -> [String] -> IO ()
commandDone env what days=do
  asked<-loadAsked env
  case what of
    "yuugao"->case askedLast asked of
      Nothing->stopEdit "先に `.tools/bin/fuutou status` を。status が見せた範囲を見終わった、と記すためのものです。"
      Just Nothing->say env "まだ何も保存されていないので、記すことがありません。"
      Just (Just last_)->do
        time<-humanTime last_
        went<-updateProgress env (advanceIndex last_)
        if went then say env(concat["夕顔を見たところを進捗に書き留めました（",time," まで）。"])
        else say env "夕顔は、もうそこまで（か、その先まで）見たことになっています。"
    "shiori"->do
      progress<-loadProgress env
      lag<-require (diaryBehind progress)
      if null days then
        if null lag then say env "しおりは、もう日記に追いついています。"
        else stopEdit(concat["どの日の分をしおりに移したかを、日付で：`.tools/bin/fuutou done shiori ",intercalate " " lag,"`\n（移した日だけを書く。その日の日記の節は `.tools/bin/fuutou show ichifure:YYYY-MM-DD` で読める）"])
      else do
        forM_ days parseDay
        let seen=askedDiary asked
            missing=filter (`notElem` map fst seen) days
        unless(null missing)$stopEdit(concat["この会話は、その日の日記をまだ見ていません（か、日記がまだです）: ",intercalate "、" missing,"。\n先に `.tools/bin/fuutou status`（しおりがまだの日が出る）か、その日の `.tools/bin/fuutou diary` を。"])
        went<-updateProgress env (advanceShiori [(key,cp) | key<-days, Just cp<-[lookup key seen]])
        let left=filter(`notElem`days)lag
        say env(concat[if null went then "その日の分は、もう記してあります。" else concat["しおりに移したところを進捗に書き留めました：",intercalate "、" went],
           if null left then "" else concat["（しおりがまだの日が残っています：",intercalate "、" left,"）"]])
    "index"->stopEdit "index は、いまは yuugao です。何もしていません。"
    _->stopEdit(concat["done のあとは yuugao か shiori を。\n",usage])

commandStatus :: SealEnv -> IO ()
commandStatus env=do
  days<-timeMachineDays(sealRecords env)
  entries<-forM days $ \day->do state<-dayState env day; pure (show day,state)
  let states=[(key,cp) | (key,Just cp)<-entries]
      lastPath=recordTimeMachine(sealRecords env) </> ".last"
  last_<-maybe "" (T.dropAround space) <$> readOptional "cannot read last timestamp" lastPath (readUtf8 lastPath)
  progress<-loadProgress env
  let diary=progressDiary progress
  let fresh=[key | (key,_)<-states, key `notElem` map fst diary]
  more<-require $ filterM (\(_,previous,current)->behind (Just previous) current)
    [(key,previous,current) | (key,current)<-states, Just previous<-[lookup key diary]]
  moreLabels<-forM more $ \(key,previous,_)->do
    time<-humanTime (checkpointTime previous)
    pure (concat [key,"（",time," より後の発話）"])
  lag<-require (diaryBehind progress)
  let seen=progressSeen progress
  end<-require (if T.null last_ then Right Nothing else Just <$> stamp last_)
  rangeText<-case end of
    Nothing->pure []
    Just latest->do
      newRange<-require (maybe (Right True) (`rawBefore` latest) seen)
      if not newRange then pure [] else do
        first<-maybe (pure "はじめ") humanTime seen
        lastTime<-humanTime latest
        pure[concat["夕顔がまだの範囲：",first," 〜 ",lastTime,"（タイムマシンに保存した発話のうち、夕顔で見ていないところ。見終わったら .tools/bin/fuutou done yuugao）"]]
  (results,_)<-allChunks env
  now<-markNow(sealMark env)
  readIds<-readMarkIds progress
  let unread=filter (\r->isClosed now r && analysisId r `S.notMember` readIds) results
      unreadLines=[concat["  ・",chunkTitle r,chunkTag r] | r<-unread]
      still=filter (not.isClosed now) results
      stillTitles=map chunkTitle still
  let out=[concat["日記がまだの日：",intercalate "、" fresh]|not(null fresh)]++
        (if null more then [] else[concat["日記に足す分がある日：",intercalate "、" moreLabels],"  （日記を書いたあとで、その日のファイルに会話が足された。足された分を読んで、同じ日の「（つづき）」の節を書く）"])++
        (if null lag then [] else[concat["しおりがまだの日：",intercalate "、" lag],"  （日記は書いた。その日の節の「問いたいこと」「候補」をしおりへ。節は .tools/bin/fuutou show ichifure:その日 で読める。移したら .tools/bin/fuutou done shiori 移した日…）"])++rangeText++
        (if null unread then [] else concat["まだ分析していないひととき：",show(length unread),"（.tools/bin/fuutou mark でしるしをつけ、読んだら .tools/bin/fuutou read で記す）"]:unreadLines)
      finished=(if null out then ["全部済んでいます。"]else out)++[concat["まだ開いているひととき（次回に）：",intercalate "、" stillTitles]|not(null still)]
  saveAsked env (statusAsked end states diary)
  say env(intercalate "\n" finished)

allChunks :: SealEnv -> IO ([LiveAnalysis],[Repetition])
allChunks env=do
  days<-timeMachineDays(sealRecords env)
  case days of
    []->pure([],[])
    first:_->do
      let mark=sealMark env
      msgs<-loadMessages mark first(last days)
      let fumi=filter ((==HumanSpeaker).messageSpeaker) msgs
          claude=filter ((==Claude).messageSpeaker) msgs
      now<-markNow mark
      rep<-requireMark(repeats now defaultRepeatOptions claude)
      table<-markLoadSessions mark
      blocks<-requireMark(Parse.chunks (Right (4*3600)) msgs)
      result<-traverse (analyzeWithSessions mark 0 fumi rep table) blocks
      pure(result,rep)

dayState :: SealEnv -> Day -> IO (Maybe Checkpoint)
dayState env day=do
  let path=recordTimeMachine(sealRecords env) </> show day++".md.b64"
  body<-readOptional "cannot read time machine record" path (readSealed path)
  case body of
    Nothing -> pure Nothing
    Just text -> do
      msgs<-requireMark(Parse.parse text(Just day))
      case msgs of
        []->pure Nothing
        first:rest->do
          let latest=foldl' (\previous msg->max previous (messageTime msg)) (messageTime first) rest
          Just <$> require (checkpoint (Syntax.stamp latest) (length msgs))

isClosed :: LocalTime -> Analysis m q s -> Bool
isClosed now result=diffLocalTime now (analysisEnd result)>=4*3600

chunkTitle :: Analysis m q s -> String
chunkTitle result=
  let start=analysisStart result;end=analysisEnd result
      format fmt=formatTime defaultTimeLocale fmt
  in concat["ひととき ",format "%m-%d %H:%M" start,"〜",format(if localDay start==localDay end then "%H:%M" else "%m-%d %H:%M")end]

chunkTag :: Analysis m q s -> String
chunkTag result=T.unpack (" " <> Render.tag result)

commandMark :: SealEnv -> [String] -> IO ()
commandMark env args=do
  let days=filter dayLike(drop 1 args)
  page<-pageArg args Nothing
  (res,rep)<-allChunks env
  if null res then say env "タイムマシンの記録がありません。" else do
    now<-markNow(sealMark env)
    let done=filter(isClosed now)res
    (pick,still,header)<-case days of
      dayText:_->do
        day<-houseDayNamed <$> parseDay dayText
        let open=filter(not.isClosed now)(filter((==day).analysisDay)res)
        let chosen=filter((==day).analysisDay)done;(y,m,d)=toGregorian(houseDayDate day)
        pure(chosen,open,concat[show y,"年",show m,"月",show d,"日（5時〜翌4時59分）に始まったひととき：",show(length chosen)])
      []->do
        progress<-loadProgress env
        ids<-readMarkIds progress
        let chosen=filter ((`S.notMember` ids).analysisId) done
            open=filter (not.isClosed now) res
        pure(chosen,open,concat["まだ読んでいないひととき：",show(length chosen)])
    note<-if null still then pure "" else do
      let titles=map chunkTitle still
      pure(concat["\nまだ開いているひととき（最後の発話から 4 時間たっていない。次回に）：",intercalate "、" titles])
    if null pick then say env(concat[if null days then "まだ読んでいないひとときはありません。" else "その日に始まった、閉じたひとときがありません。",note]) else do
      let byDay=S.toAscList(S.fromList(map analysisDay pick))
      savedHeader<-case page of
        Nothing->do
          saved<-forM byDay$ \day->do
            let directory=recordAnalysis(sealRecords env);target=directory </> show day++".mark.json.b64"
            Store.withTargetLock(sealStore env)target$ \_->do
              -- pathlib.mkdir(exist_ok=True), without creating parents.
              makeDirectory directory
              Store.backup(sealStore env)target
              let text=Json.toJson(filter((==day).analysisDay)done)
              sealed <- sealText target text
              perform env [WriteRecord target sealed]
            pure(takeFileName target)
          pure(concat[header,"（数字としるしは ",intercalate "、" saved," に保存した）"])
        Just _->pure(concat[header,"（--page なので保存はしていない）"])
      let titles=[concat["  ・",chunkTitle r,chunkTag r] | r<-pick]
      let heading=concat[savedHeader,note,"\n",intercalate "\n" titles,
            "\n読んで書いたものを fuutou read で足す日（ひとときの属する日＝始まった時刻の日。朝 5 時切りなので、未明に始まったひとときは前の日）：",
            intercalate "、"[concat[show day,"（",show(length(filter((==day).analysisDay)pick))," ひととき）"]|day<-byDay],"\n\n"]
      renderNow<-if null rep then pure now else markNow(sealMark env)
      body<-T.unpack <$> requireMark(Render.render renderNow pick rep defaultRepeatOptions)
      let chosenPage=case page of Just n | n/=0->n;_->1
      showPageText env (T.pack (concat[heading,body])) chosenPage
