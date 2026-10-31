{-# LANGUAGE OverloadedStrings #-}
-- | Native record commands; process routing lives in Hikaru.Fuutou.Entry.
module Hikaru.Fuutou.CLI (runArguments,commandMisekechi) where
import Data.List (intercalate)
import Control.Monad (forM,when)
import qualified Data.Text as T
import Hikaru.Compat.FileSystem (pathIsFile)
import Hikaru.Compat.Json (PyString, pyStringFromChars, pyStringToChars)
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import System.IO (stderr)
import qualified Hikaru.Memory.Correction.Render as Correction
import qualified Hikaru.Memory.Fade as Fade
import qualified Hikaru.Memory.Validation as Validation
import Hikaru.Memory.Validation (yuugaoGrown)
import Hikaru.Fuutou.Core
import Hikaru.Fuutou.Progress
import Hikaru.Fuutou.Record
import Hikaru.Fuutou.Text (usage)
import Hikaru.Store.Coordinator (storeDay)
import Hikaru.Store.Edit (openRecord,reopenRecord,closeRecordWithNotice,appendRecord)
import Hikaru.Store.Sealed (readSealed)
import Hikaru.Store.Work (workDir)
import System.Directory (removeFile)
import System.FilePath ((</>))

commandMisekechi :: SealEnv -> [String] -> IO ()
commandMisekechi env args=do
  target<-recordPath(sealRecords env)Yuugao
  text<-readOrNew(sealRecords env)target
  doc<-Validation.readYuugao (T.unpack text)
  let rest=[value|(previous,value)<-zip args(drop 1 args),value/="--page",previous/="--page"]
  body<-case rest of
    rid:_->case Correction.expand doc(rid)of
      Nothing->stopEdit(concat["その番号の直しはありません: ",rid,"（一覧は `.tools/bin/fuutou misekechi`）"])
      Just rendered->pure(rendered)
    []->do
      rendered<-Validation.requireCorrection(Correction.listing doc)
      (stops,warnings)<-Validation.correctionCheck(validationEnvironment env)doc
      let bullets=intercalate "\n".map(\line->concat["  ・",line])
      pure(concat[rendered,if null stops then "" else concat["\n\n形が読めないところ（このままでは close yuugao が止まる）：\n",bullets stops],
               if null warnings then "" else concat["\n\n気になるところ：\n",bullets warnings]])
  page<-pageArg args(Just 1)
  showPageText env (T.pack body) (maybe 1 id page)

runArguments :: SealEnv -> [PyString] -> IO ()
runArguments env = runTextArguments env . map pyStringToChars

runTextArguments :: SealEnv -> [String] -> IO ()
runTextArguments env original=do
  let force="--force" `elem` original
      args=filter(/="--force")original
      edit=editEnvironment env
      withName action=need args 2 >> resolve(sealRecords env)(args!!1) >>= action(args!!1)
  case args of
    []->say env usage
    cmd:_->case cmd of
      "show"->withName(\_ target->do
        text<-readOrNew(sealRecords env)target
        page<-pageArg args(Just 1)
        showPageText env text (maybe 1 id page))
      "where"->workDir(sealWork env) >>= say env
      "open"->withName(\name target->do
        openRecord edit name target force
        when(name=="yuugao") $ B.hPut stderr $ TE.encodeUtf8
          "（書く前に：項目の書き方は、このファイルの冒頭にある。この会話でまだ読んでいなければ、圧縮のあとも、先に読む）\n")
      "reopen"->withName(reopenRecord edit)
      "close"->withName(\name target->closeRecordWithNotice (growthNotice name) edit name target force)
      "append"->do
        need args 3
        target<-resolve(sealRecords env)(args!!1)
        appendRecord edit(args!!1)target(args!!2)
      "diary"->need args 3 >> commandDiary env(args!!1)(args!!2)
      "read"->let plain=filter(/="--check")args in need plain 3 >>
        (if "--check" `elem` args then commandReadCheck env(plain!!1)(plain!!2) else commandRead env(plain!!1)(plain!!2))
      "mark"->commandMark env args
      "status"->commandStatus env
      "done"->need args 2 >> commandDone env(args!!1)(drop 2 args)
      "misekechi"->commandMisekechi env args
      "judgments"->do   -- 判を JSON で（こより＝記憶の流れの目録が読む）。読むだけ
        withName(\_ target->readOrNew(sealRecords env)target >>= sayText env . judgmentsJson)
      "clean"->do
        directory<-workDir(sealWork env)
        names<-globNames directory
        counts<-forM names$ \name->do
          let path=directory </> name
          file<-pathIsFile path
          if file && name/="asked.json" then removeFile path >> pure(1::Int) else pure 0
        say env(concat["作業フォルダの平文を ",show(sum counts)," 個消しました。"])
      "fade"->do
        today<-storeDay(sealStore env)
        target<-recordPath(sealRecords env)Strobe
        text<-readSealed target
        doc<-Validation.readStrobe (T.unpack text)
        plan<-Validation.requireFade (Fade.planFade today doc)
        say env (Fade.report plan)
      "naoshi"->stopEdit "fuutou naoshi は、いまは fuutou misekechi です。何もしていません。"
      _->say env usage
  where
    need args n=when(length args<n)(stopEdit usage)
    growthNotice name old new
      | name/="yuugao"=[]
      | otherwise=case yuugaoGrown old new of
          []->[]
          notices->map pyStringFromChars [concat["夕顔で、長くなった項目（封はしました。知らせるだけ。書き直せるなら、もう一度 open yuugao → close yuugao で）：\n",
                         intercalate "\n"(map(\line->concat["  ・",pyStringToChars line])notices)]]
