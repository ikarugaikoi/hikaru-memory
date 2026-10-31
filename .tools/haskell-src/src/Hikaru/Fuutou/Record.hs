{-# LANGUAGE OverloadedStrings #-}
-- | Names and calendar addressing for sealed records. Resolving a name does
-- not create it or acquire a lock; the caller owns the transaction boundary.
module Hikaru.Fuutou.Record
  ( RecordEnv(..), Record(..), NameError(..), RecordSuffix, recordSuffix, suffixText
  , parseRecord, recordPath
  , resolve, readOrNew, readOptional, weekFile, parseDay, dayLike, pageArg
  , timeMachineDays, globNames, nameHelp, rereadReport, judgmentsJson ) where
import Control.Exception (catch)
import Control.Monad (when)
import Data.Char (ord)
import Data.List (elemIndex,isPrefixOf,isSuffixOf,sort,stripPrefix,intersperse)
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import Hikaru.Ochibo.Files (parseCollectDay)
import Hikaru.Compat.Json (pyStringFromChars)
import qualified Data.Aeson.Encoding as E
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (UnicodeException)
import Hikaru.Compat.Unicode (pyIsDecimal,pyDecimalValue)
import Hikaru.Compat.Digit (pyIsDigit)
import Hikaru.HouseDay (HouseDay, houseDayDate, houseDayNamed)
import Hikaru.Fuutou.Text (usage,shioriHead)
import qualified Hikaru.Store.Edit as Edit
import Hikaru.Store.Sealed (readSealed)
import System.Directory (listDirectory)
import System.FilePath ((</>),takeFileName,normalise,dropTrailingPathSeparator)
import System.IO.Error
import System.Posix.Files (getFileStatus, getSymbolicLinkStatus)

stopEdit :: String -> IO a
stopEdit = Edit.stopEdit . pyStringFromChars

data RecordEnv = RecordEnv
  { recordRoot :: FilePath, recordDiary :: FilePath
  , recordTimeMachine :: FilePath, recordAnalysis :: FilePath
  , recordShiori :: FilePath }

nameHelp :: String
nameHelp="oka / yuugao / strobe / biblia / shiori / ichifure / ichifure:YYYY-MM-DD / tm:YYYY-MM-DD / ariake:YYYY-MM-DD / michikusa:YYYY-MM-DD / omoigusa:名前"

-- Post-commit notice for seal.py read (also shown by read --check); not in
-- the Python baseline. It lists 判 lines that start with 「まだ分からない」 and,
-- per ひととき, one confident 判 chosen deterministically from the heading,
-- when no 「再読：」 line follows them. Counting only; never a guard.
rereadReport :: String -> [String]
rereadReport text
  | total==0 = []
  | null pending = [concat ["読み直し：",counts,"。どれも「再読：」が書いてあります。"]]
  | otherwise =
      ["読み直し（文献を開いてから。書き方は朝の手順 7）："]
      ++ concatMap blockLines blocks
      ++ [concat ["  「再読：」の行がまだないところ ",show (length pending),"。その判のすぐ下に書く（全部で ",counts,"）。"]]
  where
    blocks = map summarize (judgmentBlocks (lines text))
    total = sum [n | (_,n,_,_,_) <- blocks]
    pending = concat [todo | (_,_,_,_,todo) <- blocks]
    counts = concat ["判 ",show total,"・まだ分からない ",show (sum [u | (_,_,u,_,_) <- blocks]),
                     "・選んだ ",show (sum [p | (_,_,_,p,_) <- blocks])]
    unsure entry = "まだ分からない" `T.isPrefixOf` jeText entry
    summarize (heading,items) =
      let sure = filter (not . unsure) items
          seed = foldl' (\h c -> (h*131 + ord c) `mod` 1000003) 7 heading
          chosen = [sure !! (seed `mod` length sure) | not (null sure)]
          todo = [(entry,"判が「まだ分からない」") | entry <- filter unsure items, jeReread entry == Nothing]
              ++ [(entry,"言い切った判から、道具が選んだ一つ") | entry <- chosen, jeReread entry == Nothing]
      in (heading,length items,length (filter unsure items),length chosen,todo)
    blockLines (heading,n,u,p,todo)
      | null todo = []
      | otherwise = concat ["  ",drop 3 heading,"  判 ",show n,"・まだ分からない ",show u,"・選んだ ",show p]
          : [concat ["    まだ：",take 30 section,if null section then "" else "｜","判：",take 24 judgment,"（",why,"）"]
            | (entry,why) <- todo, let section=T.unpack (jeSection entry); judgment=T.unpack (jeText entry)]

-- | 「判：」「再読：」「しるし：」のような行の、印のあとの文。行頭の箇条の記号と空白は飛ばす。
markedAs :: String -> String -> Maybe String
markedAs key line = case stripPrefix key (dropWhile (`elem` (" \t-・*　" :: String)) line) of
  Just (c:rest) | c `elem` ("：:" :: String) -> Just (dropWhile (`elem` (" 　" :: String)) rest)
  _ -> Nothing

-- | ひとときを読んだ記録の、判の一つ一つ。こより（.tools/_koyori.py、記憶の流れの目録）が読む。
-- 行の読み方は rereadBlocks と同じ：「## ひととき」でひとときが始まり、「### 」で節が始まる。
-- 「しるし：」「再読：」は、あとの見出しより前にある、いちばん近い判のもの。
-- ひとときが始まる前の判は数えない（rereadBlocks と同じ）。行番号は 1 から。
data JudgmentEntry = JudgmentEntry
  { jeHitotoki :: T.Text, jeSection :: T.Text, jeLine :: Int, jeText :: T.Text
  , jeShirushi :: Maybe T.Text, jeReread :: Maybe T.Text }

-- An open run carries the judgment that following marks belong to.
data JudgmentRun = ClosedJudgments [JudgmentEntry] | FollowingJudgment JudgmentEntry [JudgmentEntry]

runEntries :: JudgmentRun -> [JudgmentEntry]
runEntries (ClosedJudgments entries) = entries
runEntries (FollowingJudgment entry entries) = entry:entries

judgmentBlocks :: [String] -> [(String,[JudgmentEntry])]
judgmentBlocks = reverse . map (\(heading,entries) -> (heading,reverse (runEntries entries)))
  . snd . foldl' step ("",[]) . zip [1..]
  where
    step (section,blocks) (n,line)
      | "## ひととき" `isPrefixOf` line = ("",(line,ClosedJudgments []):blocks)
      | "### " `isPrefixOf` line = (drop 4 line, case blocks of
          (heading,entries):rest -> (heading,ClosedJudgments (runEntries entries)):rest
          [] -> [])
      | (heading,entries):rest <- blocks =
          let keep current = (section,(heading,current):rest)
          in case () of
            _ | Just judgment <- markedAs "判" line ->
                  keep (FollowingJudgment (JudgmentEntry (T.pack heading) (T.pack section) n (T.pack judgment) Nothing Nothing) (runEntries entries))
              | FollowingJudgment e es <- entries, Just mark <- markedAs "しるし" line ->
                  keep (FollowingJudgment e{jeShirushi=Just (T.pack mark)} es)
              | FollowingJudgment e es <- entries, Just reread <- markedAs "再読" line ->
                  keep (FollowingJudgment e{jeReread=Just (T.pack reread)} es)
              | otherwise -> (section,blocks)
      | otherwise = (section,blocks)

-- | judgmentEntries を JSON の配列に（fuutou judgments ariake:日）。読むだけで、何も書かない。
judgmentsJson :: T.Text -> T.Text
judgmentsJson = TE.decodeUtf8 . BL.toStrict . E.encodingToLazyByteString
  . E.unsafeToEncoding . entries . concatMap snd . judgmentBlocks . lines . T.unpack
  where
    entries [] = "[]"
    entries xs = "[\n " <> mconcat (intersperse ",\n " (map entry xs)) <> "\n]"
    entry e = "{\n" <> mconcat (intersperse ",\n"
      [ field "hitotoki" (text (jeHitotoki e)), field "section" (text (jeSection e))
      , field "line" (E.int (jeLine e)), field "judgment" (text (jeText e))
      , field "shirushi" (maybe E.null_ text (jeShirushi e))
      , field "reread" (maybe E.null_ text (jeReread e)) ]) <> "\n }"
    field name value = "  \"" <> B.string8 name <> "\": " <> E.fromEncoding value
    text value
      | T.any (`elem` ['\b','\f']) value =
          let compact = TE.decodeUtf8 (BL.toStrict (E.encodingToLazyByteString (E.text value)))
              escaped = T.pack (shortEscapes (T.unpack compact))
          in E.unsafeToEncoding (B.byteString (TE.encodeUtf8 escaped))
      | otherwise = E.text value
    -- aeson spells these two controls as u0008/u000c; the file contract uses
    -- the short escapes. Consume other escape pairs together so literal
    -- backslash-u text is never rewritten.
    shortEscapes ('\\':'u':'0':'0':'0':'8':rest) = '\\':'b':shortEscapes rest
    shortEscapes ('\\':'u':'0':'0':'0':'c':rest) = '\\':'f':shortEscapes rest
    shortEscapes ('\\':c:rest) = '\\':c:shortEscapes rest
    shortEscapes (c:rest) = c:shortEscapes rest
    shortEscapes [] = []

dayLike :: String -> Bool
dayLike value=case value of
  [a,b,c,d,'-',e,f,'-',g,h]->all pyIsDecimal[a,b,c,d,e,f,g,h]
  _->False

-- | 引数の YYYY-MM-DD を読む。読めないときは、そのまま見せる止め文。
readDay :: String -> Either String Day
readDay text
  | not(dayLike text)=Left(concat["日付は YYYY-MM-DD で: ",text])
  | otherwise=maybe(Left(concat["日付が読めません: ",text]))Right(parseCollectDay (pyStringFromChars text))

parseDay :: String -> IO Day
parseDay=either stopEdit pure.readDay

weekFile :: RecordEnv -> Day -> IO (FilePath,Day,Day)
weekFile env day=do
  let (_,_,weekday)=toWeekDate day
      monday=addDays(fromIntegral(1-weekday))day
      sunday=addDays 6 monday
      valid d=let(y,_,_)=toGregorian d in y>=1 && y<=9999
  when(not(valid monday && valid sunday))(stopEdit ("OverflowError: diary week for " ++ show day ++ " is outside years 1..9999"))
  pure(recordDiary env </> show monday++"〜"++formatTime defaultTimeLocale "%m-%d" sunday++".md.b64",monday,sunday)

-- Only a genuinely absent directory has no records yet.
globNames :: FilePath -> IO [FilePath]
globNames root = maybe [] id <$> readOptional
  "cannot read record directory" root (listDirectory root)

-- Read first. ENOENT alone is ambiguous for a dangling symlink, so inspect
-- the directory entry only on that failure path. No other IO error is absence.
readOptional :: String -> FilePath -> IO a -> IO (Maybe a)
readOptional message path action =
  ((Just <$> action) `catchIOError` missing) `catch` \(_ :: UnicodeException) ->
    stopEdit ("UnicodeDecodeError: " ++ message ++ ": " ++ path ++ ": invalid UTF-8")
  where
    missing err
      | isDoesNotExistError err = do
          entry <- tryIOError (getSymbolicLinkStatus path)
          case entry of
            Left absent | isDoesNotExistError absent -> pure Nothing
            Left failure -> ioError failure
            Right _ -> ioError err
      | isUserError err = stopEdit ("ValueError: " ++ message ++ ": " ++ path ++ ": " ++ ioeGetErrorString err)
      | otherwise = ioError err

-- | 記録の名前（`fuutou show oka`、`tm:2026-10-20` など）。名前を読むことと、
-- その記録がどこにあるかを探すことを分けてある（2026-10-22、重构清单の第 3 步）。
-- 新しい記録を足すときは、ここに一つ足して、'parseRecord' と 'recordPath' を直す。
data Record
  = Oka | Yuugao | Strobe | Biblia | Shiori
  | DiaryLatest            -- ^ diary：いちばん新しい週の日記
  | DiaryOf HouseDay       -- ^ ichifure:YYYY-MM-DD：その日を含む週の日記
  | TimeMachine RecordSuffix  -- ^ tm:…：その日の原文。日付の形は確かめず、ファイル名に足す
  | Ariake RecordSuffix       -- ^ ariake:…：その日のひとときを読んだ記録
  | Michikusa RecordSuffix    -- ^ michikusa:…：その日の、自分の時間の足跡（2026-10-28。時守が書き足す。わたしは show で読む）
  | Omoigusa RecordSuffix     -- ^ omoigusa:…：自分の時間に書いたもの・作ったもの一篇（2026-10-28。時守がしまう。わたしは show で読む）
  deriving (Eq, Show)

-- | tm: と ariake: のあとの文字列。ファイル名にそのまま足すので、/ を含むものは
-- その記録のフォルダの外を指せてしまう。'recordSuffix' で「/ を含まない」と
-- 確かめたものしか、この型にならない（構成子は外に出さない）。だから
-- 'TimeMachine' や 'Ariake' を、名前を読まずにじかに作っても、外は指せない
-- Project-local implementation note.
-- `tm:../x` が通っていた。`tm:./2026-10-20` のような書き方も、いまは通らない）。
newtype RecordSuffix = RecordSuffix String
  deriving (Eq, Show)

recordSuffix :: String -> Maybe RecordSuffix
recordSuffix text=if '/' `elem` text then Nothing else Just(RecordSuffix text)

suffixText :: RecordSuffix -> String
suffixText(RecordSuffix text)=text

-- | 名前が読めなかったわけ。どれも、そのまま止め文になる。
data NameError
  = BadDay String        -- ^ ichifure: のあとの日付が読めない（文はそのまま見せる）
  | Renamed String       -- ^ 前の名前（index）
  | Unknown String
  deriving (Eq, Show)

nameErrorText :: NameError -> String
nameErrorText (BadDay message)=message
nameErrorText (Renamed message)=message
nameErrorText (Unknown name)=concat["名前が分かりません: ",name,"（",nameHelp,"）"]

-- | 名前を読むだけ。ファイルは見ない。
parseRecord :: String -> Either NameError Record
parseRecord name=case name of
  "oka"->Right Oka
  "yuugao"->Right Yuugao
  "strobe"->Right Strobe
  "biblia"->Right Biblia
  "shiori"->Right Shiori
  "ichifure"->Right DiaryLatest
  "index"->Left(Renamed "index は、いまは yuugao です。何もしていません。")
  _ | Just text<-stripPrefix "ichifure:" name->either(Left . BadDay)(Right . DiaryOf . houseDayNamed)(readDay text)
    | Just day<-stripPrefix "tm:" name->maybe(Left(Unknown name))(Right . TimeMachine)(recordSuffix day)
    | Just day<-stripPrefix "ariake:" name->maybe(Left(Unknown name))(Right . Ariake)(recordSuffix day)
    | Just day<-stripPrefix "michikusa:" name->maybe(Left(Unknown name))(Right . Michikusa)(recordSuffix day)
    | Just piece<-stripPrefix "omoigusa:" name->maybe(Left(Unknown name))(Right . Omoigusa)(recordSuffix piece)
    | otherwise->Left(Unknown name)

-- | その記録のファイルの場所。日記は、あるかどうかを見てから返す（ないときは止まる）。
recordPath :: RecordEnv -> Record -> IO FilePath
recordPath env record=case record of
  Oka->pure(recordRoot env </> ".丘の上.md.b64")
  Yuugao->pure(recordRoot env </> ".夕顔.md.b64")
  Strobe->pure(recordRoot env </> ".ストロボメモリー.md.b64")
  Biblia->pure(recordRoot env </> ".ビブリア.md.b64")
  Shiori->pure(recordShiori env)
  DiaryLatest->do
    files<-sort.filter(isSuffixOf ".b64") <$> globNames(recordDiary env)
    case reverse files of
      x:_->pure(recordDiary env </> x)
      []->stopEdit "日記はまだありません。"
  DiaryOf day->do
    (target,_,_)<-weekFile env(houseDayDate day)
    existing<-readOptional "cannot read record" target (getFileStatus target)
    case existing of
      Just _ -> pure target
      Nothing -> stopEdit(concat["その週の日記はまだありません: ",takeFileName target])
  TimeMachine day->pure(path(recordTimeMachine env)(suffixText day++".md.b64"))
  Ariake day->pure(path(recordAnalysis env)(suffixText day++".read.md.b64"))
  Michikusa day->pure(path(recordRoot env </> ".みちくさ")(suffixText day++".md.b64"))
  Omoigusa piece->pure(path(recordRoot env </> ".思ひ草")(suffixText piece++".md.b64"))
  where path root value=dropTrailingPathSeparator(normalise(root </> value))

resolve :: RecordEnv -> String -> IO FilePath
resolve env name=either(stopEdit.nameErrorText)(recordPath env)(parseRecord name)

readOrNew :: RecordEnv -> FilePath -> IO T.Text
readOrNew env path=do
  existing<-readOptional "cannot read record" path (readSealed path)
  case existing of
    Just body -> pure body
    Nothing | path==recordShiori env -> pure (T.pack shioriHead)
            | otherwise -> stopEdit(concat["まだありません: ",takeFileName path])

pageArg :: [String] -> Maybe Integer -> IO (Maybe Integer)
pageArg args default_=case elemIndex "--page" args of
  Nothing->pure default_
  Just n->case drop(n+1)args of
    value:_ | not (null value) && all pyIsDigit value ->
      case traverse pyDecimalValue value of
        Just digits | length digits <= 4300 -> pure (Just (foldl' (\total d -> total*10 + toInteger d) 0 digits))
        Just _ -> stopEdit "ValueError: --page exceeds the limit of 4300 digits"
        Nothing -> stopEdit "ValueError: --page contains a non-decimal digit"
    _->stopEdit(concat["--page のあとにはページの数字を。\n",usage])

timeMachineDays :: RecordEnv -> IO [Day]
timeMachineDays env=do
  names<-globNames(recordTimeMachine env)
  pure(sort[day | name<-names,length name==17,name!!4=='-',name!!7=='-',".md.b64" `isSuffixOf` name,
                  Just day<-[parseCollectDay(pyStringFromChars(take 10 name))]])
