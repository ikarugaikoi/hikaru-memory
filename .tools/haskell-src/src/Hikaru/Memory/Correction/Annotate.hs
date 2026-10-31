module Hikaru.Memory.Correction.Annotate
  ( Frame(..), walk, hits, note, lineNotes, annotate, chunkNotes, fadeGuard ) where
import Control.Applicative ((<|>))
import Data.List (intercalate, isInfixOf, isPrefixOf, nub, sort, stripPrefix)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time (Day, addDays, fromGregorianValid, toGregorian)
import Hikaru.Memory.Text
import Hikaru.Memory.Fade (Guard(..))
import Hikaru.HouseDay (houseDayDate)
import Hikaru.Conversation.Types (Speaker(..))
import qualified Hikaru.Memory.Strobe as Strobe
import Hikaru.Memory.Correction.Types
import Hikaru.Memory.Correction.Parse
import Hikaru.Memory.Correction.Summary

data Frame=Frame{frameText::String,frameHeads::[String],frameWho::Maybe Speaker,frameDay::Maybe Day} deriving(Eq,Show)
walk :: String -> String -> Either CorrectionError [Frame]
walk input text
  | name=="strobe"=strobeWalk <$> either (Left . RecordSyntax) Right (Strobe.parseStrobe text)
  | "tm:" `isPrefixOf` name=Right $ tmWalk Nothing Nothing Nothing (splitLF(nfc text))
  | otherwise=Right $ otherWalk M.empty (if "ariake:" `isPrefixOf` name then base else Nothing) (splitLF(nfc text))
  where
    name=normName input
    base=case break(==':')name of (_,[])->Nothing; (_,_:rest)->dayOf rest
    tmWalk _ _ _ []=[]
    tmWalk top turn who (line:rest)=
      let role | "**ふ：**" `isPrefixOf` line=Just HumanSpeaker
               | "**Claude：**" `isPrefixOf` line=Just Claude
               | otherwise=Nothing
          (nextTop,nextTurn,nextWho)=case role of
            Just r->(top,Just line,Just r)
            Nothing->(if turn==Nothing && "# " `isPrefixOf` line then Just line else top,turn,who)
      in Frame line (filter(not.null)(mapMaybe id[nextTop,nextTurn])) nextWho base:tmWalk nextTop nextTurn nextWho rest
    otherWalk _ _ []=[]
    otherWalk stack day (line:rest)=
      let (nextStack,nextDay)=case markdownHeading line of
            Nothing->(stack,day)
            Just(level,heading)->(M.insert level line(M.filterWithKey(\k _->k<level)stack), dayUnder level heading day)
      in Frame line(M.elems nextStack)Nothing nextDay:otherWalk nextStack nextDay rest
    dayUnder _level heading day
      | "ichifure:" `isPrefixOf` name,Just initial<-base = case diaryDay heading of
          Nothing->day
          Just(month,d)->let(y,m,_)=toGregorian initial in fromMaybe day (Just <$> validDate (y+if month<m then 1 else 0) month d)
      | otherwise=day
    validDate y m d | y<1 || y>9999=Nothing | otherwise=fromGregorianValid y m d
    diaryDay heading=do
      (m,r1)<-decimal 1 2 heading;r2<-stripPrefix "月" r1
      (d,r3)<-decimal 1 2 r2;_<-stripPrefix "日" r3;pure(m,d)

-- Strobe headings and their dates have already been classified together.
-- An invalid date keeps the prior context, whereas a level 1/2 heading
-- without a date clears it. This display rule differs from fade selection.
strobeWalk :: Strobe.Strobe -> [Frame]
strobeWalk = go M.empty Nothing . Strobe.strobeLines
  where
    go _ _ [] = []
    go stack day (entry:rest) =
      let line=nfc (Strobe.entryText entry)
          (nextStack,nextDay)=case Strobe.entryHeading entry of
            Nothing -> (stack,day)
            Just (level,_) ->
              let next = case Strobe.entryHeadingDay entry of
                    Strobe.HeadingDay (Strobe.ValidDay d) -> Just (houseDayDate d)
                    Strobe.HeadingDay (Strobe.InvalidDay _ _ _) -> day
                    Strobe.NoHeadingDay -> if level<=2 then Nothing else day
              in (M.insert level line (M.filterWithKey (\k _->k<level) stack),next)
      in Frame line (M.elems nextStack) Nothing nextDay:go nextStack nextDay rest

headText :: String -> String
headText text=strip(maybe text snd(markdownHeading text))
hits :: Trace -> [Frame] -> String -> ([Int],String)
hits tr frames name
  | null matching=([],"手がかりの文字列が、その記録にない")
  | null byWho=([],"手がかりはあるが、だれの発話かが合わない")
  | null byPlace=([],"手がかりはあるが、位置が合わない")
  | otherwise=(byPlace,"")
  where
    matching=[i|(i,frame)<-zip[0..]frames,nfc(traceClue tr) `isInfixOf` frameText frame]
    byWho=if "tm:" `isPrefixOf` name && traceSpeaker tr/=Nothing then filter(\i->frameWho(frames!!i)==traceSpeaker tr)matching else matching
    pl=nfc(tracePlace tr)
    exact=[i|i<-byWho,any((==pl).headText)(frameHeads(frames!!i))]
    byPlace | null pl=byWho | not(null exact)=exact
            | otherwise=[i|i<-byWho,any(pl `isInfixOf`)(frameHeads(frames!!i))]

note :: Yuugao -> Revision -> Trace -> Frame -> Bool -> Either CorrectionError String
note ix r tr frame withNow=do
  summary<-case revisionKind r of
    Just Misread->Right(case who of Just Claude->"この受け取り方は、のちに直した"; Just HumanSpeaker->"言葉はこのまま。受け取り方を、のちに直した"; Nothing->"ここは、のちに直したことに関わる")
    Just Changed->do
      (start,shown,lastDay)<-since r
      case (start,frameDay frame) of
        (Just first,Just day) | day<first ->Right("このときは、このとおり。"++shown++" から変わった")
        (Just first,Just day)->do
          let boundary=fromMaybe(addDays(-1)first)lastDay
              (year,_,_)=toGregorian boundary
          if year<1 then Left BeforeCalendar else
            if day>boundary then Right(shown++" から変わっている。これは前の言い方") else Right(unknown shown)
        _->Right(unknown shown)
    Just Conflict->Right "ほかの記録と合わないところがある。まだ決めていない"
    _->Right "場合によって違う。ふだんと、このときを分けて読む"
  let newest=tip ix r
  after<-if revisionNo newest==revisionNo r then Right "" else (\s->"（"++s++"）") <$> afterText newest
  pure("⤷ 「"++traceClue tr++"」："++summary++after++"〔直 "++revisionId newest++"〕→ 夕顔「"++revisionEntryName r++"」"
       ++if withNow && not(null(nowSummary newest)) then "。"++nowSummary newest else "")
  where
    who=frameWho frame <|> traceSpeaker tr
    unknown shown=(if null shown then "あるとき" else shown)++" から変わった。その前のことなら、このとおり"
    afterText newest=case revisionKind newest of
      Just Misread->Right "そのあと、もう一度直している"
      Just Changed->do (_,shown,_)<-since newest;pure("そのあと、"++(if null shown then "あるとき" else shown)++" から変わった")
      Just Conflict->Right "そのあと、ほかの記録と合わないところが出て、まだ決めていない"
      Just Depends->Right "そのあと、場合によると分かった"
      _->Right "そのあと、また直している"

-- Ordered association lists retain the first hit's insertion order. Notes
-- are deduplicated per line, as Python list membership, not by sorting.
appendNote :: Int -> String -> [(Int,[String])] -> [(Int,[String])]
appendNote line message rows=case lookup line rows of
  Nothing->rows++[(line,[message])]
  Just _->map(\(i,notes)->(i,if i==line && message `notElem` notes then notes++[message] else notes))rows
lineNotes :: Yuugao -> String -> String -> Bool -> Either CorrectionError [(Int,[String])]
lineNotes ix input text withNow=do
  frames<-walk name text
  foldl (step frames) (Right []) pairs
  where
    name=normName input
    pairs=[(r,tr)|r<-indexRevisions ix,revisionKind r `elem` map Just kinds,tr<-revisionTraces r,not(null(traceClue tr)),normName(traceName tr)==name]
    step frames out (r,tr)=do
      rows<-out
      foldl(\prev i->do before<-prev;message<-note ix r tr(frames!!i)withNow;pure(appendNote i message before))(Right rows)(fst(hits tr frames name))
annotate :: Yuugao -> String -> String -> Bool -> Either CorrectionError (String,Int)
annotate ix name text withNow=do
  notes<-lineNotes ix name text withNow
  if null notes then pure(text,0) else
    pure(intercalate "\n" (concat[ line:[replicate(length(takeWhile(==' ')line)+2)' '++n | n<-fromMaybe[](lookup i notes)]
                                 | (i,line)<-zip[0..](splitLF text)]),sum(map(length.snd)notes))
chunkNotes :: Yuugao -> String -> [String] -> Bool -> Either CorrectionError [(Int,[String])]
chunkNotes ix name chunks withNow=do
  notes<-lineNotes ix name(intercalate "\n\n" chunks)withNow
  let step(at,out)(j,chunk)=
        let size=length(filter(=='\n')chunk)+1
            collected=[n|i<-[at..at+size-1],n<-fromMaybe[](lookup i notes)]
        in(at+size+1,foldl(\rows n->appendNote j n rows)out collected)
  pure(snd(foldl step(0,[])(zip[0..]chunks)))
fadeGuard :: Yuugao -> [Guard]
fadeGuard ix=snd(foldl step(S.empty,[]) [r|r<-indexRevisions ix,revisionKind r==Just Misread])
  where
    step(seen,out)r=foldl(add r)(seen,out)(revisionTraces r)
    add r (seen,out)tr
      | traceSpeaker tr==Just Claude,not(null(traceClue tr)),S.notMember(traceClue tr)seen=
          (S.insert(traceClue tr)seen,out++[Guard(traceClue tr)(sort(nub(map revisionId(chain ix r))))(revisionEntryName r)])
      | otherwise=(seen,out)
