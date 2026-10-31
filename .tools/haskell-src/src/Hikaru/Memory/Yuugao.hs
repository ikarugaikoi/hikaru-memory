-- | Lossless yuugao syntax and its typed correction projection. Constructors
-- and record labels stay in this module: spelling, structure and meaning are
-- produced together, and cannot be updated independently by a caller.
module Hikaru.Memory.Yuugao
  ( Yuugao, Category, Entry, Node, Revision, Trace, Meaning, EntryKey, RevisionKey
  , Kind(..), FieldName(..), CorrectionError(..), Problem(..)
  , parseYuugao, renderYuugao, categories, categoryName, categoryEntries
  , entryNodes, nodeText, nodeField, nodeChildren, meaningKind, meaningValues
  , indexLines, indexEntries, indexRevisions, indexById, indexNext, indexProblems
  , entryNo, entryName, entryStart, entryEnd, entryBlock, entryRevisions
  , revisionNo, revisionId, revisionKind, revisionKindText, revisionMeaning, revisionMissing
  , revisionEntry, revisionEntryName, revisionTraces, revisionPrev, revisionStart, revisionEnd
  , revisionSource, revisionLines
  , traceName, tracePlace, traceSpeaker, traceClue, traceLine, traceValidName
  , value, revisionAt, entryAt, kinds, retireKinds, fields, sources, needed, kindName, fieldName
  , reference, markdownHeading, ymdSearch, ymSearch, dayOf, since, normName
  , growthFields, grown
  ) where

import Control.Applicative ((<|>))
import Data.Char (isAscii, isDigit)
import Data.List (find, intercalate, isPrefixOf, stripPrefix)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Data.Time (Day, addDays, fromGregorianValid, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, choice, count, eof, errorBundlePretty,
                        many, match, parse, parseMaybe, satisfy, some, takeRest,
                        takeWhileP, takeWhile1P, try)
import Text.Megaparsec.Char (char, string)
import Hikaru.Memory.Text
import Hikaru.Conversation.Types (Speaker(..))

data Kind = Misread | Changed | Conflict | Depends deriving (Eq, Ord, Show, Enum, Bounded)
data FieldName = Aliases | Description | Words | Connections | DateField | Corrections
  | Previous | Before | Now | Since | OneSide | OtherSide | Usually | ThisTime
  | Reason | Evidence | Scope | Remaining | UnknownField String deriving (Eq, Ord, Show)
data CorrectionError = YuugaoSyntax String | RecordSyntax String | SinceAfterCalendar | BeforeCalendar
  deriving (Eq, Show)
newtype EntryKey = EntryKey Int deriving (Eq, Ord, Show)
newtype RevisionKey = RevisionKey Int deriving (Eq, Ord, Show)
-- No constructor accepts an empty required value outside this module.
newtype Required = Required String deriving (Eq, Show)
data Meaning = Misreading Required Required | Change Required Required
             | Disagreement Required Required | Conditional Required Required deriving (Eq, Show)
data Ending = LF | End deriving (Eq, Show)
data Role = Heading Int String | BlockHead String | RevisionHead String String
          | FieldHead FieldName String (Maybe FieldName) Bool | Prose deriving (Eq, Show)
data Line = Line Int String Ending Role deriving (Eq, Show)
data Node = Node Line [Node] deriving (Eq, Show)
data Piece = Loose [Line] | Item Entry deriving (Eq, Show)
data Category = Category (Maybe Line) [Piece] deriving (Eq, Show)
data Trace = Trace String String (Maybe Speaker) String Int Bool deriving (Eq, Show)
data DraftRevision = DraftRevision
  { rNo :: RevisionKey, rId :: String, rKind :: Maybe Kind, rKindText :: String
  , rEntry :: EntryKey, rEntryName :: String, rFields :: [(FieldName,String)]
  , rTraces :: [Trace], rPrev :: Maybe String, rStart :: Int, rEnd :: Int, rIndent :: Int
  } deriving (Eq, Show)
-- A complete reading carries precisely the required fields for its kind.
-- Incomplete/unknown drafts retain their source and diagnostics, never an
-- invented empty value masquerading as a complete reading.
data RevisionBody = Understood Meaning | Missing Kind [FieldName] | Unrecognized String
  deriving (Eq, Show)
data Revision = Revision DraftRevision RevisionBody
  (Either CorrectionError (Maybe Day,String,Maybe Day)) (String,String) deriving (Eq, Show)
data Building = Building
  { buildRows :: V.Vector Line, buildEntries :: [Entry], buildRevisions :: [DraftRevision]
  , buildById :: M.Map String RevisionKey, buildNext :: M.Map String RevisionKey
  , buildProblems :: [Problem] }
data Entry = Entry
  { eNo :: EntryKey, eName :: String, eStart :: Int, eEnd :: Int
  , eBlock :: Maybe (Int,Int), eRevisions :: [RevisionKey], eNodes :: [Node]
  } deriving (Eq, Show)
data Problem = Problem { problemLine :: Int, problemEntry :: EntryKey, problemId :: Maybe String
                       , problemMessage :: String, problemStop :: Bool } deriving (Eq, Show)
data Yuugao = Yuugao
  { rows :: V.Vector Line, docCategories :: [Category], entries :: [Entry]
  , revisions :: [Revision], byId :: M.Map String RevisionKey
  , next :: M.Map String RevisionKey, problems :: [Problem] } deriving (Eq, Show)

kinds, retireKinds :: [Kind]
kinds = [minBound .. maxBound]
retireKinds = [Misread, Changed]
kindName :: Kind -> String
kindName Misread = "読み違い"
kindName Changed = "変わった"
kindName Conflict = "食い違い"
kindName Depends = "場合による"
fields :: [FieldName]
fields = [Previous, Before, Now, Since, OneSide, OtherSide, Usually, ThisTime, Reason, Evidence, Scope, Remaining]
sources :: [String]
sources = ["ユーザーの言葉","一緒に決めた","わたしが気づいた"]
fieldName :: FieldName -> String
fieldName Aliases = "別名"
fieldName Description = "説明"
fieldName Words = "ユーザーの言葉"
fieldName Connections = "つながり"
fieldName DateField = "日付"
fieldName Corrections = "みせけち"
fieldName Previous = "前の直し"
fieldName Before = "前"
fieldName Now = "いま"
fieldName Since = "いつから"
fieldName OneSide = "一方"
fieldName OtherSide = "もう一方"
fieldName Usually = "ふだん"
fieldName ThisTime = "このとき"
fieldName Reason = "なぜ"
fieldName Evidence = "よりどころ"
fieldName Scope = "範囲"
fieldName Remaining = "残っているところ"
fieldName (UnknownField name) = name
fieldKind :: String -> FieldName
fieldKind name = fromMaybe (UnknownField name) (find ((==name) . fieldName)
  ([Aliases,Description,Words,Connections,DateField,Corrections] ++ fields))
needed :: Kind -> [FieldName]
needed Misread = [Before,Now]
needed Changed = [Before,Now]
needed Conflict = [OneSide,OtherSide]
needed Depends = [Usually,ThisTime]
draftValue :: FieldName -> DraftRevision -> String
draftValue key = fromMaybe "" . lookup key . rFields

makeRevision :: DraftRevision -> Revision
makeRevision draft = Revision draft body (readSince (draftValue Since draft)) sourceValue
  where
    required key = case draftValue key draft of "" -> Nothing; text -> Just (Required text)
    meaning kind = case kind of
      Misread -> Misreading <$> required Before <*> required Now
      Changed -> Change <$> required Before <*> required Now
      Conflict -> Disagreement <$> required OneSide <*> required OtherSide
      Depends -> Conditional <$> required Usually <*> required ThisTime
    body = case rKind draft of
      Nothing -> Unrecognized (rKindText draft)
      Just kind -> case meaning kind of
        Just complete -> Understood complete
        Nothing -> Missing kind (filter (null . (`draftValue` draft)) (needed kind))
    evidence=draftValue Evidence draft
    sourceValue=(fromMaybe "" (find (`isPrefixOf` evidence) sources),
                 fromMaybe (take 10 (rId draft)) (ymdSearch evidence))

syntax :: Revision -> DraftRevision
syntax (Revision draft _ _ _) = draft
revisionMeaning :: Revision -> Maybe Meaning
revisionMeaning (Revision _ (Understood meaning) _ _) = Just meaning
revisionMeaning (Revision _ (Missing _ _) _ _) = Nothing
revisionMeaning (Revision _ (Unrecognized _) _ _) = Nothing
revisionMissing :: Revision -> [FieldName]
revisionMissing (Revision _ (Missing _ missing) _ _) = missing
revisionMissing (Revision _ (Understood _) _ _) = []
revisionMissing (Revision _ (Unrecognized _) _ _) = []
revisionSource :: Revision -> (String,String)
revisionSource (Revision _ _ _ evidence) = evidence

-- Required values of complete revisions are read from their sum type. Drafts
-- still expose partial values for the historical listing/annotation behaviour.
value :: FieldName -> Revision -> String
value key revision = case revisionMeaning revision of
  Just meaning -> case (key,meaning) of
    (Before,Misreading (Required text) _) -> text
    (Now,Misreading _ (Required text)) -> text
    (Before,Change (Required text) _) -> text
    (Now,Change _ (Required text)) -> text
    (OneSide,Disagreement (Required text) _) -> text
    (OtherSide,Disagreement _ (Required text)) -> text
    (Usually,Conditional (Required text) _) -> text
    (ThisTime,Conditional _ (Required text)) -> text
    _ -> draftValue key (syntax revision)
  Nothing -> draftValue key (syntax revision)
meaningKind :: Meaning -> Kind
meaningKind (Misreading _ _) = Misread
meaningKind (Change _ _) = Changed
meaningKind (Disagreement _ _) = Conflict
meaningKind (Conditional _ _) = Depends
meaningValues :: Meaning -> (String,String)
meaningValues meaning = let unpack (Required a) (Required b) = (a,b) in case meaning of
  Misreading a b -> unpack a b
  Change a b -> unpack a b
  Disagreement a b -> unpack a b
  Conditional a b -> unpack a b

type Parser = Parsec Void String
lineRest :: Parser String
lineRest = takeWhileP Nothing (/='\n')
-- These productions deliberately distinguish ASCII indentation, full-width
-- colons, and the generic Markdown grammar used for annotation positions.
roleP :: Parser Role
roleP = choice [try headingP, try blockP, try revisionP, try fieldP, Prose <$ lineRest]
  where
    headingP = do
      hashes <- some (char '#')
      if length hashes <= 3 then pure () else fail "entry heading level"
      _ <- char ' '
      Heading (length hashes) <$> lineRest
    blockP = BlockHead . strip <$> (string "- みせけち" *> satisfy (`elem` ":：") *> lineRest)
    revisionP = do
      _ <- some (char ' ')
      _ <- string "- 〔直 "
      rid <- takeWhileP Nothing (\c -> c/='〕' && c/='\n') <* char '〕'
      RevisionHead (strip rid) . strip <$> lineRest
    fieldP = do
      padding <- many (char ' ')
      _ <- string "- "
      key <- takeWhile1P Nothing (\c -> c/='\n' && c `notElem` ":：「｜|")
      separator <- satisfy (`elem` ":：")
      val <- strip <$> lineRest
      let counted = if null padding && separator==':' then lookup key [("説明",Description),("ユーザーの言葉",Words)] else Nothing
      pure (FieldHead (fieldKind (strip key)) val counted (not (null padding) && key=="いま"))

parseYuugao :: String -> Either CorrectionError Yuugao
parseYuugao text = build <$> either (Left . YuugaoSyntax . errorBundlePretty) Right (parse documentP "yuugao" text)
  where
    documentP :: Parser [Line]
    documentP = do
      first <- match roleP
      rest <- many (char '\n' *> match roleP)
      eof
      pure (makeLines 0 (first:rest))
    makeLines _ [] = []
    makeLines n [(raw,role)] = [Line n raw End role]
    makeLines n ((raw,role):rest) = Line n raw LF role : makeLines (n+1) rest

lineText :: Line -> String
lineText (Line _ raw _ _) = raw
lineRole :: Line -> Role
lineRole (Line _ _ _ role) = role
lineNumber :: Line -> Int
lineNumber (Line number _ _ _) = number
indent :: Line -> Int
indent = length . takeWhile space . lineText
blank :: Line -> Bool
blank = null . strip . lineText
entryHeading :: Line -> Maybe String
entryHeading row = case lineRole row of
  Heading 3 rest | not (null rest) -> let name=rstrip rest in Just (if null name then take 1 rest else name)
  _ -> Nothing
blockHeading :: Line -> Maybe String
blockHeading row = case lineRole row of BlockHead rest -> Just rest; _ -> Nothing
revHeading :: Line -> Maybe (String,String)
revHeading row = case lineRole row of RevisionHead rid kind -> Just (rid,kind); _ -> Nothing
fieldHeading :: Line -> Maybe (FieldName,String)
fieldHeading row = case lineRole row of
  FieldHead key val _ _ | " " `isPrefixOf` lineText row -> Just (key,val)
  _ -> Nothing

markdownHeading :: String -> Maybe (Int,String)
markdownHeading = parseMaybe markdownP
markdownP :: Parser (Int,String)
markdownP = do
  hashes <- some (char '#')
  if length hashes<=6 then pure () else fail "heading level"
  _ <- satisfy space
  (length hashes,) . strip <$> takeRest

search :: Parser a -> String -> Maybe a
search production = parseMaybe (scan <* takeRest)
  where scan = try production <|> (anySingle *> scan)
reference :: String -> Maybe String
reference = search (string "〔直 " *> takeWhileP Nothing (/='〕') <* char '〕')
digitP :: Parser Char
digitP = satisfy (\c -> case decimal 1 1 [c] of Just _ -> True; Nothing -> False)
ymP, ymdP :: Parser String
ymP = fst <$> match (count 4 digitP *> char '-' *> count 2 digitP)
ymdP = fst <$> match (ymP *> char '-' *> count 2 digitP)
validId :: String -> Bool
validId = maybe False (const True) . parseMaybe (ymdP *> satisfy (\c -> c>='a' && c<='z'))
validTraceName :: String -> Bool
validTraceName = maybe False (const True) . parseMaybe (choice
  [() <$ string "strobe", () <$ string "shiori",
   choice (map string ["ichifure:","ariake:","tm:"]) *> (() <$ ymdP)])
ymdSearch, ymSearch :: String -> Maybe String
ymdSearch = search ymdP
ymSearch = search ymP
dayOf :: String -> Maybe Day
dayOf text = do
  raw <- ymdSearch text
  if all (\c -> c=='-' || (isAscii c && isDigit c)) raw then pure () else Nothing
  (year,_) <- decimal 4 4 raw
  (month,_) <- decimal 2 2 (drop 5 raw)
  (day,_) <- decimal 2 2 (drop 8 raw)
  if year>=1 && year<=9999 then fromGregorianValid (fromIntegral year) month day else Nothing

since :: Revision -> Either CorrectionError (Maybe Day,String,Maybe Day)
since (Revision _ _ period _) = period

readSince :: String -> Either CorrectionError (Maybe Day,String,Maybe Day)
readSince text = case dayOf text of
  Just day -> Right (Just day,show day,Nothing)
  Nothing -> case ymSearch text of
    Nothing -> Right (Nothing,"",Nothing)
    Just raw -> let ym=do (year,_)<-decimal 4 4 raw; (month,_)<-decimal 2 2(drop 5 raw); pure(year,month)
                in case ym of
      Just(year,month) | year>=1,year<=9999,month>=1,month<=12 ->
        if year==9999 && month==12 then Left SinceAfterCalendar else
          Right (fromGregorianValid (fromIntegral year) month 1,raw,
                 fromGregorianValid (fromIntegral year) month (gregorianMonthLength (fromIntegral year) month))
      _ -> Right (Nothing,"",Nothing)
normName :: String -> String
normName input = let name=strip input in case stripPrefix "ichifure:" name >>= dayOf of
  Nothing -> name
  Just day -> let (_,_,weekday)=toWeekDate day
              in "ichifure:"++show (addDays (fromIntegral (1-weekday)) day)

trace :: String -> Int -> Maybe Trace
trace line number = do
  let body0=strip line; body=fromMaybe body0 (stripPrefix "- " body0)
  (prefix,clue) <- parseMaybe traceP body
  let name :| tailParts = fmap strip (splitBars prefix)
      (who,places)=case reverse tailParts of
        "ふ":xs -> (Just HumanSpeaker,reverse xs)
        "わたし":xs -> (Just Claude,reverse xs)
        "":xs -> (Nothing,reverse xs)
        _ -> (Nothing,tailParts)
  pure (Trace name (intercalate "｜" (filter (not.null) places)) who clue number (validTraceName name))
  where
    traceP :: Parser (String,String)
    traceP = do
      prefix <- takeWhileP Nothing (/='「') <* char '「'
      rest <- takeRest
      case reverse (rstrip rest) of '」':back -> pure (prefix,reverse back); _ -> fail "trace quote"
    splitBars text=case break (`elem` "｜|") text of
      (a,[]) -> a :| []
      (a,_:b) -> let first :| rest=splitBars b in a :| (first:rest)

build :: [Line] -> Yuugao
build input = finish (withNext (foldl addEntry initial (zip (map EntryKey [0..]) starts)))
  where
    ls=input
    initial=Building (V.fromList ls) [] [] M.empty M.empty []
    starts=[(i,name) | (i,line)<-zip [0..] ls,Just name<-[entryHeading line]]
    at i=buildRows initial V.! i
    range a b=[a..b-1]
    ends row=case lineRole row of Heading _ _ -> True; _ -> False
    addEntry ix (number,(start,name)) =
      let end=fromMaybe(length ls)(find (ends.at) (range(start+1)(length ls)))
          ent=Entry number name start end Nothing [] (nest (take (end-start-1) (drop (start+1) ls)))
          heads=filter (\i->blockHeading(at i)/=Nothing)(range(start+1)end)
          prob i msg stop rid=Problem i number rid msg stop
          stray a b=[prob i ("直しが「- みせけち:」の下にありません（その下に、空白二つ字下げして書く）: "++snip 40(lineText(at i))) True Nothing
                     | i<-range a b,"- 〔直 " `isPrefixOf` dropWhile space(lineText(at i))]
      in case heads of
        [] -> ix { buildEntries=buildEntries ix++[ent], buildProblems=buildProblems ix++stray(start+1)end }
        b:others ->
          let endBlock0=fromMaybe end(find (\i->not(null(strip(lineText(at i)))) && not(any (`isPrefixOf` lineText(at i)) [" ","\t"])) (range(b+1)end))
              trim z | z>b+1 && null(strip(lineText(at(z-1)))) = trim(z-1) | otherwise=z
              endBlock=trim endBlock0
              before=[prob j "「- みせけち:」が二つあります（一つの項目に一つ）" True Nothing | j<-take 1 others]
                    ++[prob b "「- みせけち:」の行には何も続けず、その下に一件ずつ書きます" True Nothing | blockHeading(at b)/=Just ""]
              ix0=ix { buildProblems=buildProblems ix++before }
              (ix1,ids)=parseBlock ix0 ent b endBlock
              ix2=ix1{buildProblems=buildProblems ix1++stray(start+1)b++stray endBlock end,
                      buildEntries=buildEntries ix1++[ent{eBlock=Just(b,endBlock),eRevisions=ids}]}
          in ix2
    withNext ix=ix{buildNext=M.fromList[(p,rNo r)|r<-buildRevisions ix,Just p<-[rPrev r],not(null p)]}

parseBlock :: Building -> Entry -> Int -> Int -> (Building,[RevisionKey])
parseBlock initial ent begin endBlock =
  let (done,_,_,_,ids)=foldl step(initial,Nothing,Nothing,0,[]) [begin+1..endBlock-1]
      trim r=let go end | end>rStart r+1 && null(strip(at(end-1)))=go(end-1) | otherwise=end
              in r{rEnd=go(rEnd r)}
  in (done{buildRevisions=map(\r->if rNo r `elem` ids then trim r else r)(buildRevisions done)},ids)
  where
    row i=buildRows initial V.! i
    at i=lineText (row i)
    problem ix i msg stop rid=ix{buildProblems=buildProblems ix++[Problem i(eNo ent)rid msg stop]}
    update ix n fn=ix{buildRevisions=map(\r->if rNo r==n then fn r else r)(buildRevisions ix)}
    step state@(ix,cur,fld,fldInd,ids) i
      | null(strip line)=state
      | '\t' `elem` lead=(problem ix i("字下げにタブがあります（空白で）: "++snip 40 line)True currentId,cur,fld,fldInd,ids)
      | Just(rid,kind)<-revHeading (row i) =
          let closed=maybe ix(\priorNo->update ix priorNo(\prior->prior{rEnd=i}))cur
              n=RevisionKey (length(buildRevisions closed))
              r=DraftRevision n rid (find ((==kind) . kindName) kinds) kind(eNo ent)(eName ent)[] [] Nothing i endBlock ind
              added=closed{buildRevisions=buildRevisions closed++[r]}
              idChecked
                | not(validId rid)=problem added i("番号の形が違います: 〔直 "++rid++"〕（〔直 2026-10-04a〕のように、日付と小文字一つ）")True(Just rid)
                | Just old<-M.lookup rid(buildById added)=problem added i("番号が重なっています: 〔直 "++rid++"〕（項目「"++rEntryName(draftAt added old)++"」にもう同じ番号がある）")True(Just rid)
                | otherwise=added{buildById=M.insert rid n(buildById added)}
              kindChecked=if kind `elem` map kindName kinds then idChecked else problem idChecked i("種類が違います: 「"++kind++"」（"++intercalate "／" (map kindName kinds)++" のどれか。番号のあとは種類だけ）")True(Just rid)
          in (kindChecked,Just n,Nothing,0,ids++[n])
      | Nothing<-cur=(problem ix i("直しの頭（- 〔直 2026-10-04a〕読み違い）より前に行があります: "++snip 40 line)True Nothing,cur,fld,fldInd,ids)
      | Just n<-cur =
          let r=draftAt ix n; rid=Just(rId r)
              done v=(v,cur,fld,fldInd,ids)
          in if fld==Just Remaining && ind>fldInd then case trace line i of
               Nothing -> done(problem ix i("残っているところの一件が読めません（名前｜位置｜だれ「手がかり」）: "++snip 40 line)False rid)
               Just tr -> done(update ix n(\rev->rev{rTraces=rTraces rev++[tr]}))
             else case fieldHeading (row i) of
               Just(key,val) | ind>rIndent r ->
                 let duplicate=key `elem` map fst(rFields r)
                     stored=update ix n(\rev->rev{rFields=rFields rev++[(key,val)]})
                     known | key `elem` map UnknownField ["tm","ariake","ichifure"]=problem stored i("残っているところの一件は、「- 残っているところ:」の下に、もう一段字下げして書きます: "++snip 40 line)True rid
                           | key `notElem` fields=problem stored i("知らない欄です: 「"++fieldName key++"」（"++intercalate "／" (map fieldName fields)++"）")False rid
                           | otherwise=stored
                     prior | key==Previous=case reference val of
                               Nothing->problem known i "「前の直し」には番号を書きます（- 前の直し: 〔直 2026-10-04a〕）" True rid
                               Just p->update known n(\rev->rev{rPrev=Just(strip p)})
                           | otherwise=known
                     withTraces=if key==Remaining && not(null val) then problem prior i("残っているところは、その下に一件ずつ（一行に並べない）: "++snip 40 val)False rid else prior
                     final=if duplicate then problem ix i("「"++fieldName key++"」が二つあります")True rid else withTraces
                 in (final,cur,Just key,ind,ids)
               _ | Just key<-fld,key/=Remaining,ind>fldInd,not("- " `isPrefixOf` dropWhile space line) ->
                     done(update ix n(\rev->rev{rFields=map(\(k,v)->(k,if k==key then v++strip line else v))(rFields rev)}))
                 | otherwise -> done(problem ix i("読めない行です: "++snip 40 line)True rid)
      where line=at i; lead=takeWhile space line; ind=length lead
            currentId=rId . draftAt ix <$> cur

finish :: Building -> Yuugao
finish draft = arrange (Yuugao (buildRows draft) [] (buildEntries draft)
  (map makeRevision (buildRevisions draft)) (buildById draft) (buildNext draft) (buildProblems draft))

draftAt :: Building -> RevisionKey -> DraftRevision
draftAt doc (RevisionKey n) = buildRevisions doc !! n

-- The recursive syntax includes unrecognised lines, blank lines and malformed
-- fields. Those are input, not error recovery. Semantic checks can refuse a
-- draft without losing a single code point in the original document.
nest :: [Line] -> [Node]
nest [] = []
nest (line:rest) = Node line (nest children) : nest siblings
  where (children,siblings) = span (\r -> blank r || indent r>indent line) rest

arrange :: Yuugao -> Yuugao
arrange doc = doc { docCategories = sections (V.toList (rows doc)) }
  where
    atStart = M.fromList [(eStart entry,entry) | entry<-entries doc]
    boundary line = case lineRole line of Heading level _ -> level<=2; _ -> False
    sections [] = []
    sections input@(line:rest)
      | boundary line = let (body,more)=break boundary rest
                        in Category (Just line) (pieces body):sections more
      | otherwise = let (body,more)=break boundary input
                    in Category Nothing (pieces body):sections more
    pieces [] = []
    pieces input@(line:rest) = case M.lookup (lineNumber line) atStart of
      Just entry -> Item entry:pieces (drop (eEnd entry-eStart entry) input)
      Nothing -> Loose [line]:pieces rest

categories :: Yuugao -> [Category]
categories = docCategories
categoryName :: Category -> Maybe String
categoryName (Category (Just (Line _ _ _ (Heading 2 name))) _) = Just (strip name)
categoryName _ = Nothing
categoryEntries :: Category -> [Entry]
categoryEntries (Category _ pieces) = [entry | Item entry<-pieces]
entryNodes :: Entry -> [Node]
entryNodes = eNodes
nodeText :: Node -> String
nodeText (Node line _) = lineText line
nodeField :: Node -> Maybe (FieldName,String)
nodeField (Node line _) = case lineRole line of
  FieldHead key val _ _ -> Just (key,val)
  BlockHead val -> Just (Corrections,val)
  _ -> Nothing
nodeChildren :: Node -> [Node]
nodeChildren (Node _ children) = children

-- Full document rendering follows the hierarchy, not an untouched copy of
-- the input string. Display-only correction rendering lives in Render.hs.
renderYuugao :: Yuugao -> String
renderYuugao doc = concatMap renderCategory (categories doc)
  where
    renderLine (Line _ raw ending _) = raw ++ case ending of LF -> "\n"; End -> ""
    renderNode (Node line children) = renderLine line ++ concatMap renderNode children
    renderPiece (Loose ls) = concatMap renderLine ls
    renderPiece (Item entry) = renderLine (rows doc V.! eStart entry) ++ concatMap renderNode (entryNodes entry)
    renderCategory (Category heading pieces) = maybe "" renderLine heading ++ concatMap renderPiece pieces

indexLines :: Yuugao -> [String]
indexLines = map lineText . V.toList . rows
indexEntries :: Yuugao -> [Entry]
indexEntries = entries
indexRevisions :: Yuugao -> [Revision]
indexRevisions = revisions
indexById, indexNext :: Yuugao -> M.Map String RevisionKey
indexById = byId
indexNext = next
indexProblems :: Yuugao -> [Problem]
indexProblems = problems
entryNo :: Entry -> EntryKey
entryNo = eNo
entryName :: Entry -> String
entryName = eName
entryStart, entryEnd :: Entry -> Int
entryStart = eStart
entryEnd = eEnd
entryBlock :: Entry -> Maybe (Int,Int)
entryBlock = eBlock
entryRevisions :: Entry -> [RevisionKey]
entryRevisions = eRevisions
revisionNo :: Revision -> RevisionKey
revisionNo = rNo . syntax
revisionId, revisionKindText, revisionEntryName :: Revision -> String
revisionId = rId . syntax
revisionKindText = rKindText . syntax
revisionEntryName = rEntryName . syntax
revisionKind :: Revision -> Maybe Kind
revisionKind (Revision _ body _ _) = case body of
  Understood meaning -> Just (meaningKind meaning)
  Missing kind _ -> Just kind
  Unrecognized _ -> Nothing
revisionEntry :: Revision -> EntryKey
revisionEntry = rEntry . syntax
revisionTraces :: Revision -> [Trace]
revisionTraces = rTraces . syntax
revisionPrev :: Revision -> Maybe String
revisionPrev = rPrev . syntax
revisionStart, revisionEnd :: Revision -> Int
revisionStart = rStart . syntax
revisionEnd = rEnd . syntax
traceName, tracePlace, traceClue :: Trace -> String
traceName (Trace name _ _ _ _ _) = name
tracePlace (Trace _ place _ _ _ _) = place
traceSpeaker :: Trace -> Maybe Speaker
traceSpeaker (Trace _ _ who _ _ _) = who
traceClue (Trace _ _ _ clue _ _) = clue
traceLine :: Trace -> Int
traceLine (Trace _ _ _ _ line _) = line
traceValidName :: Trace -> Bool
traceValidName (Trace _ _ _ _ _ valid) = valid
revisionAt :: Yuugao -> RevisionKey -> Revision
revisionAt doc (RevisionKey n) = revisions doc !! n
entryAt :: Yuugao -> EntryKey -> Entry
entryAt doc (EntryKey n) = entries doc !! n

-- Growth deliberately uses the historical counting projection: only ASCII
-- colons and the first physical field line count; # does not end a count;
-- repeated headings reset the value but retain their first display position.
growthFields :: Yuugao -> [(String,(Int,Int))]
growthFields = snd . foldl' step (Nothing,[]) . V.toList . rows
  where
    step (current,counts) line = case lineRole line of
      Heading 3 raw ->
        let name=strip raw
            reset=if any ((==name).fst) counts
                  then map (\(key,v)->(key,if key==name then (0,0) else v)) counts
                  else counts++[(name,(0,0))]
        in (Just name,reset)
      Heading 2 _ -> (Nothing,counts)
      FieldHead _ val (Just column) _ | Just name<-current,not (null name) ->
        (current,map (\(key,(a,b))->(key,if key/=name then (a,b)
                         else if column==Description then (a+length val,b) else (a,b+length val))) counts)
      _ -> (current,counts)

grown :: Yuugao -> Yuugao -> [String]
grown old new = concatMap growth (growthFields new)
  where
    before=growthFields old
    growth (name,(setsu,koto)) =
      let (setsu0,koto0)=fromMaybe (0,0) (lookup name before)
      in [name++"：説明 "++show setsu0++" → "++show setsu++" 字。説明は一、二文（長くても数文）で、いまの形に書き直す（足さない）。日ごとの経過や細部は、ストロボメモリーと原文に"
         | setsu>setsu0 && setsu>300]
         ++[name++"：ユーザーの言葉 "++show koto0++" → "++show koto++" 字。ここは、好み・約束・頼みのような、これからも効く言葉だけ。その場の話の流れは、ストロボメモリーに"
           | koto-koto0>=200]

-- The expansion renderer uses the already classified exact field spelling.
revisionLines :: Yuugao -> Revision -> [(String,Bool)]
revisionLines doc revision = [(lineText line,current line)
  | line<-V.toList (V.slice (revisionStart revision) (revisionEnd revision-revisionStart revision) (rows doc))]
  where current line = case lineRole line of FieldHead _ _ _ isNow -> isNow; _ -> False
