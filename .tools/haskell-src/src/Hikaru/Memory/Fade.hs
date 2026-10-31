-- | Pure decisions over parsed strobe documents. A plan keeps its original
-- document and eligible titles together; callers cannot forge an allow-list.
module Hikaru.Memory.Fade
  ( Guard(..), FadeError(..), FadePlan, planFade, selectedDays
  , oldMarks, check, report ) where

import Data.List (intercalate, isInfixOf, nub, sort, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (isJust)
import Data.Time (addDays, toGregorian)
import Hikaru.HouseDay (HouseDay, houseDayDate)
import Hikaru.Memory.Text
import Hikaru.Memory.Strobe
import Hikaru.Memory.FadePolicy
import Hikaru.Memory.FadeInstructions (how)

data Guard = Guard { guardClue :: String, guardIds :: [String], guardEntry :: String }
  deriving (Eq, Show)
data FadeError = InvalidDateTitle Title | CutoffBeforeCalendar | GuardWithoutIds Guard
  deriving (Eq, Show)
data FadePlan = FadePlan Strobe [(Step,[(Title,Int)])] deriving (Eq, Show)

older :: HouseDay -> Title -> Either FadeError Bool
older today title = case titlePeriod title of
  Daily (InvalidDay _ _ _) -> Left (InvalidDateTitle title)
  Daily (ValidDay day) ->
    let cutoff=addDays (-ageDays) (houseDayDate today); (year,_,_)=toGregorian cutoff
    in if year<1 then Left CutoffBeforeCalendar else Right (houseDayDate day<cutoff)
  Monthly _ _ -> Right False
  Undated -> Right False

planFade :: HouseDay -> Strobe -> Either FadeError FadePlan
planFade today doc = FadePlan doc . filter (not . null . snd) <$> mapM pick fadingSteps
  where
    pick step = (step,) <$> choose (layerSize doc (fromLayer step)-limit step) (layerDays doc (fromLayer step))
    choose over _ | over<=0 = Right []
    choose _ [] = Right []
    choose over ((title,size):rest) = do
      eligible <- older today title
      if eligible then ((title,size):) <$> choose (over-size) rest else Right []

selectedDays :: FadePlan -> [(Layer,[(Title,Int)])]
selectedDays (FadePlan _ selected) = [(fromLayer step,days) | (step,days)<-selected]
allowedTitles :: [(Step,[(Title,Int)])] -> S.Set String
allowedTitles = S.fromList . concatMap (map (titleText . fst) . snd)

kept :: Entry -> Bool
kept entry = case entryMark entry of Kept _ -> True; Unmarked -> False; LegacyKeep -> False
oldMarks :: Strobe -> [String]
oldMarks doc = [snip 24 (strip (entryText e)) | e<-strobeLines doc,entryMark e==LegacyKeep]
sections :: Strobe -> [(String,String)]
sections doc = [(titleText (sectionTitle s),sectionBody s) | (_,s)<-locatedSections doc]
layered :: Strobe -> [(Maybe Layer,String,String)]
layered doc = [(layer,titleText (sectionTitle s),sectionBody s) | (layer,s)<-locatedSections doc]

hits :: [Guard] -> String -> [Guard]
hits guard line = [g | g<-guard,nfc (guardClue g) `isInfixOf` nfc line,
                      not (any (\rid -> ("〔直 "++rid++"〕") `isInfixOf` line) (guardIds g))]

guarded :: FadePlan -> Strobe -> [Guard] -> Either FadeError [String]
guarded (FadePlan old selected) new guard = do
  (first,told) <- firstPass [] S.empty (map entryText (strobeLines new))
  (second,_) <- secondPass [] told (locatedSections new)
  pure (first++second)
  where
    key = squeeze . nfc
    allowed = allowedTitles selected
    was = S.fromList (map (key . entryText) (strobeLines old))
    before = M.fromListWith (++) [(titleText (sectionTitle s),[(layer,sectionBody s)]) | (layer,s)<-locatedSections old]
    firstPass result told [] = Right (reverse result,told)
    firstPass result told (line:rest)
      | null (key line) || S.member (key line) was = firstPass result told rest
      | otherwise = do
          found <- mapM (message True line) (hits guard line)
          firstPass (reverse found++result) (foldl' (\s g->S.insert (key line,guardClue g) s) told (hits guard line)) rest
    secondPass result told [] = Right (reverse result,told)
    secondPass result told ((layer,section):rest)
      | S.notMember title allowed || (layer,body) `elem` M.findWithDefault [] title before = secondPass result told rest
      | otherwise = do
          (out,next) <- scan result told (sectionEntries section)
          secondPass out next rest
      where title=titleText (sectionTitle section); body=sectionBody section
    scan result told [] = Right (result,told)
    scan result told (entry:rest)
      | null (key line) || kept entry = scan result told rest
      | otherwise = do
          let gs=filter (\g->S.notMember (key line,guardClue g) told) (hits guard line)
          -- Baseline checks told after every item, including repeated clues.
          (out,next) <- addHits result told line gs
          scan out next rest
      where line=rstrip (entryText entry)
    addHits result told _ [] = Right (result,told)
    addHits result told line (g:rest)
      | S.member (key line,guardClue g) told = addHits result told line rest
      | otherwise = do
          value <- message False line g
          addHits (value:result) (S.insert (key line,guardClue g) told) line rest
    message fresh line g = case guardIds g of
      [] -> Left (GuardWithoutIds g)
      ids -> Right ((if fresh then "新しく書いた行に、取り下げた受け取り方がそのまま入っています:「" else "薄める日の行に、取り下げた受け取り方がそのまま残っています:「")
        ++guardClue g++"」（夕顔「"++guardEntry g++"」〔直 "++maximum ids++"〕）。"
        ++(if fresh then "落とすか、番号を添えて、そのときの受け取り方として書く: "
            else "一字も変えずに移す行でも、前の受け取り方は持ち越さない。落とすか、番号を添えて、そのときの受け取り方として書く: ")++snip 40 line)

counts :: Ord a => [a] -> M.Map a Int
counts = M.fromListWith (+) . map (\x->(x,1))
count :: Ord a => a -> M.Map a Int -> Int
count = M.findWithDefault 0

check :: FadePlan -> Strobe -> [Guard] -> Either FadeError [String]
check plan@(FadePlan old selected) new guard = do
  extras <- if null guard then Right [] else guarded plan new guard
  pure (headingProblems++orderProblem++retained++duplicates++bodyProblems++moved++extras)
  where
    allowed = allowedTitles selected
    oldHeads = layerHeadings old
    newHeads = layerHeadings new
    headingProblems = concatMap checkHead layerOrder
    checkHead layer = ["層の見出し「# "++name++"」が消えています" | layer `elem` oldHeads,null found]
                  ++["層の見出し「# "++name++"」が二つ以上あります" | length found>1]
      where found=filter (==layer) newHeads; name=layerName layer
    orderProblem = ["層の順序が崩れています（"++intercalate " → " (map layerName layerOrder)++"、の順）" | nub newHeads/=sort (nub newHeads)]
    os=sections old; ns=sections new
    oc=counts (map fst os); nc=counts (map fst ns)
    newLines=S.fromList [squeeze (entryText e) | (_,s)<-locatedSections new,e<-sectionEntries s]
    retained=[keepMark k++"の行が消えているか、変わっています: "++snip 24 line
             | (_,s)<-locatedSections old,e<-sectionEntries s,Kept k<-[entryMark e],
               let line=rstrip (entryText e),S.notMember (squeeze line) newLines]
    duplicates=["見出し「"++title++"」が二つ以上あります" | (title,k)<-M.toAscList nc,k>1,k>count title oc]
    bodyProblems=snd (foldl' checkBody (counts ns,[]) (map snd (locatedSections old)))
    checkBody (left,probs) section
      | S.member title allowed = (left,probs)
      | count item left>0 = (M.adjust (subtract 1) item left,probs)
      | count title nc==0 = (left,probs++["「"++title++"」が消えています（薄めてよい日ではありません）"])
      | Monthly _ _ <- titlePeriod (sectionTitle section) =
          let have=S.fromList [squeeze (entryText e) | (_,s)<-locatedSections new,titleText (sectionTitle s)==title,e<-sectionEntries s]
              lost=[line | e<-sectionEntries section,let line=rstrip (entryText e),not (null (strip line)),S.notMember (squeeze line) have]
          in (left,probs++["「"++title++"」から行が消えています（月のまとめは足すだけ）: "++snip 24 line | line<-take 1 lost])
      | count title nc<count title oc = (left,probs++["「"++title++"」が "++show (count title oc)++" つあったのに "++show (count title nc)++" つになっています（薄めてよい日ではありません）"])
      | otherwise = (left,probs++["「"++title++"」の本文が変わっています（薄めてよい日ではありません）"])
      where title=titleText (sectionTitle section); body=sectionBody section; item=(title,body)
    ol=layered old; nl=layered new
    was=counts [(l,t) | (l,t,_)<-ol]; now=counts [(l,t) | (l,t,_)<-nl]
    nowOrder=nub [(l,t) | (l,t,_)<-nl]
    sortKey ((layer,title),_)=(layer,title)
    moved = if not (any (\(l,_,_)->isJust l) ol) then [] else
      ["「"++title++"」が別の層に移っています（「"++maybe "層の外" layerName layer++"」→ "++there layer title++"。薄めてよい日ではありません）"
      | ((layer,title),k)<-sortOn sortKey (M.toList was),S.notMember title allowed,count title nc/=0,count (layer,title) now<k]
    there layer title = let labels=["「"++layerName l++"」" | (Just l,t)<-nowOrder,t==title,Just l/=layer]
                        in if null labels then "層の外" else intercalate "、" labels

comma :: Int -> String
comma number = reverse (intercalate "," (parts (reverse (show number))))
  where parts []=[]; parts text=take 3 text:parts (drop 3 text)

report :: FadePlan -> String
report (FadePlan doc selected) =
  intercalate "\n" (headLine:if null selected then ["きょうは、薄めるものはありません。"] else concatMap block selected++["",how])
  where
    note Konogoro="（目安 "++comma (limit RecentToMiddle)++"。一か月以内の日は動かさない）"
    note SukoshiMae="（目安 "++comma (limit MiddleToOld)++"）"
    note Mukashi=""
    headLine="ストロボメモリーの重さ："++intercalate "、" [layerName l++" "++comma (layerSize doc l)++" 字"++note l | l<-reverse layerOrder]++"。"
    block (step,pick) = ("\n「"++layerName (fromLayer step)++"」が重くなっています。古いほうから、次の日を「"++layerName (toLayer step)++"」に移してください（薄めてよいのは、この日だけ）：")
                     : ["  ・"++titleText title++"（"++comma size++" 字）" | (title,size)<-pick]
