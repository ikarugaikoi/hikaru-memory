module Hikaru.Memory.Correction.Check (checkIndex, check, whereProblem) where
import Data.List (intercalate, isPrefixOf, nub, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Hikaru.Memory.Text (snip)
import Hikaru.Memory.Correction.Types
import Hikaru.Memory.Correction.Annotate (walk,hits)

whereProblem :: Yuugao -> Problem -> String
whereProblem ix p="項目「"++entryName(entryAt ix(problemEntry p))++"」"++rid++"（夕顔の "++show(problemLine p+1)++" 行目）："++problemMessage p
  where rid=maybe "" (\r->if null r then "" else "〔直 "++r++"〕")(problemId p)

checkIndex :: Yuugao -> Maybe (M.Map String String) -> Either CorrectionError ([Problem],S.Set EntryKey)
checkIndex ix records=do
  basics<-concat <$> mapM basic(indexRevisions ix)
  checkedRecords<-case records of Nothing->pure [];Just contents->concat <$> mapM (recordCheck (M.mapWithKey walk contents)) (indexRevisions ix)
  let previous=[p | r<-indexRevisions ix,Just p<-[revisionPrev r],not(null p),M.member p(indexById ix)]
      forks=[mk(revisionAt ix(indexById ix M.! p)) Nothing ("この直しを「前の直し」にしているものが "++show count++" つあります（ひとつながりにする）")False
            |p<-nub previous,let count=length(filter(==p)previous),count>1]
      probs=sortOn problemLine(indexProblems ix++basics++forks++checkedRecords)
  pure(probs,S.fromList[problemEntry p|p<-probs,problemStop p])
  where
    mk r line msg stop=Problem(fromMaybe(revisionStart r)line)(revisionEntry r)(Just(revisionId r))msg stop
    basic r=do
      sinceProblems<-if revisionKind r/=Just Changed then Right [] else do
        (day,_,_)<-since r
        pure[mk r Nothing "「いつから」に日付（YYYY-MM-DD。日がはっきりしなければ YYYY-MM まで）がありません（その前か後かで、注の言い方が変わる）" False | day==Nothing]
      let need=maybe [] needed(revisionKind r); missing=revisionMissing r
          required=[mk r Nothing ("「"++intercalate "」「" (map fieldName missing)++"」がありません（"++revisionKindText r++"には "++intercalate "・" (map fieldName need)++" が要る）")True | not(null missing)]
          origin=value Evidence r
          evidence | null origin=[mk r Nothing "「よりどころ」がありません" True]
                   | not(any (`isPrefixOf` origin) sources)=[mk r Nothing ("「よりどころ」の頭に、出どころを書きます（"++intercalate "／" sources++"）: "++snip 40 origin)False]
                   | otherwise=[]
          scope=[mk r Nothing "「範囲」がありません（どこまでのことか。「なぜ」にいまの使い方を縛ることがあれば、同じことを範囲にも）" False | revisionKind r==Just Misread,null(value Scope r)]
      pure(required++evidence++scope++sinceProblems++previousCheck r++concatMap(traceCheck r)(revisionTraces r))
    previousCheck r=case revisionPrev r of
      Just p | not(null p)->
        if p==revisionId r then [mk r Nothing "「前の直し」が自分を指しています" True]
        else case M.lookup p(indexById ix) of
          Nothing->[mk r Nothing ("「前の直し」の番号がありません: 〔直 "++p++"〕")True]
          Just index->let prior=revisionAt ix index in
            if revisionEntry prior/=revisionEntry r then [mk r Nothing ("「前の直し」〔直 "++p++"〕は別の項目（「"++revisionEntryName prior++"」）にあります（同じ項目の中でつなぐ）")True]
            else [mk r Nothing "「前の直し」が輪になっています" True | cycleFound(S.singleton(revisionId r))prior]
      _->[]
    cycleFound seen r
      | S.member(revisionId r)seen=True
      | otherwise=case revisionPrev r >>= (\p->if null p then Nothing else M.lookup p(indexById ix)) of
          Nothing->False
          Just n->cycleFound(S.insert(revisionId r)seen)(revisionAt ix n)
    traceCheck r tr
      | not(traceValidName tr)=[mk r (Just(traceLine tr)) ("残っているところの名前が違います: 「"++traceName tr++"」（strobe / shiori / ichifure:日 / ariake:日 / tm:日）")False]
      | null(traceClue tr)=[mk r (Just(traceLine tr)) "残っているところの手がかりが空です" False]
      | Nothing<-traceSpeaker tr,not("tm:" `isPrefixOf` traceName tr)=[mk r (Just(traceLine tr)) ("残っているところに「だれ」がありません（ふ／わたし）: "++traceName tr++"「"++snip 20(traceClue tr)++"」")False]
      | otherwise=[]
    recordCheck contents r=concat <$> mapM test(revisionTraces r)
      where
        test tr
          | not(traceValidName tr) || null(traceClue tr)=pure []
          | otherwise=let nm=normName(traceName tr);line=Just(traceLine tr) in case M.lookup nm contents of
            Nothing->pure [mk r line ("残っているところの記録が読めません: "++traceName tr)False]
            Just parsed->do
              frames<-parsed
              let (got,why)=hits tr frames nm
                  tag=traceName tr++"「"++snip 20(traceClue tr)++"」"
              pure $ if null got then [mk r line("残っているところが見つかりません（"++why++"）: "++tag++"。薄めたりして、もうその言い方が無いなら、その一件は消してよい")False]
                 else [mk r line("残っているところが "++show(length got)++" か所に当たります。位置を足して、一か所に: "++tag)False | length got>1]

check :: Yuugao -> Maybe (M.Map String String) -> Either CorrectionError ([String],[String])
check ix records=do
  (ps,_)<-checkIndex ix records
  pure([whereProblem ix p|p<-ps,problemStop p],[whereProblem ix p|p<-ps,not(problemStop p)])
