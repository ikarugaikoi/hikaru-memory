module Hikaru.Memory.Correction.Render (fold, withoutBlocks, listing, expand) where
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Hikaru.Memory.Text (strip)
import Hikaru.Memory.Correction.Types
import Hikaru.Memory.Correction.Parse
import Hikaru.Memory.Correction.Summary
import Hikaru.Memory.Correction.Check (checkIndex)

slice :: Int -> Int -> [a] -> [a]
slice start end=take(max 0(end-start)).drop start
past :: [a] -> [a]
past xs=reverse(take(max 0(length xs-1))xs)

fold :: Yuugao -> Either CorrectionError (String,String,[String])
fold ix=do
  (_,bad)<-checkIndex ix Nothing
  (out,pos,total,folded,kept)<-foldl(step bad)(Right([],0,0::Int,0::Int,[]))(indexEntries ix)
  let tailText=if total==0 then "" else "（みせけち："++show total++" 件。うち "++show folded++" 件は一行に畳んである。全部は .tools/bin/fuutou misekechi、一つは .tools/bin/fuutou misekechi 番号）"
  pure(intercalate "\n" (out++drop pos ls),tailText,kept)
  where
    ls=indexLines ix
    step bad previous ent=do
      (out,pos,total,folded,kept)<-previous
      case entryBlock ent of
        Nothing->Right(out,pos,total,folded,kept)
        Just(b,end)->do
          let rs=map(revisionAt ix)(entryRevisions ent);total'=total+length rs
          if S.member(entryNo ent)bad || null rs then Right(out,pos,total',folded,kept++[entryName ent]) else do
            (lines_,count)<-foldl perRevision (Right([],folded))rs
            pure(out++slice pos(b+1)ls++lines_,end,total',count,kept)
    perRevision before r=do
      (lines_,count)<-before
      if M.member(revisionId r)(indexNext ix) then Right(lines_,count+1) else
        if revisionKind r `elem` map Just retireKinds then do
          line<-oneLine r(chain ix r);pure(lines_++["  - "++line],count+1)
        else do
          previous<-mapM(\p->do old<-retired p;pure("    - （その前に"++label p++"。"++old++"）"))(past(chain ix r))
          pure(lines_++slice(revisionStart r)(revisionEnd r)ls++previous,count)

withoutBlocks :: Yuugao -> String
withoutBlocks ix=
  let ls=indexLines ix
      step(done,cursor)ent=case entryBlock ent of Nothing->(done,cursor);Just(b,end)->(done++slice cursor b ls,end)
      (out,pos)=foldl step([],0)(indexEntries ix)
  in intercalate "\n" (out++drop pos ls)

listing :: Yuugao -> Either CorrectionError String
listing ix
  | null(indexRevisions ix)=Right "みせけちは、まだありません。"
  | otherwise=do
      lines_<-concat <$> mapM renderEntry entries
      pure(intercalate "\n" (intro++lines_))
  where
    entries=filter(not.null.entryRevisions)(indexEntries ix)
    intro=["みせけち："++show(length(indexRevisions ix))++" 件（"++show(length entries)++" 項目）。一つを全部見るには .tools/bin/fuutou misekechi 番号。",
           "読み違い＝前の受け取り方が違っていた／変わった＝そのときはそのとおりで、ある日から変わった／食い違い＝記録どうしが合わず、まだ決めていない／場合による＝条件が違えば、別々に成り立つ。"]
    renderEntry ent=do
      rs<-concat <$> mapM renderRevision[revisionAt ix n|n<-entryRevisions ent,let r=revisionAt ix n,not(M.member(revisionId r)(indexNext ix))]
      pure(["","### "++entryName ent]++rs)
    renderRevision r=do
      details<-if revisionKind r `elem` map Just retireKinds then do t<-retired r;pure["  - "++t]
               else if revisionKind r==Just Conflict then Right["  - 一方："++dot(value OneSide r),"  - もう一方："++dot(value OtherSide r),"  - まだ決めていない。"] else Right[]
      previous<-mapM(\p->do old<-retired p;pure("  - （その前に"++label p++"。"++old++"）"))(past(chain ix r))
      pure(["- "++label r]++details++["  - "++nowSummary r|not(null(nowSummary r))]
           ++["  - 範囲："++dot(value Scope r)|not(null(value Scope r))]++previous)

expand :: Yuugao -> String -> Maybe String
expand ix input=do
  n<-M.lookup normalized(indexById ix)
  let r=revisionAt ix n;ch=chain ix r
      lead=["### "++revisionEntryName r,""]++if length ch>1 then ["（同じことを "++show(length ch)++" 回直している。古い順。いま効いているのは、最後の〔直 "++revisionId(last ch)++"〕）",""] else []
      render k p=[line++if k<length ch-1 && currentField then "　←（そのあと直した：〔直 "++revisionId(ch!!(k+1))++"〕）" else "" | (line,currentField)<-revisionLines ix p]
  pure(intercalate "\n" (lead++concat[render k p|(k,p)<-zip[0..]ch]))
  where
    normalized=replace "直 " "" (trimBrackets(strip input))
    trimBrackets=reverse.dropWhile(`elem` "〔〕").reverse.dropWhile(`elem` "〔〕")
    replace _ _ []=[]
    replace old new rest | old `isPrefixOf` rest=new++replace old new(drop(length old)rest)
                         | otherwise=take 1 rest++replace old new(drop 1 rest)
