module Hikaru.Memory.Correction.Summary
  ( dot, source, retired, label, nowSummary, oneLine ) where
import Data.List (isSuffixOf)
import Hikaru.Memory.Text (strip)
import Hikaru.Memory.Correction.Types

dot :: String -> String
dot text=let s=strip text in s++if null s || "。" `isSuffixOf` s then "" else "。"
source :: Revision -> (String,String)
source = revisionSource
retired :: Revision -> Either CorrectionError String
retired r
  | revisionKind r==Just Misread=Right("取り下げた受け取り方："++dot(value Before r))
  | revisionKind r==Just Changed=do
      (_,shown,_)<-since r
      pure((if null shown then "前：" else shown++" より前：")++dot(value Before r))
  | otherwise=Right(revisionKindText r++"。")
label :: Revision -> String
label r=let(who,day)=source r in "〔直 "++revisionId r++"〕"++revisionKindText r++"（"++(if null who then "" else who++"、")++day++"）"
nowSummary :: Revision -> String
nowSummary r
  | revisionKind r `elem` map Just retireKinds=if null(value Now r) then "" else "いま："++dot(value Now r)
  | revisionKind r==Just Depends="ふだん："++dot(value Usually r)++"このとき："++dot(value ThisTime r)
  | otherwise=""
oneLine :: Revision -> [Revision] -> Either CorrectionError String
oneLine r ch=do
  old<-shown r
  previous<-mapM (\p->do text<-shown p;pure("（その前に"++label p++"。"++text++"）")) (reverse(dropLast ch))
  pure(label r++"。"++old++(if revisionKind r==Just Changed then "そのあとは、上のとおり。" else "")
       ++(if null(value Scope r) then "" else "範囲："++dot(value Scope r))++concat previous++" → fuutou misekechi "++revisionId r)
  where dropLast []=[];dropLast xs=take(length xs-1)xs
        -- 開場の一行には、読み違いの前の受け取り方を出さない（2026-10-14）。
        -- いまの理解は項目の本文にある。前の受け取り方は fuutou misekechi 番号で読める。
        shown x | revisionKind x==Just Misread=Right "" | otherwise=retired x
