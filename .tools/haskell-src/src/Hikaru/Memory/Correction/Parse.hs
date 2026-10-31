-- | Entry point and traversal of the typed correction graph. Duplicate IDs,
-- missing links and cycles remain representable drafts, diagnosed by Check.
module Hikaru.Memory.Correction.Parse
  ( parse, reference, markdownHeading, ymdSearch, ymSearch, dayOf, since
  , normName, tip, chain, namesUsed ) where
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Hikaru.Memory.Yuugao

parse :: String -> Either CorrectionError Yuugao
parse = parseYuugao

tip :: Yuugao -> Revision -> Revision
tip ix start=go(S.singleton(revisionId start))start
  where go seen r=case M.lookup(revisionId r)(indexNext ix) of
          Nothing->r
          Just n->let next=revisionAt ix n in if S.member(revisionId next)seen then r else go(S.insert(revisionId next)seen)next
chain :: Yuugao -> Revision -> [Revision]
chain ix r=let newest=tip ix r in go(S.singleton(revisionId newest))[newest]
  where go _ []=[]
        go seen current@(first:_)=case revisionPrev first >>= (`M.lookup` indexById ix) of
          Nothing->current
          Just n->let previous=revisionAt ix n in if S.member(revisionId previous)seen then current else go(S.insert(revisionId previous)seen)(previous:current)
namesUsed :: Yuugao -> [String]
namesUsed doc=S.toAscList(S.fromList[traceName tr|r<-indexRevisions doc,tr<-revisionTraces r,traceValidName tr])
