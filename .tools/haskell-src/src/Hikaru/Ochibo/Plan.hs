{-# LANGUAGE OverloadedStrings #-}
-- | Pure decisions over typed observations. DayWrite is opaque: the only
-- constructors check Archivable rows, format, deduplication and loss protection.
module Hikaru.Ochibo.Plan
  ( Sessions, sessions, sessionPairs, shortSession, renderMessages, dayGroups
  , Batch(..), planBatch, archivedPool
  , DayWrite, writeText, writeCount, AppendDecision(..), planAppend
  , RebuildOrigin, rebuildOrigin, RebuildDecision(..), planRebuild
  ) where

import Control.Monad (foldM)
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Hikaru.Ochibo.Archivable
import Hikaru.Ochibo.Format
import Hikaru.Ochibo.Read
import Hikaru.Conversation.Line
import Hikaru.HouseDay (HouseDay)

newtype Sessions = Sessions [(Text,Text)] deriving (Eq, Show)
sessions :: [(Text,Text)] -> Sessions
sessions = Sessions
sessionPairs :: Sessions -> [(Text,Text)]
sessionPairs (Sessions pairs) = pairs
shortSession :: Maybe Text -> Sessions -> (Text,Sessions)
shortSession Nothing table = ("----",table)
shortSession (Just "") table = ("----",table)
shortSession (Just full) table@(Sessions pairs) = case find ((==full) . snd) pairs of
  Just (name,_) -> (name,table)
  Nothing ->
    let choose n | T.take n full `elem` map fst pairs && n < T.length full = choose (n+1)
                 | otherwise = T.take n full
        name = choose 3
    in (name,Sessions (filter ((/=name) . fst) pairs ++ [(name,full)]))

dayGroups :: [Archivable] -> Either Text [(HouseDay,[Archivable])]
dayGroups rows = M.toAscList . fmap reverse <$> foldM add M.empty rows
  where add groups row = do
          day <- messageDay (archivedMessage row)
          pure (M.insertWith (++) day [row] groups)

renderMessages :: Sessions -> [Archivable] -> Either Text (Text,Sessions)
renderMessages table rows = do
  (_,pieces,next) <- foldM step (Nothing,[],table) rows
  pure (T.dropWhileEnd (=='\n') (T.concat (reverse pieces)) <> "\n",next)
  where
    step (previous,pieces,current) row = do
      let message = archivedMessage row
          (name,next) = shortSession (utteranceSession message) current
      day <- messageDay message
      let headed = if previous == Just day then pieces else header day:pieces
      pure (Just day,entry name message:headed,next)

data Batch = Batch
  { batchVisible :: [Archivable]
  , batchNewest :: Maybe (UTCTime,Text)
  }
planBatch :: Closure -> Maybe UTCTime -> UTCTime -> Batch
planBatch closure after now =
  let fresh = saveable closure after now
      latest best row = let m = collectedMessage row; t = utteranceTime m
        in case best of Just (old,_) | old >= t -> best; _ -> Just (t,utteranceRawTime m)
  in Batch (archivable fresh) (foldl' latest Nothing fresh)
archivedPool :: Closure -> Maybe UTCTime -> UTCTime -> [Archivable]
archivedPool closure done now = archivable (archived closure done now)

data DayWrite = DayWrite Text Int
writeText :: DayWrite -> Text
writeText (DayWrite text _) = text
writeCount :: DayWrite -> Int
writeCount (DayWrite _ n) = n

data AppendDecision = OldFormat | Unchanged | Append DayWrite
planAppend :: HouseDay -> Text -> Sessions -> [Archivable] -> (AppendDecision,Sessions)
planAppend day old table rows
  | oldFormat old day = (OldFormat,table)
  | otherwise =
      let keys = entriesIn old
          select (pieces,n,current,remaining) row =
            let message=archivedMessage row
                (name,next)=shortSession (utteranceSession message) current
            in case consumeEntry remaining name message of
              Just rest -> (pieces,n,next,rest)
              Nothing -> (entry name message:pieces,n+1,next,remaining)
          (rendered,count,finalTable,_) = foldl' select ([],0,table,keys) rows
      in (if count==0 then Unchanged else Append (DayWrite (appendPrefix day old <> T.concat (reverse rendered)) count),finalTable)

-- Keep the pre-operation session mapping separate from new alias allocations
-- made while rebuilding earlier days of the same invocation.
newtype RebuildOrigin = RebuildOrigin Sessions
rebuildOrigin :: Sessions -> RebuildOrigin
rebuildOrigin = RebuildOrigin

data RebuildDecision = WouldLose [(Text,Text)] | Replace DayWrite [(Text,Text)]
planRebuild :: Bool -> HouseDay -> Text -> RebuildOrigin -> Sessions -> [Archivable] -> (RebuildDecision,Sessions)
planRebuild force day old (RebuildOrigin original) table rows =
  -- Check exactly the rows this plan will write, never the cross-day pool.
  let lost = lostOnRebuild (sessionPairs original) old (poolKeys (map archivedMessage rows))
      step (pieces,current) row =
        let message=archivedMessage row; (name,next)=shortSession (utteranceSession message) current
        in (entry name message:pieces,next)
      (rendered,finalTable) = foldl' step ([],table) rows
  in if not force && not (null lost) then (WouldLose lost,table)
     else (Replace (DayWrite (header day <> T.concat (reverse rendered)) (length rows)) lost,finalTable)
