{-# LANGUAGE OverloadedStrings #-}
module Hikaru.Oshirase.NoticePending
  ( ToolUseId, toolUseId, Pending, emptyPending, pendingNull, pendingInsert
  , pendingTake, pendingEntries, pendingRawHit ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

newtype ToolUseId = ToolUseId Text deriving (Eq, Ord, Show)
newtype Pending a = Pending (Map.Map ToolUseId a) deriving (Eq, Show)

toolUseId :: Text -> Maybe ToolUseId
toolUseId text | T.null text = Nothing
               | otherwise = Just (ToolUseId text)

emptyPending :: Pending a
emptyPending = Pending Map.empty

pendingNull :: Pending a -> Bool
pendingNull (Pending entries) = Map.null entries

pendingInsert :: ToolUseId -> a -> Pending a -> Pending a
pendingInsert ident value (Pending entries) = Pending (Map.insertWith (\_ old -> old) ident value entries)

pendingTake :: ToolUseId -> Pending a -> (Maybe a, Pending a)
pendingTake ident (Pending entries) =
  let (found, rest) = Map.updateLookupWithKey (\_ _ -> Nothing) ident entries
  in (found, Pending rest)

pendingEntries :: Pending a -> [(ToolUseId, a)]
pendingEntries (Pending entries) = Map.toList entries

pendingRawHit :: Text -> Pending a -> Bool
pendingRawHit line = any (\(ToolUseId ident, _) -> ident `T.isInfixOf` line) . pendingEntries
