-- | Counts with explicit first-occurrence order. Hashing only serves lookup;
-- the ordinary sequence number makes visible ties independent of hash order.
module Hikaru.Shirushi.RepeatTable (Table, empty, alter, toList, toListWhere) where

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M
import Data.List (sortOn)

data Table k v = Table !Int !(M.HashMap k (Int, v)) deriving (Eq, Show)

empty :: Table k v
empty = Table 0 M.empty

alter :: (Eq k, Hashable k) => k -> (Maybe v -> v) -> Table k v -> Table k v
alter key update (Table next entries) = case M.lookup key entries of
  Nothing -> Table (next + 1) (M.insert key (next, update Nothing) entries)
  Just (position, value) -> Table next (M.insert key (position, update (Just value)) entries)

toList :: Table k v -> [(k, v)]
toList = toListWhere (\_ _ -> True)

-- Eligibility does not depend on insertion order. Only survivors need their
-- first-occurrence order restored; ties still use the original sequence.
toListWhere :: (k -> v -> Bool) -> Table k v -> [(k, v)]
toListWhere eligible (Table _ entries) =
  [(key, value) | (key, (_, value)) <- sortOn (fst . snd)
    (filter (\(key, (_, value)) -> eligible key value) (M.toList entries))]
