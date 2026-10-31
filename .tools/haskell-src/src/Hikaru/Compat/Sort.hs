-- | Stable ordering supplied by the standard library.
module Hikaru.Compat.Sort (pySortByLT) where

import Data.List (sortBy)

ordering :: (a -> a -> Bool) -> a -> a -> Ordering
ordering less a b
  | less a b = LT
  | less b a = GT
  | otherwise = EQ

pySortByLT :: (a -> a -> Bool) -> [a] -> [a]
pySortByLT less = sortBy (ordering less)
