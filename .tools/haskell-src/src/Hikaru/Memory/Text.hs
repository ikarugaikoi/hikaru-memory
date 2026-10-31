-- | Code-point operations for memory formats. String retains isolated
-- surrogates from synthetic JSON, unlike conversion through scalar Text.
module Hikaru.Memory.Text
  ( space, strip, rstrip, splitLF, splitWords, snip, squeeze, nfc, decimal
  ) where

import Data.List (dropWhileEnd)
import Data.Maybe (mapMaybe)
import Hikaru.Compat.Json (pyStringFromChars, pyStringToChars)
import Hikaru.Compat.Normalize (normalizeNFC)
import Hikaru.RecordText (space)
import Hikaru.Compat.Unicode (pyDecimalValue)

strip, rstrip, squeeze, nfc :: String -> String
strip = dropWhile space . rstrip
rstrip = dropWhileEnd space
squeeze = filter (not . space)
nfc = pyStringToChars . normalizeNFC . pyStringFromChars

splitLF :: String -> [String]
splitLF value = case break (=='\n') value of
  (one, []) -> [one]
  (one, _:rest) -> one : splitLF rest

splitWords :: String -> [String]
splitWords text = case dropWhile space text of
  [] -> []
  rest -> let (one,more) = break space rest in one : splitWords more

snip :: Int -> String -> String
snip limit text = let value = unwords (splitWords text)
                  in take limit value ++ if length value > limit then "…" else ""

decimal :: Int -> Int -> String -> Maybe (Int,String)
decimal low high input = do
  let digits = take high input
      found = takeWhile (maybe False (const True) . pyDecimalValue) digits
      size = length found
  if size < low then Nothing else
    Just (foldl (\a b -> 10*a+b) 0 (mapMaybe pyDecimalValue found), drop size input)

