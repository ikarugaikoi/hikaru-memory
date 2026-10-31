-- | Unicode Nd digits share one conversion across stored-text grammars.
module Hikaru.Decimal (decimalChar, decimalValue, asciiDecimal) where

import Data.Char (GeneralCategory(DecimalNumber), generalCategory, ord, chr)

decimalChar :: Char -> Bool
decimalChar c = generalCategory c == DecimalNumber

-- Consecutive Nd runs may contain several alphabets of ten digits.
decimalValue :: Char -> Maybe Int
decimalValue c
  | decimalChar c = Just ((ord c - beginning (ord c)) `mod` 10)
  | otherwise = Nothing
  where
    beginning n | n > 0 && decimalChar (chr (n - 1)) = beginning (n - 1)
                | otherwise = n

asciiDecimal :: Char -> Char
asciiDecimal c = maybe c (chr . (ord '0' +)) (decimalValue c)
