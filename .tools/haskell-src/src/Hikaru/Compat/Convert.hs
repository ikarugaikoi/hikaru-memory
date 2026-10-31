{-# LANGUAGE OverloadedStrings #-}
-- | Python integer conversion on the JSON value domain, including Unicode
-- decimal digits, underscore grammar, integer length limits and signed zero.
module Hikaru.Compat.Convert (pyInt) where

import Data.Char (chr, ord)
import Data.List (dropWhileEnd)
import Data.Text (Text)
import Hikaru.Compat.Json
import Hikaru.Compat.Text (pySpace)
import Hikaru.Compat.Unicode (pyDecimalValue)

numericSpace :: Char -> Bool
numericSpace c = pySpace c && (ord c >= 128 || c `elem` (" \t\n\r\v\f" :: String))

digit :: Char -> Bool
digit c = c >= '0' && c <= '9'

normalized :: PyString -> Either Text String
normalized text = underscores Nothing (map decimal stripped)
  where
    stripped = dropWhileEnd numericSpace (dropWhile numericSpace (pyStringToChars text))
    decimal c = maybe c (chr . (ord '0' +)) (pyDecimalValue c)
    underscores _ [] = Right []
    underscores before ('_':rest@(after:_))
      | maybe False digit before && digit after = underscores before rest
    underscores _ ('_':_) = Left "ValueError"
    underscores _ (c:rest) = (c:) <$> underscores (Just c) rest

signed :: String -> (Bool, String)
signed ('-':rest) = (True, rest)
signed ('+':rest) = (False, rest)
signed text = (False, text)

pyInt :: Value -> Either Text Integer
pyInt (Bool b) = Right (if b then 1 else 0)
pyInt (Number (PyInteger n)) = Right n
pyInt (Number PyNaN) = Left "ValueError"
pyInt (Number (PyFloat n))
  | isNaN n = Left "ValueError"
  | isInfinite n = Left "OverflowError"
  | otherwise = Right (truncate n)
pyInt (String text) = do
  (negative, body) <- signed <$> normalized text
  if null body || not (all digit body) || length body > 4300 then Left "ValueError" else do
    let n = foldl' (\a c -> a*10 + toInteger (ord c-ord '0')) 0 body
    pure (if negative then negate n else n)
pyInt _ = Left "TypeError"
