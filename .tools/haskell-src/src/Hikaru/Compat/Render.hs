{-# LANGUAGE OverloadedStrings #-}
-- | Python str/repr and json.dumps formatting for the JSON value domain.
-- Keep code points until the explicit strict-UTF-8 output boundary.
module Hikaru.Compat.Render (pyStr, pyRepr, dumpJson) where

import Data.Char (ord)
import Data.List (intercalate, sortOn)
import qualified Data.Text as T
import qualified Data.Vector as V
import Hikaru.Compat.Json
import Hikaru.Compat.Number (pyFloatRepr)
import Hikaru.Compat.Unicode (pyIsPrintable)
import Numeric (showHex)

hex :: Int -> Int -> String
hex width n = let chars = showHex n "" in replicate (max 0 (width - length chars)) '0' ++ chars

unicodeEscape :: Char -> String
unicodeEscape c
  | ord c <= 0xffff = "\\u" ++ hex 4 (ord c)
  | otherwise = let n = ord c - 0x10000 in "\\u" ++ hex 4 (0xd800 + n `div` 1024)
                                         ++ "\\u" ++ hex 4 (0xdc00 + n `mod` 1024)

jsonString :: Bool -> PyString -> String
jsonString ascii value = '"' : concatMap escape (pyStringToChars value) ++ "\""
  where
    escape c = case c of
      '"' -> "\\\""; '\\' -> "\\\\"; '\b' -> "\\b"; '\f' -> "\\f"
      '\n' -> "\\n"; '\r' -> "\\r"; '\t' -> "\\t"
      _ | ord c < 32 || (ascii && ord c > 127) -> unicodeEscape c
        | ascii && ord c == 127 -> "\\u007f"
        | otherwise -> [c]

number :: Bool -> PyNumber -> String
number _ (PyInteger n) = show n
number json PyNaN = if json then "NaN" else "nan"
number json (PyFloat value)
  | json && isNaN value = "NaN"
  | json && isInfinite value = if value < 0 then "-Infinity" else "Infinity"
  | otherwise = T.unpack (pyFloatRepr value)

pyStr :: Value -> PyString
pyStr (String text) = text
pyStr value = pyRepr value

pyRepr :: Value -> PyString
pyRepr = pyStringFromChars . representation
  where
    representation Null = "None"
    representation (Bool value) = if value then "True" else "False"
    representation (Number value) = number False value
    representation (String value) = reprString value
    representation (Array values) = "[" ++ intercalate ", " (map representation (V.toList values)) ++ "]"
    representation (Object values) = "{" ++ intercalate ", "
      [reprString key ++ ": " ++ representation value | (key,value) <- objectPairs values] ++ "}"
    reprString value = quote : concatMap escape chars ++ [quote]
      where
        chars = pyStringToChars value
        quote = if '\'' `elem` chars && not ('"' `elem` chars) then '"' else '\''
        escape c
          | c == quote || c == '\\' = ['\\', c]
          | c == '\n' = "\\n"
          | c == '\r' = "\\r"
          | c == '\t' = "\\t"
          | pyIsPrintable c = [c]
          | ord c < 256 = "\\x" ++ hex 2 (ord c)
          | ord c <= 0xffff = "\\u" ++ hex 4 (ord c)
          | otherwise = "\\U" ++ hex 8 (ord c)

-- Arguments correspond to ensure_ascii, indent and sort_keys. Python's
-- negative indentation has the same layout as zero indentation.
dumpJson :: Bool -> Maybe Int -> Bool -> Value -> PyString
dumpJson ascii indentation sorted = pyStringFromChars . render 0
  where
    indent = fmap (max 0) indentation
    pad depth = replicate (depth * maybe 0 id indent) ' '
    render _ Null = "null"
    render _ (Bool value) = if value then "true" else "false"
    render _ (Number value) = number True value
    render _ (String value) = jsonString ascii value
    render depth (Array values) = enclosure depth '[' ']' (map (render (depth+1)) (V.toList values))
    render depth (Object values) = enclosure depth '{' '}'
      [jsonString ascii key ++ ": " ++ render (depth+1) value | (key,value) <- pairs]
      where pairs = (if sorted then sortOn fst else id) (objectPairs values)
    enclosure _ open close [] = [open,close]
    enclosure depth open close items = case indent of
      Nothing -> open : intercalate ", " items ++ [close]
      Just _ -> [open,'\n'] ++ pad (depth+1) ++ intercalate (",\n" ++ pad (depth+1)) items
                ++ "\n" ++ pad depth ++ [close]
