{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Fixed _hk.pages/show_page behavior on code-point strings. No I/O.
-- This pure layer is independently verified; complete CLI integration is separate.
module Hikaru.Store.Paging (pages, showPage) where

import Data.Char (ord)
import Control.Monad (foldM)
import Data.List (intercalate)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import Hikaru.Compat.Json (PyString, pyStringFromChars, pyStringToChars, pyStringFromText, pyStringToText)

splitLF :: String -> [String]
splitLF text = case break (=='\n') text of
  (line, []) -> [line]
  (line, _:rest) -> line : splitLF rest

utf8Width :: Char -> Either Text Int
utf8Width c
  | code >= 0xd800 && code <= 0xdfff = Left "UnicodeEncodeError"
  | code <= 0x7f = Right 1
  | code <= 0x7ff = Right 2
  | code <= 0xffff = Right 3
  | otherwise = Right 4
  where code = ord c

pages :: Int -> PyString -> Either Text [PyString]
pages limit text
  -- Scalar Text cannot contain surrogates. Keep the original path for
  -- nonprogress limits as well as code-point strings so error order and
  -- legacy looping behavior remain unchanged.
  | limit >= 4, Right scalar <- pyStringToText text =
      Right (map pyStringFromText (scalarPages limit scalar))
  | otherwise = map pyStringFromChars <$> codePointPages limit (pyStringToChars text)

scalarPages :: Int -> Text -> [Text]
scalarPages limit text =
  let (out,current,_) = foldl' step ([],[],0) (T.splitOn "\n" text)
      finished = if null current then out else T.intercalate "\n" (reverse current):out
  in if null finished then [""] else reverse finished
  where
    byteLength = BS.length . TE.encodeUtf8
    step state line = addLine state line (byteLength line+1)
    addLine (out,current,!size) line !bytes
      | bytes > limit =
          let (piece,rest) = T.splitAt (limit `div` 4) line
              flushed = if null current then out else T.intercalate "\n" (reverse current):out
          in addLine (piece:flushed,[],0) rest (bytes-byteLength piece)
      | size+bytes > limit && not (null current) =
          (T.intercalate "\n" (reverse current):out,[line],bytes)
      | otherwise = (out,line:current,size+bytes)

codePointPages :: Int -> String -> Either Text [String]
codePointPages limit text = do
  (out, current, _) <- foldM step ([], [], 0) (splitLF text)
  let finished = if null current then out else intercalate "\n" (reverse current) : out
  pure (if null finished then [""] else reverse finished)
  where
    step state line = do
      widths <- traverse utf8Width line
      addLine state line widths (sum widths+1)
    addLine (out, current, size) line widths bytes
      | bytes > limit =
          let rawCut = limit `div` 4
              -- Python negative [:cut]/[cut:] uses length-relative bounds.
              cut = if rawCut < 0 then max 0 (length line+rawCut) else rawCut
              (piece, rest) = splitAt cut line
              (used, remaining) = splitAt cut widths
              flushed = if null current then out else intercalate "\n" (reverse current):out
          in addLine (piece:flushed, [], 0) rest remaining (bytes-sum used)
      | size+bytes > limit && not (null current) = Right (intercalate "\n" (reverse current):out, [line], bytes)
      | otherwise = Right (out, line:current, size+bytes)

-- As in the baseline, a nonpositive/sufficiently small custom limit can fail
-- to make progress. Production uses the unchanged 16000-byte default; tests
-- must isolate legacy nontermination instead of weakening that input's oracle.
showPage :: PyString -> Integer -> Either Text PyString
showPage text index = do
  chunks <- pages 16000 text
  let total = length chunks
      body
        | index < 1 || index > fromIntegral total =
            "ページは 1〜" ++ show total ++ " です。\n"
        | otherwise = pyStringToChars (chunks !! (fromInteger index-1)) ++ "\n" ++ footer
      footer
        | total <= 1 = ""
        | otherwise = "\n［ページ " ++ show index ++ "/" ++ show total ++ "。" ++
            (if index < fromIntegral total then "続きは --page " ++ show (index+1) else "これで最後") ++ "］\n"
  pure (pyStringFromChars body)
