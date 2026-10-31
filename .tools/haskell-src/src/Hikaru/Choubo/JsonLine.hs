{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- | Conversation framing is byte based: only LF outside a JSON string ends a
-- record. The strict syntax entry point is also used by our own journals and
-- deliberately does not accept conversation-only raw string controls.
module Hikaru.Choubo.JsonLine
  ( JsonLine(..), classifyJsonLine, jsonLineMessage
  , QuoteState(..), recordBoundary, JsonRecord(..), splitRecords
  , conversationText, classifyRecord
  ) where

import Control.Monad (void)
import Data.Char (digitToInt, ord)
import qualified Data.ByteString as B
import Data.Word (Word8)
import qualified Data.Aeson.Decoding.Text as A
import qualified Data.Aeson.Decoding.Tokens as A
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (tokens)
import Text.Megaparsec.Char (char)

data JsonLine = CompleteJson | IncompleteJson | InvalidJson deriving (Eq,Show)

data QuoteState = Outside | Quoted | Escaped deriving (Eq,Show)

quoteStep :: QuoteState -> Word8 -> QuoteState
quoteStep Escaped _ = Quoted
quoteStep Quoted 92 = Escaped
quoteStep Quoted 34 = Outside
quoteStep Outside 34 = Quoted
quoteStep state _ = state

-- State after this chunk, physical LF count, and the first terminating LF.
-- Callers retain the state across IO chunks, never across record boundaries.
recordBoundary :: QuoteState -> B.ByteString -> (QuoteState, Int, Maybe Int)
recordBoundary initial bytes = go initial 0 0
  where
    size = B.length bytes
    go !state !lines_ !i
      | i == size = (state,lines_,Nothing)
      | otherwise = let byte = B.index bytes i in
          if byte == 10 && state == Outside then (Outside,lines_+1,Just i)
          else go (quoteStep state byte) (if byte == 10 then lines_+1 else lines_) (i+1)

data JsonRecord = JsonRecord
  { recordBytes :: B.ByteString, recordBegin :: Integer, recordEnd :: Integer
  , recordLine :: Integer, recordTerminated :: Bool, recordQuote :: QuoteState
  } deriving (Eq,Show)

splitRecords :: B.ByteString -> [JsonRecord]
splitRecords = go 0 1
  where
    go offset line bytes
      | B.null bytes = []
      | otherwise = case recordBoundary Outside bytes of
          (state,_,Nothing) -> [JsonRecord bytes offset (offset+toInteger (B.length bytes)) line False state]
          (_,lines_,Just i) ->
            let (raw,rest) = B.splitAt (i+1) bytes
                end = offset+toInteger (i+1)
            in JsonRecord raw offset end line True Outside : go end (line+toInteger lines_) rest

-- Escape only literal controls inside strings. A backslash followed by a raw
-- control remains an invalid escape. JSON syntax and decoded contents otherwise
-- stay unchanged; in particular CR/LF inside strings are never normalized.
conversationText :: Text -> Text
conversationText raw
  | not (T.any (< '\x20') (T.dropWhileEnd jsonSpace raw)) = raw
  | otherwise = T.concat (reverse (snd (T.foldl' escape (Outside,[]) raw)))
  where
    escape (state,parts) c =
      let next = if ord c <= 127 then quoteStep state (fromIntegral (ord c)) else state
          rendered | state == Quoted && c < '\x20' =
                       T.pack ['\\','u','0','0',hex (ord c `div` 16),hex (ord c `mod` 16)]
                   | otherwise = T.singleton c
      in (next,rendered:parts)
    hex n = "0123456789abcdef" !! n

classifyRecord :: Bool -> QuoteState -> Text -> JsonLine
classifyRecord terminated state raw
  | not terminated && state /= Outside = IncompleteJson
  | otherwise = classifyJsonLine (conversationText raw)

classifyJsonLine :: Text -> JsonLine
classifyJsonLine raw = case tokens (A.textToTokens raw) of
  Right rest | T.all jsonSpace rest -> CompleteJson
  _ | unfinished raw -> IncompleteJson
    | otherwise -> InvalidJson

-- Validate the token stream without allocating another JSON object tree.
tokens :: A.Tokens k String -> Either String k
tokens (A.TkLit _ rest) = Right rest
tokens (A.TkText _ rest) = Right rest
tokens (A.TkNumber _ rest) = Right rest
tokens (A.TkArrayOpen rest) = array rest
tokens (A.TkRecordOpen rest) = record rest
tokens (A.TkErr reason) = Left reason

array :: A.TkArray k String -> Either String k
array (A.TkItem item) = tokens item >>= array
array (A.TkArrayEnd rest) = Right rest
array (A.TkArrayErr reason) = Left reason

record :: A.TkRecord k String -> Either String k
record (A.TkPair _ item) = tokens item >>= record
record (A.TkRecordEnd rest) = Right rest
record (A.TkRecordErr reason) = Left reason

jsonSpace :: Char -> Bool
jsonSpace c = c `elem` (" \t\r\n" :: String)

-- Only failed syntax reaches this small prefix recognizer. Aeson's EOF error
-- also covers invalid escapes before a missing quote, so its wording alone
-- cannot establish that appending bytes could complete the JSON document.
type PrefixParser = Parsec Void Text

unfinished :: Text -> Bool
unfinished raw = case parse (value <* eof) "" raw of
  Left bundle -> all endedInput (NE.toList (bundleErrors bundle))
  Right () -> False
  where
    endedInput (TrivialError offset (Just EndOfInput) _) = offset == T.length raw
    endedInput _ = False

space :: PrefixParser ()
space = takeWhileP Nothing jsonSpace >> pure ()

value :: PrefixParser ()
value = space *> (lookAhead anySingle >>= start) <* space
  where
    start '{' = between (char '{' *> space) (char '}') (void (pair `sepBy` (char ',' *> space)))
    start '[' = between (char '[' *> space) (char ']') (void (value `sepBy` (char ',' *> space)))
    start '"' = quoted
    start 't' = literal "true"
    start 'f' = literal "false"
    start 'n' = literal "null"
    start _ = number
    pair = quoted *> space *> char ':' *> value
    literal :: String -> PrefixParser ()
    literal = mapM_ char

quoted :: PrefixParser ()
quoted = char '"' *> void (many character) <* char '"'
  where
    character = void (satisfy (\c -> c >= '\x20' && c /= '"' && c /= '\\')) <|> escape
    escape = char '\\' *> (void (oneOf ("\"\\/bfnrt" :: String)) <|> (char 'u' *> unicode))
    hex4 = foldl' (\n c -> n*16+digitToInt c) 0 <$> count 4 (oneOf ("0123456789abcdefABCDEF" :: String))
    unicode = do
      code <- hex4
      if code >= 0xd800 && code <= 0xdbff then do
        _ <- char '\\' *> char 'u'
        low <- hex4
        if low >= 0xdc00 && low <= 0xdfff then pure () else fail "invalid surrogate pair"
      else if code >= 0xdc00 && code <= 0xdfff then fail "unpaired low surrogate" else pure ()

number :: PrefixParser ()
number = do
  _ <- optional (char '-')
  _ <- (void (char '0')) <|> (oneOf ("123456789" :: String) *> void (many digit))
  _ <- optional (char '.' *> some digit)
  _ <- optional (oneOf ("eE" :: String) *> optional (oneOf ("+-" :: String)) *> some digit)
  pure ()
  where digit = oneOf ("0123456789" :: String)

jsonLineMessage :: FilePath -> Integer -> Text
jsonLineMessage path line = "invalid JSONL file: " <> T.pack path <> ": line " <> T.pack (show line) <> ": invalid JSON"
