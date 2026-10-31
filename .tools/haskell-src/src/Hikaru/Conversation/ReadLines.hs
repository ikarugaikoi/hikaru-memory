{-# LANGUAGE OverloadedStrings #-}
-- | TextIOWrapper-style 8192-byte UTF-8 read-ahead and universal newlines.
-- A task prefix can stop iteration before EOF, but decoding failures in the
-- already-read chunk still precede its first returned line.
module Hikaru.Conversation.ReadLines (foldLinesUntil, readTextUniversal, splitLines) where
import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Hikaru.Conversation.AsciiNewline (breakLF,normalizeNewlines)
import Hikaru.Compat.Diagnostic (decodeUtf8Chunk,decodeUtf8Diagnostic,raiseDiagnostic)
import System.IO (IOMode(ReadMode), withBinaryFile)

-- Left is an ordinary early return; Right continues.
foldLinesUntil :: FilePath -> (s -> Text -> IO (Either s s)) -> s -> IO s
foldLinesUntil path step initial = withBinaryFile path ReadMode $ \handle ->
  let consume state parts text next =
        let (front,rest) = breakLF text in
        if T.null rest then next state (if T.null front then parts else front:parts)
        else do
          result <- step state (T.concat (reverse ((front<>"\n"):parts)))
          case result of
            Left done -> pure done
            Right state' -> consume state' [] (T.drop 1 rest) next
      more state parts undecoded heldCR = do
        bytes <- BS.hGetSome handle 8192
        if BS.null bytes then do
          if BS.null undecoded then pure () else either raiseDiagnostic (const (pure ())) (decodeUtf8Diagnostic undecoded)
          consume state parts (if heldCR then "\n" else "") $ \s pending ->
            if null pending then pure s else either id id <$> step s (T.concat (reverse pending))
        else do
          -- Preserve CPython's incremental error timing, including a trailing
          -- ED A0..BF pair: it remains pending until the next byte or EOF.
          -- Strictly validate the entire read chunk before returning any line.
          (decoded,remaining) <- either raiseDiagnostic pure (decodeUtf8Chunk False (undecoded<>bytes))
          _ <- evaluate (T.length decoded)
          let joined = (if heldCR then "\r" else "") <> decoded
              endsCR = not (T.null joined) && T.last joined == '\r'
              body = if endsCR then T.init joined else joined
              normalized = normalizeNewlines body
          consume state parts normalized $ \s pending -> more s pending remaining endsCR
  in more initial [] BS.empty False

readTextUniversal :: FilePath -> IO Text
readTextUniversal path = T.concat . reverse <$> foldLinesUntil path (\parts line -> pure (Right (line:parts))) []

-- Python str.splitlines(), distinct from file line iteration and split("\n").
splitLines :: Text -> [Text]
splitLines text | T.null text = []
splitLines text =
  let (first,rest) = T.break (`elem` ("\n\r\v\f\x1c\x1d\x1e\x85\x2028\x2029" :: String)) text
  in first : if T.null rest then [] else
       splitLines (if T.isPrefixOf "\r\n" rest then T.drop 2 rest else T.drop 1 rest)
