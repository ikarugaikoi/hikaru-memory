{-# LANGUAGE OverloadedStrings #-}
module Hikaru.Diary.OneLine (parseDiary, readOneLines) where

import Control.Monad (foldM)
import Data.List (minimumBy, sort)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, diffDays, fromGregorianValid, toGregorian)
import Hikaru.RecordText (space, strip)
import Hikaru.Compat.Unicode (pyIsDecimal, pyDecimalValue)
import Hikaru.Compat.Diagnostic
import Hikaru.Compat.Json (pyStringFromText)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import System.IO.Error (catchIOError, isDoesNotExistError)

decimal :: Text -> Maybe Integer
decimal = fmap (foldl' (\n d -> n*10 + fromIntegral d) 0) . traverse pyDecimalValue . T.unpack

takeDigits :: Int -> Text -> Maybe (Integer, Text)
takeDigits size text = let (prefix, rest) = T.splitAt size text
                      in if T.length prefix == size && T.all pyIsDecimal prefix
                         then (,rest) <$> decimal prefix else Nothing

filenameDate :: Text -> Maybe (Either Text Day)
filenameDate name = do
  (year, a) <- takeDigits 4 name
  b <- T.stripPrefix "-" a
  (month, c) <- takeDigits 2 b
  d <- T.stripPrefix "-" c
  (day, _) <- takeDigits 2 d
  pure (makeDay year month day)

makeDay :: Integer -> Integer -> Integer -> Either Text Day
makeDay year month day
  | year < 1 || year > 9999 = Left "year must be in 1..9999"
  | month < 1 || month > 12 = Left "month must be in 1..12"
  | otherwise = maybe (Left "day is out of range for month") Right (fromGregorianValid year (fromInteger month) (fromInteger day))

header :: Text -> Maybe (Integer, Integer)
header line = do
  rest <- T.dropWhile space <$> T.stripPrefix "##" line
  let month = T.takeWhile pyIsDecimal (T.take 2 rest)
  if T.null month then Nothing else pure ()
  b <- T.stripPrefix "月" (T.drop (T.length month) rest)
  let (day, _) = T.span pyIsDecimal (T.take 2 b)
  if T.null day then Nothing else pure ()
  _ <- T.stripPrefix "日" (T.drop (T.length day) b)
  (,) <$> decimal month <*> decimal day

oneLine :: Text -> Maybe Text
oneLine line = do
  let start = T.dropWhile space line
      rest = case T.uncons start of Just (c, tail_) | c == '-' || c == '・' -> tail_; _ -> start
  tail_ <- T.stripPrefix "一行で" (T.dropWhile space rest)
  after <- case T.uncons tail_ of Just (c, t) | c == ':' || c == '：' -> Just t; _ -> Nothing
  -- Regex \s*(.+) backtracks when the entire suffix is whitespace: it
  -- still appends an empty stripped one-liner if at least one char exists.
  if T.null after then Nothing else Just (strip after)

parseDiary :: Day -> Text -> M.Map Day [Text]
parseDiary mon text = snd (foldl' step (Nothing, M.empty) (T.splitOn "\n" text))
  where
    (year, _, _) = toGregorian mon
    step (_, out) line | Just (month, day) <- header line =
      let candidates = [d | y <- [year-1,year,year+1], Right d <- [makeDay y month day]]
          current = if null candidates then Nothing else Just (minimumBy (comparing (abs . (`diffDays` mon))) candidates)
      in (current, out)
    step state@(current, out) line = case (current, oneLine line) of
      (Just day, Just summary_) -> (current, M.insertWith (flip (++)) day [summary_] out)
      _ -> state

readOneLines :: FilePath -> IO (M.Map Day [Text])
readOneLines root = do
  names <- sort . filter ((== ".b64") . takeExtension) <$> (listDirectory root `catchIOError` \err ->
    if isDoesNotExistError err then pure [] else ioError err)
  foldM readFile_ M.empty names
  where
    readFile_ out name = case filenameDate (T.pack name) of
      Nothing -> pure out
      Just (Left reason) -> raiseDiagnostic (Diagnostic "ValueError"
        (pyStringFromText ("invalid diary filename date: " <> T.pack (root </> name) <> ": " <> reason)))
      Just (Right mon) -> do
        content <- readSealedDiagnostic (root </> name)
        pure (M.unionWith (++) out (parseDiary mon content))
