{-# LANGUAGE OverloadedStrings #-}
-- | Display-only selectors. The shiori grammar lives in Hikaru.Shiori.
module Hikaru.Mezame.Items (withoutForm, withoutMoved) where

import Data.Text (Text)
import qualified Hikaru.RecordText as Record
import qualified Data.Text as T
import qualified Data.Set as S
import Hikaru.Decimal (decimalValue)

withoutForm :: Text -> Text
withoutForm text = case break (T.isPrefixOf "項目の形（") rows of
  (_, []) -> text
  (before, first:rest) -> case break ((== "---") . Record.strip) rest of
    (_, []) -> text
    (middle, after)
      | any (T.isPrefixOf "## ") (first:middle) -> text
      | otherwise -> T.intercalate "\n" (before ++ note ++ after)
  where
    rows = T.splitOn "\n" text
    note = ["（項目の書き方の説明は、ここには出していない。夕顔に書くときは、`.tools/bin/fuutou open yuugao` で開いて、冒頭から読む。",
            "説明の末尾に（くわしくは 日付）とある項目は、ここにあるのが全部ではない。細部を話す前に、原文を見る）", ""]

diaryDay :: Text -> Maybe (Int, Int)
diaryDay line = do
  rest <- T.stripPrefix "## " line
  let (month, afterMonth) = T.span isDecimal rest
  after <- T.stripPrefix "月" afterMonth
  let (day, end) = T.span isDecimal after
  _ <- T.stripPrefix "日" end
  if valid month && valid day then (,) <$> digits month <*> digits day else Nothing
  where
    isDecimal = maybe False (const True) . decimalValue
    valid t = not (T.null t) && T.length t <= 2
    digits = fmap (foldl (\a n -> a * 10 + n) 0) . traverse decimalValue . T.unpack

withoutMoved :: S.Set (Int, Int) -> Text -> (Text, Int)
withoutMoved moved text =
  let (out, _, _, days) = foldl step ([], Nothing, False, S.empty) (T.splitOn "\n" text)
  in (T.intercalate "\n" (reverse out), S.size days)
  where
    step (out, prior, skipping, days) line =
      let (day, skip) = case diaryDay line of
            Just d -> (Just d, False)
            Nothing | "#" `T.isPrefixOf` line -> (Nothing, False)
                    | otherwise -> (prior, skipping)
      in case day of
        Just d | S.member d moved && any (`T.isPrefixOf` line) ["- 問いたいこと", "- 候補"] ->
          (out, day, True, S.insert d days)
        _ | skip && indented line && not (T.null (Record.strip line)) -> (out, day, skip, days)
          | otherwise -> (line:out, day, False, days)
    indented = maybe False ((`elem` [' ', '\t', '　']) . fst) . T.uncons
