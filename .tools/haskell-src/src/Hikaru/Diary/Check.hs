{-# LANGUAGE OverloadedStrings #-}
-- | Post-commit observations. Existing recognition protects heading shape;
-- calendar validity and membership of the target week are checked separately.
module Hikaru.Diary.Check (DiaryReason(..), DiaryConcern(..), diaryConcerns) where

import Control.Applicative ((<|>), optional, some)
import Data.List (minimumBy, mapAccumL)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, diffDays, fromGregorianValid, toGregorian)
import Data.Void (Void)
import Hikaru.Diary.Syntax
import Hikaru.HouseDay (houseDayDate)
import qualified Hikaru.RecordText as Record
import Hikaru.Shiori (decimalValue)
import Text.Megaparsec (Parsec, parse, takeWhileP, count, try, token)
import Text.Megaparsec.Char (char)

type Parser = Parsec Void Text

data DiaryReason = HeadingShape | ImpossibleDiaryDate | OutsideWeek deriving (Eq, Show)
data DiaryConcern = DiaryConcern Int DiaryReason Text deriving (Eq, Show)
data HeadingCandidate = HeadingCandidate (Maybe Integer) MonthDay

diaryConcerns :: DiaryWeek -> Diary -> [DiaryConcern]
diaryConcerns week document = concat (snd (mapAccumL checkSection 1 (sections document)))
  where
    monday = houseDayDate (weekStart week)
    checkSection first section =
      let rows = sectionLines section
          skip = maybe 0 (const 1) (sectionHeading section)
          known = [DiaryConcern first reason (T.take 40 (headingText h))
                  | Just h <- [sectionHeading section]
                  , reason <- dateReasons (HeadingCandidate Nothing (headingDate h))]
          suspected = [DiaryConcern line reason (T.take 40 row)
                      | (line, row) <- drop skip (zip [first..] rows)
                      , Right candidate <- [parse headingCandidate "diary concern" row]
                      , reason <- HeadingShape : dateReasons candidate]
      in (first + length rows, known ++ suspected)
    dateReasons candidate = case candidateDay monday candidate of
      Nothing -> [ImpossibleDiaryDate]
      Just day -> [OutsideWeek | let delta = diffDays day monday, delta < 0 || delta > 6]

-- Require a Markdown heading and a date at its start. Ordinary dated prose,
-- quoted examples, weekly titles, and the diary's field labels are untouched.
headingCandidate :: Parser HeadingCandidate
headingCandidate = do
  _ <- takeWhileP Nothing Record.space
  _ <- some (char '#' <|> char '＃')
  _ <- takeWhileP Nothing Record.space
  try iso <|> try japaneseYear <|> short
  where
    iso = do
      year <- number 4
      separator <- char '-' <|> char '/'
      month <- small
      _ <- char separator
      day <- small
      pure (HeadingCandidate (Just (toInteger year)) (MonthDay month day))
    japaneseYear = do
      year <- number 4
      _ <- char '年'
      MonthDay month day <- monthDay
      pure (HeadingCandidate (Just (toInteger year)) (MonthDay month day))
    short = HeadingCandidate Nothing <$> monthDay
    monthDay = do
      month <- small
      _ <- takeWhileP Nothing Record.space
      separator <- char '月' <|> char '/' <|> char '-'
      _ <- takeWhileP Nothing Record.space
      day <- small
      _ <- if separator == '月' then optional (char '日') else pure Nothing
      pure (MonthDay month day)
    number n = foldl (\a d -> a * 10 + d) 0 <$> count n digit
    small = do
      digits <- some digit
      if length digits <= 2 then pure (foldl (\a d -> a * 10 + d) 0 digits)
                            else fail "one or two digits"
    digit = token decimalValue mempty

candidateDay :: Day -> HeadingCandidate -> Maybe Day
candidateDay monday (HeadingCandidate explicit (MonthDay month day)) =
  case [date | year <- years, year >= 1, year <= 9999,
               Just date <- [fromGregorianValid year month day]] of
    [] -> Nothing
    dates -> Just (minimumBy (comparing (abs . (`diffDays` monday))) dates)
  where
    (y,_,_) = toGregorian monday
    -- Match OneLine's nearest date across the previous/current/next year,
    -- including December/January weeks and its leap-day treatment.
    years = maybe [y-1,y,y+1] (:[]) explicit
