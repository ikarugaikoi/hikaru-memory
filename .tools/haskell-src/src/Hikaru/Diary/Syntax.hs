{-# LANGUAGE OverloadedStrings #-}
-- | Lossless diary syntax. Recognition describes the two existing readers;
-- it does not make their permissive month/day prefixes into calendar dates.
module Hikaru.Diary.Syntax
  ( Diary, Section, Heading, Recognition(..), MonthDay(..)
  , parseDiary, renderDiary, sections, sectionLines, sectionHeading
  , readHeading, headingText, headingDate, recognition
  , DiaryWeek, weekOf, weekStart, weekFromPath
  ) where

import Control.Applicative (some)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addDays, fromGregorianValid)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Void (Void)
import Hikaru.HouseDay (HouseDay, houseDayDate, houseDayNamed)
import qualified Hikaru.RecordText as Record
import Hikaru.Shiori (decimalValue)
import System.FilePath (takeFileName)
import Text.Megaparsec (Parsec, ParseErrorBundle, parse, sepBy1, takeWhileP, eof, count, token)
import Text.Megaparsec.Char (char, string)

type Parser = Parsec Void Text

data Diary = Diary [Section] deriving (Eq, Show)
data Section = Section (Maybe Heading) [Text] deriving (Eq, Show)
data Heading = Heading Text MonthDay Recognition deriving (Eq, Show)
data MonthDay = MonthDay Int Int deriving (Eq, Show)
data Recognition = BothReaders | OneLineOnly deriving (Eq, Show)

sections :: Diary -> [Section]
sections (Diary xs) = xs
sectionHeading :: Section -> Maybe Heading
sectionHeading (Section h _) = h
sectionLines :: Section -> [Text]
sectionLines (Section h body) = maybe [] ((:[]) . headingText) h ++ body
headingText :: Heading -> Text
headingText (Heading text _ _) = text
headingDate :: Heading -> MonthDay
headingDate (Heading _ date _) = date
recognition :: Heading -> Recognition
recognition (Heading _ _ readers) = readers

parseDiary :: Text -> Either (ParseErrorBundle Text Void) Diary
parseDiary = fmap (Diary . group) . parse sourceLines "diary"
  where
    sourceLines :: Parser [Text]
    sourceLines = takeWhileP Nothing (/= '\n') `sepBy1` char '\n' <* eof
    group rows = case break isHeading rows of
      (before, []) -> [Section Nothing before]
      (before, rest) -> [Section Nothing before | not (null before)] ++ headed rest
    headed [] = []
    headed (row:rest) = let (body, remaining) = break isHeading rest
                        in Section (readHeading row) body : headed remaining
    isHeading = maybe False (const True) . readHeading

renderDiary :: Diary -> Text
renderDiary = T.intercalate "\n" . concatMap sectionLines . sections

-- OneLine.header: ##, any existing record whitespace, 1-2 Unicode Nd digits,
-- 月, 1-2 Nd digits, 日; everything after 日 is accepted. Wake.diaryDay uses
-- exactly "## " instead. Its language is a subset, recorded explicitly here.
readHeading :: Text -> Maybe Heading
readHeading text = case parse header "diary heading" text of
  Left _ -> Nothing
  Right (spaces, md) -> Just (Heading text md (if spaces == " " then BothReaders else OneLineOnly))
  where
    header :: Parser (Text, MonthDay)
    header = do
      _ <- string "##"
      spaces <- takeWhileP Nothing Record.space
      month <- shortNumber
      _ <- char '月'
      day <- shortNumber
      _ <- char '日'
      pure (spaces, MonthDay month day)
    shortNumber = do
      digits <- some decimal
      if length digits <= 2 then pure (foldl (\n d -> n * 10 + d) 0 digits)
                            else fail "one or two decimal digits"

decimal :: Parser Int
decimal = token decimalValue mempty

data DiaryWeek = DiaryWeek HouseDay deriving (Eq, Show)
weekStart :: DiaryWeek -> HouseDay
weekStart (DiaryWeek day) = day
weekOf :: HouseDay -> DiaryWeek
weekOf day = let (_,_,weekday) = toWeekDate (houseDayDate day)
             in DiaryWeek (houseDayNamed (addDays (fromIntegral (1-weekday)) (houseDayDate day)))

-- Like OneLine.filenameDate, use the leading named date, not today's clock.
-- Unknown filenames have no inferred week; they keep their existing behavior.
weekFromPath :: FilePath -> Maybe DiaryWeek
weekFromPath = either (const Nothing) Just . parse named "diary filename" . T.pack . takeFileName
  where
    named :: Parser DiaryWeek
    named = do
      year <- number 4
      _ <- char '-'
      month <- number 2
      _ <- char '-'
      day <- number 2
      date <- if year < 1 then fail "invalid year"
              else maybe (fail "invalid date") pure (fromGregorianValid (toInteger year) month day)
      pure (weekOf (houseDayNamed date))
    number n = foldl (\a d -> a * 10 + d) 0 <$> count n decimal
