{-# LANGUAGE OverloadedStrings #-}
-- | Native values at the wake input boundary. The ISO grammar covers the
-- timestamps written by conversation logs and the progress ledger.
module Hikaru.Mezame.Input (InputError(..), parseStamp, parseDay) where

import Control.Applicative ((<|>), optional)
import Control.Exception (Exception(..))
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Void (Void)
import Hikaru.Conversation.DateTime (DateTime(..), DateZone(..))
import Text.Megaparsec (Parsec, parse, eof, count, satisfy, takeWhile1P, errorBundlePretty)
import Text.Megaparsec.Char (char)

newtype InputError = InputError Text deriving Show
instance Exception InputError where
  displayException (InputError text) = T.unpack text

type Parser = Parsec Void Text

parseDay :: Text -> Either Text Day
parseDay = parseInput dayParser

parseStamp :: Text -> Either Text DateTime
parseStamp = parseInput $ do
  day <- dayParser
  clock <- optional $ do
    _ <- char 'T' <|> char ' '
    hour <- number 2
    _ <- char ':'
    minute <- number 2
    _ <- char ':'
    second <- number 2
    fraction <- optional (char '.' *> takeWhile1P (Just "fraction") asciiDigit)
    let micros = maybe 0 (digits . T.justifyLeft 6 '0' . T.take 6) fraction
    tod <- maybe (fail "clock value out of range") pure
      (makeTimeOfDayValid hour minute (fromIntegral second + fromIntegral micros / 1000000))
    when (second > 59) (fail "clock value out of range")
    zone <- optional (FixedOffset 0 <$ char 'Z' <|> offsetParser)
    pure (tod, maybe NaiveZone id zone)
  pure $ case clock of
    Nothing -> DateTime (LocalTime day midnight) NaiveZone
    Just (tod, zone) -> DateTime (LocalTime day tod) zone

parseInput :: Parser a -> Text -> Either Text a
parseInput parser = either (Left . T.pack . errorBundlePretty) Right . parse (parser <* eof) "wake input"

dayParser :: Parser Day
dayParser = do
  year <- number 4
  _ <- char '-'
  month <- number 2
  _ <- char '-'
  day <- number 2
  when (year < 1) (fail "year out of range")
  maybe (fail "day out of range") pure (fromGregorianValid (toInteger year) month day)

offsetParser :: Parser DateZone
offsetParser = do
  sign <- (1 <$ char '+') <|> (-1 <$ char '-')
  hour <- number 2
  _ <- char ':'
  minute <- number 2
  when (hour > 23 || minute > 59) (fail "timezone offset out of range")
  pure (FixedOffset (sign * toInteger (hour * 60 + minute) * 60000000))

asciiDigit :: Char -> Bool
asciiDigit c = c >= '0' && c <= '9'
number :: Int -> Parser Int
number size = foldl (\a c -> a * 10 + fromEnum c - fromEnum '0') 0 <$> count size (satisfy asciiDigit)
digits :: Text -> Int
digits = T.foldl' (\a c -> a * 10 + fromEnum c - fromEnum '0') 0
