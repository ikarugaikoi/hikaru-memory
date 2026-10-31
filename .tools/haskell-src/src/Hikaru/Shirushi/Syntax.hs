{-# LANGUAGE OverloadedStrings #-}
-- | Small grammars shared by the day-file and language parsers.
module Hikaru.Shirushi.Syntax
  ( Parser, decimal, decimalChar, spaces, scan, scanFrom, match, namedDay, isoLocal, stamp, whiteSpace, strip ) where

import Data.Char (isSpace, isSeparator)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Void (Void)
import Hikaru.Decimal (decimalChar, decimalValue)
import Text.Megaparsec hiding (match)
import Text.Megaparsec.Char (char)

type Parser = Parsec Void Text

decimal :: Int -> Int -> Parser Int
decimal least most = foldl' (\n d -> n * 10 + d) 0 <$> count' least most (token decimalValue mempty)

spaces :: Parser ()
spaces = () <$ takeWhileP (Just "whitespace") whiteSpace

-- Unicode White_Space includes the line/paragraph separators and NEL.
-- They separate words but never split a stored LF-delimited line.
whiteSpace :: Char -> Bool
whiteSpace c = isSpace c || isSeparator c || c == '\x85'

strip :: Text -> Text
strip = T.dropAround whiteSpace

match :: Parser a -> Text -> Maybe a
match parser = parseMaybe (parser <* eof)

-- Leftmost, non-overlapping matches. The parser owns each pattern; a failed
-- pattern leaves that character in the body, so inner parentheses still match.
scan :: Parser a -> Text -> (Text, [a])
scan = scanFrom (const True)

-- 始まりえない文字の間は Text のまま通す。候補で失敗したら一文字だけ
-- 進むので、外の括弧が閉じなくても内側の括弧を拾う順番は変わらない。
scanFrom :: (Char -> Bool) -> Parser a -> Text -> (Text, [a])
scanFrom starts parser = go [] []
  where
    go body hits input =
      let (plain, candidate) = T.break starts input
          preceding = if T.null plain then body else plain:body
      in case parseMaybe ((,) <$> try parser <*> takeRest) candidate of
        Just (hit, rest) -> go preceding (hit:hits) rest
        Nothing -> case T.uncons candidate of
          Nothing -> (T.concat (reverse preceding), reverse hits)
          Just (c, rest) -> go (T.singleton c:preceding) hits rest

namedDay :: Parser Day
namedDay = do
  year <- decimal 4 4 <* char '-'
  month <- decimal 2 2 <* char '-'
  day <- decimal 2 2
  maybe (fail "invalid calendar date") pure
    (if year == 0 then Nothing else fromGregorianValid (toInteger year) month day)

isoLocal :: Text -> Maybe LocalTime
isoLocal = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q+09:00" . T.unpack

stamp :: LocalTime -> Text
stamp local = T.pack (formatTime defaultTimeLocale ("%04Y-%m-%dT%H:%M:%S" ++ fraction) local) <> "+09:00"
  where
    seconds = todSec (localTimeOfDay local)
    fraction = if seconds == fromInteger (floor seconds) then "" else "%6Q"
