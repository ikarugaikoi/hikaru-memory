{-# LANGUAGE OverloadedStrings #-}
-- | Lossless syntax for shiori. Unrecognised prose and malformed marks remain
-- source lines; recognising a terminal mark never requires a valid date.
module Hikaru.Shiori
  ( Shiori, Section, Block(..), Item, ItemLine(..), Mark(..), MarkNote(..)
  , parseShiori, renderShiori, sections, heading, blocks, itemLines
  , terminalMark, openItems, parseMark, decimalValue
  , MarkReason(..), MarkConcern(..), markConcerns
  ) where

import Control.Applicative ((<|>), optional)
import Data.List (mapAccumL, nub)
import Data.Maybe (listToMaybe, mapMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Time (fromGregorianValid)
import Hikaru.HouseDay (HouseDay, houseDayNamed)
import Hikaru.Decimal (decimalValue)
import qualified Hikaru.RecordText as Record
import Text.Megaparsec (Parsec, ParseErrorBundle, parse, sepBy1, takeWhileP, takeRest, eof, try, choice, count, satisfy, anySingle, manyTill, some, token)
import Text.Megaparsec.Char (char, string)

type Parser = Parsec Void Text

newtype Shiori = Shiori [Section] deriving (Eq, Show)
data Section = Section (Maybe Text) [Block] deriving (Eq, Show)
data Block = Prose Text | Entry Item deriving (Eq, Show)
data Item = Item ItemLine [ItemLine] deriving (Eq, Show)
data ItemLine = ItemLine Text (Maybe Mark) deriving (Eq, Show)
data Mark = Done MarkNote | Inserted MarkNote | OnHill MarkNote | Declined MarkNote
  deriving (Eq, Show)
data MarkNote = Bare | Dated HouseDay Text | Undated Text deriving (Eq, Show)

sections :: Shiori -> [Section]
sections (Shiori xs) = xs
heading :: Section -> Maybe Text
heading (Section title _) = title
blocks :: Section -> [Block]
blocks (Section _ xs) = xs
itemLines :: Item -> [ItemLine]
itemLines (Item first rest) = first:rest

-- The parser is total over Text: every character other than LF belongs to a
-- source line, including CR, unusual whitespace and incomplete Markdown.
parseShiori :: Text -> Either (ParseErrorBundle Text Void) Shiori
parseShiori = fmap (Shiori . groupSections) . parse sourceLines "shiori"
  where
    sourceLines :: Parser [Text]
    sourceLines = takeWhileP Nothing (/= '\n') `sepBy1` char '\n' <* eof
    groupSections rows = case break isHeading rows of
      (before, []) -> [Section Nothing (groupItems before)]
      (before, rest) -> [Section Nothing (groupItems before) | not (null before)] ++ headed rest
    headed [] = []
    headed (title:rest) = let (body, remaining) = break isHeading rest
                        in Section (Just title) (groupItems body) : headed remaining
    isHeading = T.isPrefixOf "## "
    groupItems [] = []
    groupItems (row:rest)
      | "- " `T.isPrefixOf` row =
          let (more, remaining) = span continuation rest
              typed line = ItemLine line (parseMark line)
          in Entry (Item (typed row) (map typed more)) : groupItems remaining
      | otherwise = Prose row : groupItems rest
    continuation line = maybe False ((`elem` [' ', '\t', '　']) . fst) (T.uncons line)
                        && not (T.null (Record.strip line))

renderShiori :: Shiori -> Text
renderShiori = T.intercalate "\n" . concatMap sectionLines . sections
  where
    sectionLines (Section title xs) = maybe [] (:[]) title ++ concatMap blockLines xs

blockLines :: Block -> [Text]
blockLines (Prose text) = [text]
blockLines (Entry item) = [text | ItemLine text _ <- itemLines item]

-- Notes are a display context, not stored shiori syntax. With no annotation
-- index even a source line starting with ⤷ is ordinary item body, as before.
terminalMark :: Bool -> Item -> Maybe Mark
terminalMark annotated (Item (ItemLine _ firstMark) rest) = foldl next firstMark rest
  where
    next previous (ItemLine text mark)
      | annotated && "⤷" `T.isPrefixOf` Record.stripStart text = previous
      | otherwise = mark

openItems :: Bool -> Shiori -> (Text, Int)
openItems annotated document =
  let (total, kept) = mapAccumL keepSection 0 (sections document)
  in (renderShiori (Shiori kept), total)
  where
    keepSection n (Section title xs) =
      let (count_, selected) = mapAccumL keep n xs
      in (count_, Section title (concat selected))
    keep n block@(Entry item) | Just _ <- terminalMark annotated item = (n + 1, [])
                             | otherwise = (n, [block])
    keep n block = (n, [block])

parseMark :: Text -> Maybe Mark
parseMark text = listToMaybe (mapMaybe atArrow (suffixes text))
  where
    -- Retain arrows inside the parenthesised explanation. Try each suffix,
    -- since a malformed earlier mark must not hide a later terminal mark.
    suffixes input = case T.breakOn "→" input of
      (_, rest) | T.null rest -> []
                | otherwise -> let after = T.drop 1 rest in after : suffixes after
    atArrow input = either (const Nothing) Just (parse marker "shiori mark" input)

marker :: Parser Mark
marker = do
  _ <- takeWhileP Nothing Record.space
  kind <- choice [Done <$ string "済", Inserted <$ string "入れた",
                  OnHill <$ string "丘の上", Declined <$ string "入れない"]
  note <- (char '（' *> (T.pack <$> manyTill anySingle (try (char '）' <* trailer))) >>= pure . noteOf)
          <|> (Bare <$ trailer)
  pure (kind note)
  where
    trailer = optional (char '。' <|> char '.') *> takeWhileP Nothing Record.space *> eof

noteOf :: Text -> MarkNote
noteOf raw = either (const (Undated raw)) id (parse dated "shiori date" raw)
  where
    dated :: Parser MarkNote
    dated = do
      y <- number 4
      _ <- char '-'
      m <- number 2
      _ <- char '-'
      d <- number 2
      day <- if y < 1 then fail "invalid date" else maybe (fail "invalid date") pure (fromGregorianValid (toInteger y) m d)
      rest <- takeRest
      pure (Dated (houseDayNamed day) rest)
    number n = foldl (\a d -> a * 10 + d) 0 <$> count n (token decimalValue mempty)

-- Concerns are observations about syntax, never content permits or refusals.
data MarkReason = HalfwidthParen | MissingCloseParen | TextAfterMark | ImpossibleMarkDate
  deriving (Eq, Show)
data MarkConcern = MarkConcern Int MarkReason Text deriving (Eq, Show)

markConcerns :: Shiori -> [MarkConcern]
markConcerns document = concat (snd (mapAccumL section 1 (sections document)))
  where
    section line (Section title body) =
      let (next, found) = mapAccumL block (line + maybe 0 (const 1) title) body
      in (next, concat found)
    block line (Prose _) = (line + 1, [])
    block line (Entry item) =
      let rows = itemLines item
          -- Recognition protects shape in both display modes. Calendar
          -- validity is independent, including inside an accepted mark.
          accepted = isJust (terminalMark False item) || isJust (terminalMark True item)
          concerns = [MarkConcern n reason (T.take 40 text)
                     | (n, ItemLine text mark) <- zip [line..] rows
                     , reason <- case mark of
                         Just known -> [ImpossibleMarkDate | impossibleMarkDate known]
                         Nothing -> filter (\r -> not accepted || r == ImpossibleMarkDate)
                                      (suspiciousMarks text)]
      in (line + length rows, concerns)

-- Use the note already selected by parseMark. Rescanning an accepted line
-- would mistake arrows/dates in its prose or explanation for another mark.
impossibleMarkDate :: Mark -> Bool
impossibleMarkDate mark = case note of
  Bare -> False
  Dated _ _ -> False
  Undated raw -> impossibleNoteDate raw
  where
    note = case mark of
      Done n -> n
      Inserted n -> n
      OnHill n -> n
      Declined n -> n

suspiciousMarks :: Text -> [MarkReason]
suspiciousMarks = nub . concatMap inspect . arrows
  where
    arrows text = case T.breakOn "→" text of
      (_, rest) | T.null rest -> []
                | otherwise -> let after = T.drop 1 rest in after : arrows after
    inspect text = case parse candidate "shiori concern" text of
      Left _ -> []
      Right (Nothing, body) -> [TextAfterMark | not (T.null (Record.strip body))]
      Right (Just opener, body) ->
        let (note, end) = T.break (`elem` ['）', ')']) body
            after = T.drop 1 end
            trailer = Record.strip (maybe after id (T.stripPrefix "。" after <|> T.stripPrefix "." after))
        in [HalfwidthParen | opener == '(' || ")" `T.isPrefixOf` end] ++
           [MissingCloseParen | T.null end] ++
           [TextAfterMark | not (T.null end), not (T.null trailer)] ++
           [ImpossibleMarkDate | impossibleNoteDate note]
    candidate :: Parser (Maybe Char, Text)
    candidate = do
      _ <- takeWhileP Nothing Record.space
      _ <- choice (map string ["済", "入れた", "丘の上", "入れない"])
      (do opener <- char '（' <|> char '('
          (Just opener,) <$> takeRest)
        <|> (do first <- satisfy (\c -> Record.space c || c == '。' || c == '.')
                rest <- takeRest
                pure (Nothing, T.cons first rest))

-- Only an explicit date at the start of a note is inspected. Without a year,
-- February 29 remains possible; no clock-dependent inference is made.
impossibleNoteDate :: Text -> Bool
impossibleNoteDate text = case parse date "shiori note date" (Record.stripStart text) of
  Left _ -> False
  Right (year, month, day) -> year < 1 || not (isJust (fromGregorianValid year month day))
  where
    date :: Parser (Integer, Int, Int)
    date = try iso <|> short
    iso = do
      year <- number 4
      _ <- char '-'
      month <- number 2
      _ <- char '-'
      day <- number 2
      pure (toInteger year, month, day)
    short = do
      month <- small
      separator <- char '/' <|> char '月'
      day <- small
      if separator == '月' then char '日' *> pure () else pure ()
      pure (2000, month, day)
    number n = foldl (\a d -> a * 10 + d) 0 <$> count n digit
    small = do
      digits <- some digit
      if length digits <= 2 then pure (foldl (\a d -> a * 10 + d) 0 digits)
                            else fail "one or two digits"
    digit = token decimalValue mempty
