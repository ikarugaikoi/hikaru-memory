-- | Lossless strobe syntax. Only this module constructs documents, sections
-- and entries: their spelling and their meaning cannot be updated separately.
-- Unlabelled prose, repeated headings and invalid date-shaped titles are part
-- of the input language, not errors to repair or silently discard.
module Hikaru.Memory.Strobe
  ( Strobe, Section, Entry, Title, Layer(..), Keep(..), Mark(..)
  , DateValue(..), Period(..), HeadingDay(..)
  , parseStrobe, renderStrobe, strobeLines, layerOrder, layerName, keepName
  , layerHeadings, locatedSections, layerSize, layerDays
  , sectionTitle, titleText, titlePeriod, sectionEntries, sectionBody
  , entryText, entryMark, entryHeading, entryHeadingDay, datedEntries
  ) where

import Control.Applicative (optional, (<|>))
import Data.List (dropWhileEnd)
import Data.Time (fromGregorianValid)
import Data.Void (Void)
import qualified Data.Set as S
import Text.Megaparsec
  ( Parsec, anySingle, choice, count, eof, many, match, lookAhead, notFollowedBy, oneOf
  , parse, parseMaybe, errorBundlePretty, satisfy, some, takeRest, takeWhileP
  , token, try )
import Text.Megaparsec.Char (char, string)
import Hikaru.Compat.Unicode (pyDecimalValue)
import Hikaru.HouseDay (HouseDay, houseDayNamed)
import Hikaru.Memory.Text (strip, rstrip, nfc, space)

data Layer = Mukashi | SukoshiMae | Konogoro deriving (Eq, Ord, Show, Enum, Bounded)
data Keep = Neyuki | Ikebi deriving (Eq, Ord, Show, Enum, Bounded)
data Mark = Unmarked | Kept Keep | LegacyKeep deriving (Eq, Show)

-- Invalid dates remain distinguishable from prose. Fade only reports them
-- when selection actually reaches that title, as it did before this parser.
data DateValue = ValidDay HouseDay | InvalidDay Integer Int Int deriving (Eq, Show)
data Period = Daily DateValue | Monthly Integer Int | Undated deriving (Eq, Show)
data HeadingDay = NoHeadingDay | HeadingDay DateValue deriving (Eq, Show)
data Ending = LF | End deriving (Eq, Show)
data Title = Title String Period deriving (Eq, Show)
data Role = LayerHead Layer | SectionHead Title | Prose deriving (Eq, Show)
data Entry = Entry String Ending Mark Role (Maybe (Int, String, HeadingDay)) deriving (Eq, Show)
data Section = Section Entry Title [Entry] deriving (Eq, Show)
data Block = Block [Entry] [Section] deriving (Eq, Show)
data LayerBlock = LayerBlock Entry Layer Block deriving (Eq, Show)
-- Keep the physical entry stream shared with its hierarchical view. A suffix
-- check or rendering need not build the layer projection first.
data Strobe = Strobe [Entry] Block [LayerBlock] deriving (Eq, Show)

layerOrder :: [Layer]
layerOrder = [minBound .. maxBound]

layerName :: Layer -> String
layerName Mukashi = "むかし"
layerName SukoshiMae = "すこし前"
layerName Konogoro = "このごろ"

keepName :: Keep -> String
keepName Neyuki = "ねゆき"
keepName Ikebi = "いけび"

type Parser = Parsec Void String

digit :: Parser Int
digit = token pyDecimalValue S.empty

number :: Int -> Int -> Parser Int
number low high = do
  first <- count low digit
  rest <- upTo (high-low)
  pure (foldl (\a b -> 10*a+b) 0 (first++rest))
  where
    upTo 0 = pure []
    upTo n = maybe [] id <$> optional ((:) <$> digit <*> upTo (n-1))

dayP :: Parser DateValue
dayP = do
  y <- fromIntegral <$> number 4 4 <* char '年'
  m <- number 1 2 <* char '月'
  d <- number 1 2 <* char '日'
  pure $ case if y < 1 || y > 9999 then Nothing else fromGregorianValid y m d of
    Just day -> ValidDay (houseDayNamed day)
    Nothing -> InvalidDay y m d

periodP :: Parser Period
periodP = try (Daily <$> dayP) <|> try monthP <|> pure Undated
  where
    monthP = do
      y <- fromIntegral <$> number 4 4 <* char '年'
      m <- number 1 2 <* char '月'
      notFollowedBy digit
      pure (Monthly y m)

layerP :: Parser Layer
layerP = do
  _ <- string "# "
  layer <- choice [l <$ string (layerName l) | l <- layerOrder]
  rest <- takeWhileP Nothing (/='\n')
  case rstrip rest of
    "" -> pure layer
    '（':xs | not (null xs), last xs == '）' -> pure layer
    _ -> fail "layer suffix"

-- Classify headings on the document stream itself. Prose is an explicit
-- production; a near miss such as "# このごろ (注)" remains ordinary text.
lineP :: Parser (String,Role)
lineP = do
  first <- lookAhead (optional anySingle)
  case first of
    Just '#' -> try (match (LayerHead <$> layerP)) <|> sectionP <|> proseP
    _ -> proseP
  where
    sectionP = do
      (raw,title) <- match $ do
        _ <- try (string "## ")
        (spelling,period) <- match $ do
          _ <- takeWhileP Nothing (\c -> c/='\n' && space c)
          value <- periodP
          _ <- takeWhileP Nothing (/='\n')
          pure value
        pure (Title (strip spelling) period)
      pure (raw,SectionHead title)
    proseP = (,Prose) <$> takeWhileP Nothing (/='\n')

-- Absence of a suffix mark is the Unmarked production, not an error recovery.
markOf :: String -> Mark
markOf line = case dropWhile space (reverse line) of
  closing:rest | closing `elem` ")）" -> case parseMaybe markP rest of
    Nothing -> Unmarked
    Just mark -> mark
  _ -> Unmarked
  where
    markP :: Parser Mark
    markP = do
      mark <- choice ([Kept k <$ string (reverse (keepName k)) | k <- [minBound .. maxBound]]
                     ++ [LegacyKeep <$ string (reverse "残す")])
      _ <- oneOf "(（"
      _ <- takeRest
      pure mark

headingP :: Parser (Int, String, HeadingDay)
headingP = do
  hashes <- some (char '#')
  if length hashes > 6 then fail "heading level" else pure ()
  _ <- satisfy space
  title <- strip <$> takeRest
  let found = parseMaybe (searchDay <* takeRest) title
  pure (length hashes, title, maybe NoHeadingDay HeadingDay found)
  where searchDay = try dayP <|> (anySingle *> searchDay)

-- The whole document is consumed by Megaparsec. Splitting is only on LF;
-- CR, Unicode separators, trailing spaces and the last empty line survive.
parseStrobe :: String -> Either String Strobe
parseStrobe = either (Left . errorBundlePretty) Right . parse documentP "strobe"
  where
    documentP :: Parser Strobe
    documentP = do
      first <- lineP
      rest <- many (char '\n' *> lineP)
      eof
      pure (arrange (entries (first:rest)))
    entries [] = []
    entries [line] = [entry line End]
    entries (line:rest) = entry line LF : entries rest
    entry (line,role) ending =
      Entry line ending (markOf line) role (heading line)
    heading line@('#':_) = parseMaybe headingP (nfc line)
    heading _ = Nothing

arrange :: [Entry] -> Strobe
arrange entries = let (outside,layers) = foldr step (Block [] [],[]) entries
                  in Strobe entries outside layers
  where
    step e@(Entry _ _ _ (LayerHead layer) _) (outside,layers) =
      (Block [] [],LayerBlock e layer outside:layers)
    step e (outside,layers) = (addEntry e outside,layers)

block :: [Entry] -> Block
block = foldr addEntry (Block [] [])

addEntry :: Entry -> Block -> Block
addEntry e@(Entry _ _ _ (SectionHead title) _) (Block prefix sections) =
  Block [] (Section e title prefix:sections)
addEntry e (Block prefix sections) = Block (e:prefix) sections

entryText :: Entry -> String
entryText (Entry line _ _ _ _) = line
entryMark :: Entry -> Mark
entryMark (Entry _ _ mark _ _) = mark
entryHeading :: Entry -> Maybe (Int,String)
entryHeading (Entry _ _ _ _ heading) = (\(level,title,_) -> (level,title)) <$> heading
entryHeadingDay :: Entry -> HeadingDay
entryHeadingDay (Entry _ _ _ _ heading) = maybe NoHeadingDay (\(_,_,day) -> day) heading

renderEntries :: [Entry] -> String
renderEntries = foldr appendLine ""
  where
    appendLine (Entry line ending _ _ _) rest = line ++ case ending of
      LF -> '\n':rest
      End -> rest
sectionTitle :: Section -> Title
sectionTitle (Section _ title _) = title
titleText :: Title -> String
titleText (Title text _) = text
titlePeriod :: Title -> Period
titlePeriod (Title _ period) = period
sectionEntries :: Section -> [Entry]
sectionEntries (Section _ _ entries) = entries
sectionLines :: Section -> [Entry]
sectionLines (Section heading _ entries) = heading:entries
blockLines :: Block -> [Entry]
blockLines (Block prefix sections) = prefix ++ concatMap sectionLines sections
strobeLines :: Strobe -> [Entry]
strobeLines (Strobe entries _ _) = entries
renderStrobe :: Strobe -> String
renderStrobe = renderEntries . strobeLines

layerHeadings :: Strobe -> [Layer]
layerHeadings (Strobe _ _ layers) = [l | LayerBlock _ l _ <- layers]
locatedSections :: Strobe -> [(Maybe Layer,Section)]
locatedSections (Strobe _ (Block _ outside) layers) = map (Nothing,) outside ++
  [(Just layer,s) | LayerBlock _ layer (Block _ sections) <- layers, s <- sections]

-- The old size calculation concatenates repeated instances of a layer,
-- including prose between them. Keep that projection, without parsing again.
layerLines :: Strobe -> Layer -> [Entry]
layerLines (Strobe _ (Block _ sections) []) Konogoro = concatMap sectionLines sections
layerLines (Strobe _ _ []) Mukashi = []
layerLines (Strobe _ _ []) SukoshiMae = []
layerLines (Strobe _ _ layers) wanted = concat [blockLines b | LayerBlock _ l b <- layers,l==wanted]
layerSize :: Strobe -> Layer -> Int
layerSize doc = length . strip . renderEntries . layerLines doc
layerDays :: Strobe -> Layer -> [(Title,Int)]
layerDays doc layer = [(sectionTitle s,length (strip (renderEntries (sectionLines s))))
                     | let Block _ sections = block (layerLines doc layer),s <- sections]
sectionBody :: Section -> String
sectionBody = dropWhile (=='\n') . dropWhileEnd (=='\n') . concatMap rendered . sectionEntries
  where rendered (Entry line ending _ _ _) = rstrip line ++ case ending of LF -> "\n"; End -> ""

-- A month summary remains a month, never an invented first day. Future
-- recency consumers can inspect this period beside each individual entry.
datedEntries :: Strobe -> [(Period,Entry)]
datedEntries doc = [(titlePeriod (sectionTitle s),e) | (_,s) <- locatedSections doc,e <- sectionEntries s]
