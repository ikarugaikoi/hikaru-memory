{-# LANGUAGE OverloadedStrings #-}
-- | Pure archive grammar and rendering. Only literal LF separates body lines.
module Hikaru.Ochibo.Format
  ( EntryHead(..), EntryKey(..), PoolKey(..), EntryKeys, PoolKeys
  , pending, header, tmFileName, parseEntryHead, decimalDigit
  , escapeText, unescapeText, normBody, pictureNames
  , entriesIn, countEntries, entryKeysList, poolKeysList
  , poolKeys, lostOnRebuild, oldFormat, appendPrefix, alreadyPresent, consumeEntry
  , entry, messageDay, clockText, collectDay
  ) where

import Control.Applicative ((<|>), optional)
import Data.Char (GeneralCategory(DecimalNumber), generalCategory, ord, chr)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, count, satisfy, takeWhile1P, takeWhileP, eof, token)
import Text.Megaparsec.Char (char, string)
import Hikaru.Conversation.Input (jst, archiveSpace, stripArchive)
import Hikaru.Conversation.Line
import Hikaru.Conversation.Types (Speaker(..))
import Hikaru.HouseDay (HouseDay, houseDayDate, houseDayFromJST)

type Parser = Parsec Void Text
data EntryHead = EntryHead
  { entryRole :: !Speaker, entryTime :: !Text, entrySession :: !(Maybe Text) }
  deriving (Eq,Ord,Show)
data EntryKey = EntryKey !Speaker !(Maybe Text) !Text !Text deriving (Eq,Ord,Show)
-- Rebuild identity uses the full session recovered from the existing table,
-- exact seconds and normalized body. Counts preserve repeated utterances.
data PoolKey = PoolKey !Speaker !Text !Text !Text deriving (Eq,Ord,Show)
newtype EntryKeys = EntryKeys (M.Map EntryKey Int) deriving Show
newtype PoolKeys = PoolKeys (M.Map PoolKey Int) deriving Show
entryKeysList :: EntryKeys -> [EntryKey]
entryKeysList (EntryKeys keys) = concat [replicate n key | (key,n) <- M.toList keys]
poolKeysList :: PoolKeys -> [PoolKey]
poolKeysList (PoolKeys keys) = concat [replicate n key | (key,n) <- M.toList keys]
num :: Show a => a -> Text
num = T.pack . show

pending :: Integer -> Integer -> Text -> Text -> Text
pending 0 _ _ _ = ""
pending opened waiting word tailText = T.concat
  ["まだ閉じていないひととき",word," ",num opened,"（"
  ,if waiting /= 0 then T.concat ["それにかかる閉じたひととき ",num waiting," も",tailText] else tailText,"）"]
header :: HouseDay -> Text
header day = let (y,m,d)=toGregorian (houseDayDate day) in
  T.concat ["# ",num y,"年",num m,"月",num d,"日（5時〜翌4時59分）\n\n"]
tmFileName :: HouseDay -> Text
tmFileName day = num day <> ".md.b64"

-- Decimal blocks in Unicode contain complete groups of ten, sometimes
-- adjacent (the mathematical alphabets). GeneralCategory excludes superscripts.
decimalDigit :: Char -> Maybe Int
decimalDigit c
  | generalCategory c /= DecimalNumber = Nothing
  | otherwise = Just ((ord c - beginning (ord c)) `mod` 10)
  where beginning n | generalCategory (chr (n-1)) == DecimalNumber = beginning (n-1)
                    | otherwise = n
isDecimal :: Char -> Bool
isDecimal c = generalCategory c == DecimalNumber

parseEntryHead :: Text -> Maybe EntryHead
parseEntryHead = parseMaybe (heading <* eof)
  where
    pair = T.pack <$> count 2 (satisfy isDecimal)
    heading :: Parser EntryHead
    heading = do
      role <- (HumanSpeaker <$ string "**ふ：**（") <|> (Claude <$ string "**Claude：**（")
      hh <- pair
      _ <- char ':'
      mm <- pair
      ss <- optional (char ':' *> pair)
      sid <- optional (char '・' *> takeWhile1P Nothing (\c -> c /= '）' && c /= '・'))
      _ <- char '）'
      _ <- takeWhileP Nothing archiveSpace
      pure (EntryHead role (hh <> ":" <> mm <> maybe "" (":" <>) ss) sid)

collectDay :: Text -> Maybe Day
collectDay = parseMaybe (dayParser <* eof)
  where
    decimal = token decimalDigit mempty
    ascii a b = (\c -> ord c - ord '0') <$> satisfy (\c -> c >= a && c <= b)
    number = foldl' (\n d -> n*10 + d) 0
    dayParser :: Parser Day
    dayParser = do
      y <- number <$> count 4 decimal
      _ <- char '-'
      -- Match the established strptime grammar, including unpadded dates.
      m <- (char '0' *> (number . pure <$> ascii '1' '9'))
        <|> (do a <- ascii '1' '9'; b <- if a == 1 then optional (ascii '0' '2') else pure Nothing
                pure (number (a:maybe [] pure b)))
      _ <- char '-'
      d <- (char ' ' *> (number . pure <$> ascii '1' '9'))
        <|> (char '0' *> (number . pure <$> ascii '1' '9'))
        <|> (do a <- ascii '1' '9'; b <- if a `elem` [1,2] then optional decimal
                                       else if a == 3 then optional (ascii '0' '1') else pure Nothing
                pure (number (a:maybe [] pure b)))
      case fromGregorianValid (toInteger y) m d of
        Just day | y >= 1 -> pure day
        _ -> fail "invalid date"

specialPrefix :: Text -> Bool
specialPrefix line = "**ふ：**（" `T.isPrefixOf` line || "**Claude：**（" `T.isPrefixOf` line ||
  ("# " `T.isPrefixOf` line && let digits=T.take 4 (T.drop 2 line) in T.length digits==4 && T.all isDecimal digits)
escapeText :: Text -> Text
escapeText = T.intercalate "\n" . map escape . T.splitOn "\n"
  where escape line | specialPrefix (T.dropWhile (=='\\') line) = "\\" <> line
                    | otherwise = line
unescapeText :: Text -> Text
unescapeText = T.intercalate "\n" . map unescape . T.splitOn "\n"
  where unescape line = case T.uncons line of
          Just ('\\',rest) | specialPrefix (T.dropWhile (=='\\') rest) -> rest
          _ -> line

normBody :: Bool -> Text -> Text
normBody unescape = stripArchive . T.intercalate "\n" . map replace . T.splitOn "\n" . prepare
  where
    prepare = if unescape then unescapeText else id
    replace text = case T.breakOn "【画像" text of
      (_,rest) | T.null rest -> text
      (before,rest) -> case T.breakOn "】" (T.drop 3 rest) of
        (_,after) | T.null after -> text
        (_,after) -> before <> "【画像】" <> replace (T.drop 1 after)
pictureNames :: Text -> [Text]
pictureNames = concatMap find . T.splitOn "\n"
  where
    find text = case T.breakOn "【画像: " text of
      (_,rest) | T.null rest -> []
      (_,rest) -> case T.breakOn "】" (T.drop 5 rest) of
        (_,after) | T.null after -> []
        (name,after) -> [name | not (T.null name)] ++ find (T.drop 1 after)

bodyEntries :: Text -> [(EntryHead,Text)]
bodyEntries value = reverse (finish current body result)
  where
    (current,body,result) = foldl' visit (Nothing,[],[]) (T.splitOn "\n" value)
    finish Nothing _ acc = acc
    finish (Just h) reversed acc = (h,T.intercalate "\n" (reverse reversed)):acc
    visit (cur,buf,acc) line = case parseEntryHead line of
      Just h -> (Just h,[],finish cur buf acc)
      Nothing -> (cur,case cur of Nothing -> buf; Just _ -> line:buf,acc)
entriesIn :: Text -> EntryKeys
entriesIn value = EntryKeys (M.fromListWith (+)
  [(EntryKey (entryRole h) (entrySession h) (entryTime h) (stripArchive body),1) | (h,body) <- bodyEntries value])
countEntries :: Text -> Int
countEntries = length . bodyEntries
oldFormat :: Text -> HouseDay -> Bool
oldFormat value day = not (T.null (stripArchive value)) && stripArchive (T.takeWhile (/='\n') value) /= stripArchive (header day)
appendPrefix :: HouseDay -> Text -> Text
appendPrefix day old | T.null old = header day
                     | otherwise = T.dropWhileEnd (=='\n') old <> "\n\n"
clockText :: Bool -> Utterance -> Text
clockText seconds = T.pack . formatTime defaultTimeLocale (if seconds then "%H:%M:%S" else "%H:%M") . utcToLocalTime jst . utteranceTime
messageDay :: Utterance -> Either Text HouseDay
messageDay = maybe (Left "hikaru day outside datetime year range") Right . houseDayFromJST . utcToLocalTime jst . utteranceTime
poolKeys :: [Utterance] -> PoolKeys
poolKeys messages = PoolKeys (M.fromListWith (+)
  [(PoolKey (utteranceSpeaker m) full (clockText True m) (normBody False (utteranceText m)),1)
  | m <- messages, Just full <- [utteranceSession m], not (T.null full)])
lostOnRebuild :: [(Text,Text)] -> Text -> PoolKeys -> [(Text,Text)]
lostOnRebuild table old (PoolKeys keys) = reverse (snd (foldl' consume (keys,[]) (bodyEntries old)))
  where
    -- A minute-only header, missing session, or unmapped alias cannot prove
    -- identity. Do not guess that a new session allocation denotes old text.
    recover h body = do
      alias <- entrySession h
      full <- lookup alias table
      if T.length (entryTime h) == 8 && alias /= "----" && not (T.null full)
        then Just (PoolKey (entryRole h) full (entryTime h) (normBody True body))
        else Nothing
    consume (remaining,lost) (h,body) = case recover h body >>= (`consumeKey` remaining) of
      Just rest -> (rest,lost)
      Nothing -> (remaining,(entryTime h,speakerText (entryRole h)):lost)
entry :: Text -> Utterance -> Text
entry alias message = T.concat ["**",speakerText (utteranceSpeaker message),"：**（",clockText True message,"・",alias,"）\n\n",escapeText (utteranceText message),"\n\n"]
alreadyPresent :: EntryKeys -> Text -> Utterance -> Bool
alreadyPresent keys alias message = isJust (consumeEntry keys alias message)
-- Consume only existing archive occurrences. Distinct admitted source rows
-- with the same recoverable identity still each need an archive occurrence.
consumeEntry :: EntryKeys -> Text -> Utterance -> Maybe EntryKeys
consumeEntry (EntryKeys keys) alias message =
  let who=utteranceSpeaker message; time=clockText True message; body=escapeText (utteranceText message)
  in EntryKeys <$> (consumeKey (EntryKey who (Just alias) time body) keys
                   <|> consumeKey (EntryKey who Nothing time body) keys)
consumeKey :: Ord key => key -> M.Map key Int -> Maybe (M.Map key Int)
consumeKey key keys = do
  n <- M.lookup key keys
  pure (if n == 1 then M.delete key keys else M.insert key (n-1) keys)
