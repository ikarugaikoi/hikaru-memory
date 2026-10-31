{-# LANGUAGE OverloadedStrings #-}
-- | Fixed linguistic cues expressed as parser combinators, with typed results.
module Hikaru.Shirushi.Language
  ( Language(..), lang, squeeze, features, marksClaude, marksHumanSpeaker
  , quotes, stripParentheses, parentheses, stripQuotes, quoteContents
  , nameParts, snip, questionSentences, alternativeQuestion
  ) where

import Control.Monad (void)
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import Hikaru.Shirushi.Syntax
import Hikaru.Shirushi.Parse (before)
import Hikaru.Shirushi.Types

paren :: Parser Text
paren = pair '（' '）' <|> pair '(' ')'
  where pair open close = do
          _ <- char open
          body <- takeWhileP Nothing (\c -> c /= open && c /= close)
          _ <- char close
          pure (T.cons open (T.snoc body close))

quoted :: Parser Text
quoted = do
  _ <- oneOf ("「『“\"" :: String)
  body <- count' 3 60 (noneOf ("「」『』“”\"\n" :: String))
  _ <- oneOf ("」』”\"" :: String)
  pure (T.pack body)

stripParentheses, stripQuotes :: Text -> Text
stripParentheses = fst . scanParentheses
stripQuotes = fst . scanQuotes
parentheses, quoteContents :: Text -> [Text]
parentheses = snd . scanParentheses
quoteContents = snd . scanQuotes

scanParentheses, scanQuotes :: Text -> (Text, [Text])
scanParentheses = scanFrom (`elem` ("（(" :: String)) paren
scanQuotes = scanFrom (`elem` ("「『“\"" :: String)) quoted

hans :: S.Set Char
hans = S.fromList "这说们吗么还没对让给过时问觉呢吧啊很为什个从东车书见发关开气现实认该应听话语读习请谁哪怎样两边别虽经难题谢爱图园长门间简单复杂师运远进连选议论词诗调谈课谓译识诉试详误讲记许设评证确刚种办帮务业总变联"

kana :: Char -> Bool
kana c = c >= '\x3040' && c <= '\x30ff'

data Language = Mixed | Japanese | Chinese | Other deriving (Eq, Show)
lang :: Text -> Language
lang text = case (T.any kana text, T.any (`S.member` hans) text) of
  (True, True) -> Mixed
  (True, False) -> Japanese
  (False, True) -> Chinese
  _ -> Other

squeeze :: Text -> Text
squeeze = T.filter (not . whiteSpace)

listLine :: Text -> Bool
listLine = maybe False (const True) . parseMaybe (spaces *> bullet <* takeRest)
  where bullet = try (oneOf ("-*・" :: String) *> satisfy whiteSpace)
                 <|> (takeWhile1P Nothing decimalChar *> oneOf (".．、" :: String))

firstWord :: Text -> Text
firstWord original = fromMaybe (T.take 4 original) $
  parseMaybe (choice [try (T.pack <$> count n (satisfy (/= '\n')) <* lookAhead boundary) | n <- [1..6]] <* takeRest) (strip original)
  where boundary :: Parser ()
        boundary = eof <|> void (satisfy (\c -> whiteSpace c || c `elem` ("、。，！？!?…" :: String)))

features :: Message -> Shape
features msg = Shape
  { shapeLen = size
  , shapeEndQ = maybe False ((`elem` ("?？" :: String)) . snd) (T.unsnoc stripped)
  , shapeNQ = T.length (T.filter (`elem` ("?？" :: String)) text)
  , shapeParenN = length pairs
  , shapeParenRatio = if size == 0 then 0 else fromIntegral (sum (map T.length pairs)) / fromIntegral size
  , shapeList = length (filter listLine lines_)
  , shapeBold = T.count "**" text `div` 2
  , shapeHeading = length (filter (T.isPrefixOf "#") lines_)
  , shapeMixed = lang text == Mixed
  , shapeFirst = firstWord text
  }
  where
    text = messageText msg
    stripped = strip body
    (body, pairs) = scanParentheses text
    size = T.length text
    lines_ = T.splitOn "\n" text

-- 語の先頭は語表から一緒に作る。候補の位置だけで同じ構文を試し、
-- 選択肢の順番と、一度拾った語に重ねて拾わない規則を保つ。
data Cue = Cue { cueStarts :: !(S.Set Char), cueParser :: Parser Text }

literals :: [Text] -> Cue
literals words_ = Cue (S.fromList (map T.head words_)) (choice (map (try . string) words_))
cues :: [Cue] -> Cue
cues choices = Cue (S.unions (map cueStarts choices)) (choice (map (try . cueParser) choices))
searchCue :: Cue -> Text -> [Text]
searchCue (Cue starts parser) = snd . scanFrom (`S.member` starts) parser

question :: Text -> Cue
question prefix = Cue (S.singleton (T.head prefix)) $
  (<>) <$> string prefix <*> (T.singleton <$> oneOf ("?？" :: String))
again :: Cue
again = Cue (S.singleton 'ま') $ choice [try $ do
  prefix <- string "また"
  middle <- T.pack <$> count n (satisfy (/= '\n'))
  suffix <- string "しちゃった"
  pure (prefix <> middle <> suffix) | n <- [6,5..0]]
markdown :: Cue
markdown = Cue (S.singleton '.') $ string ".md" <* notFollowedBy (satisfy asciiWord)
  where asciiWord c = c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9' || c == '_'

avoid :: [(MarkKind, Cue)]
avoid =
  [(Waiting,literals ["慢慢说","不急","我等你","ゆっくりでいい","急がなくていい"]),
   (Apology,cues [literals ["是我想多了","是我理解错了","是我没"], again]),
   (Prompting,cues [literals ["后来呢"],question "それから",question "どうだった",question "然后呢"]),
   (Gambling,literals ["我押","押一","开牌","买定离手"]),
   (NotHere,literals ["本当に来た気","まるで本当","もし本当に","好像真的","就像真的","仿佛"]),
   (Body,literals ["耳がない","目がない","缺耳朵","我最缺","補聴器","助听器","義肢","义肢","假肢"]),
   (ThirdPerson,literals ["あっちの彼女","那边的她"]),
   (Declaration,literals ["これからは","从现在开始","以后我会","我以后不"]),
   (Backstage,cues [literals ["文件夹","图片","ファイル名",".jpg",".png",".webp"], markdown])]

unique :: Ord a => [a] -> [a]
unique = go S.empty
  where go _ [] = []
        go seen (x:xs) | S.member x seen = go seen xs
                       | otherwise = x : go (S.insert x seen) xs
joined :: [Text] -> Text
joined = T.intercalate "、" . unique

marksClaude :: Message -> [(MarkKind, Text)]
marksClaude msg = [(kind, joined hits) | (kind, cue) <- avoid,
  let hits = searchCue cue (messageText msg), not (null hits)]

correction, hesitate, pleased :: Cue
correction = literals ["不是","不对","不，","不。","别","不用","不要","哦不是","哦，不是","いや","違う","ちがう",
                       "やめて","そうじゃなく","じゃなくて","ではなく","不太对"]
hesitate = literals ["唉","怎么说呢","呃","well","嗯…","嗯……","吧…","吧……"]
pleased = literals ["笑","哈哈","えへへ","ふふ","好き","すき","喜欢","すごっ","すごい","厉害","谢谢","ありがとう",
                    "嬉しい","うれしい","开心","幸せ","可爱","かわいい"]

marksHumanSpeaker :: Message -> [(MarkKind, Text)]
marksHumanSpeaker msg = correctionHit ++ [(Hesitation, joined hesitation) | not (null hesitation)]
  ++ [(Pleasure, joined pleasure) | not (null pleasure)] ++ [(Short, text) | T.length text <= 3]
  where
    text = strip (messageText msg)
    correctionHit = maybe [] (\hit -> [(Correction, hit)]) (parseMaybe (cueParser correction <* takeRest) text)
    hesitation = searchCue hesitate text ++ ["末尾…" | any (`T.isSuffixOf` text) ["…", "..."]]
    pleasure = searchCue pleased text

names, nameParts :: S.Set Text
names = S.fromList ["ひかる","クロード","ユーザー","丘の上","ストロボメモリー","一週間フレンズ",
                   "タイムマシン","Rabbit_Deer_and_You","ビブリア","Claude","Codex"]
nameParts = S.fromList [T.take k (T.drop i name) | name <- S.toList names,
  k <- [3..T.length name], i <- [0..T.length name-k]]

quotes :: Message -> [Message] -> [Quote]
quotes msg history = map matchQuote (filter (`S.notMember` names) (quoteContents (messageText msg)))
  where
    base = lang (stripQuotes (stripParentheses (messageText msg)))
    matchQuote text = Quote text hit (ql == base || ql == Other || base == Other || base == Mixed)
      where ql = lang text
            hit = find (\h -> before h msg && squeeze text `T.isInfixOf` squeeze (messageText h)) (reverse history)

snip :: Text -> Int -> Text
snip text size = T.take size one <> if T.length one > size then "…" else ""
  where one = T.intercalate " " (filter (not . T.null) (T.split whiteSpace text))

questionSentences :: Text -> [Text]
questionSentences = filter (not . T.null) . map T.strip . catMaybes . snd . scan sentence
  where
    -- 問いでない文も終端まで一度で読む。失敗して同じ長い段落の
    -- 二文字目、三文字目から読み直すことはしない。
    sentence = do
      body <- takeWhile1P Nothing (`notElem` ("。！!\n？?" :: String))
      ending <- optional (oneOf ("。！!\n？?" :: String))
      pure $ if ending == Just '？' || ending == Just '?' then Just body else Nothing

alternativeQuestion :: Text -> Bool
alternativeQuestion sentence =
  (let (prefix, suffix) = T.breakOn "还是" sentence in not (T.null suffix) && T.length prefix >= 2)
  || any (`T.isInfixOf` sentence) ["それとも", "どっち", "どちら"]
