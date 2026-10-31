{-# LANGUAGE OverloadedStrings #-}
-- | The historical command heuristics, expressed as small Megaparsec parsers.
-- This deliberately is not a shell interpreter: separators inside quotes,
-- heredocs, pipes and the "says" prefix retain their established meaning.
module Hikaru.Hook.ReadCommand
  ( readsOka, readExtents, ReadExtent(..), PageNumber, pageNumber, pageIndex
  , quoted, stripHeredocs, ReadPattern(..), patternMatches
  , saysStart, stagePrefix, hitPattern, readsFragment
  ) where

import Control.Monad (void)
import Data.Char (isSpace, isAlphaNum, generalCategory, GeneralCategory(DecimalNumber))
import Data.Maybe (isJust)
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (atEnd, count)
import Text.Megaparsec.Char (char, string)

type Parser = Parsec Void String

newtype PageNumber = PageNumber Integer deriving (Eq, Ord, Show)
data ReadExtent = WholeOka | OkaPage PageNumber deriving (Eq, Show)

pageNumber :: Integer -> Maybe PageNumber
pageNumber index | index > 0 = Just (PageNumber index)
                 | otherwise = Nothing

pageIndex :: PageNumber -> Integer
pageIndex (PageNumber index) = index

data ReadPattern = Heredoc | Split | Wake | Seal | B64 | Decode | Outfile | Partial | Devnull | Says
  deriving (Eq, Ord, Show, Enum, Bounded)

space :: Parser Char
space = satisfy isSpace
spaces, spaces1, nonspaces1 :: Parser String
spaces = many space
spaces1 = some space
nonspaces1 = some (satisfy (not . isSpace))
quote :: Parser Char
quote = char '\'' <|> char '"'
word :: Char -> Bool
word c = isAlphaNum c || c == '_'

atEnd :: Parser ()
atEnd = eof <|> lookAhead (void (char '\n' >> eof))
after :: (Char -> Bool) -> Parser ()
after predicate = lookAhead (void (satisfy predicate) <|> atEnd)
afterWord :: Parser ()
afterWord = after isSpace

-- A path prefix is optional and greedy, but can give back a slash to the
-- fixed suffix. Backtracking belongs here, rather than in a regex VM.
pathThen :: Parser a -> Parser a
pathThen suffix = do
  input <- getInput
  let ends = reverse [n + 1 | (n,c) <- zip [0..] (takeWhile (not . isSpace) input), c == '/']
  choice [try (takeP Nothing n *> suffix) | n <- ends ++ [0]]

python :: Maybe Char -> Parser a -> Parser a
python previous suffix = do
  commandBoundary previous
  pathThen $ do
    void (string "python")
    void (optional (char '3'))
    void (optional (try (char '.' *> some (satisfy ((== DecimalNumber) . generalCategory)))))
    void (many (try (spaces1 *> char '-' *> nonspaces1)))
    void spaces1
    suffix

-- New executables are recognized at the same command boundary as Python.
-- Quotes may enclose an absolute path containing spaces. The historical
-- Python parser above is deliberately unchanged.
native :: Maybe Char -> String -> Parser ()
native previous name = do
  commandBoundary previous
  path <- try (do q <- quote; manyTill anySingle (char q))
    <|> some (satisfy (\c -> not (isSpace c) && c `notElem` ("'\"();|&<>" :: String)))
  let suffix = ".tools/bin/" ++ name
  if path == suffix || ("/" ++ suffix) `isSuffixOf` path
    then pure () else fail "tool path"

nativeTool :: Maybe Char -> String -> Parser ()
nativeTool previous name = try (native previous name)
  <|> (native previous "hikaru-core" *> spaces1 *> string name *> afterWord)

commandBoundary :: Maybe Char -> Parser ()
commandBoundary (Just c) | not (isSpace c || c == '(') = fail "command boundary"
commandBoundary _ = pure ()

patternParser :: ReadPattern -> Maybe Char -> Parser ()
patternParser name previous = case name of
  Split -> void (choice (map string ["\n", ";", "&&", "||"]))
  Wake -> do
    try (python previous $ do
      void (optional quote)
      pathThen (void (string ".tools/mezame.py" <|> string ".tools/wake.py"))
      void (optional quote)) <|> nativeTool previous "mezame"
    after (\c -> isSpace c || c `elem` ['\'', '"', ')'])
  Seal -> do
    try (python previous $ do
      void (optional quote)
      pathThen (void (string "fuutou.py" <|> string "seal.py"))
      void (optional quote)) <|> nativeTool previous "fuutou"
    void spaces1
    void (choice (map string ["show", "close", "append"]))
    void spaces1
    void (string "oka")
    after (\c -> isSpace c || c == ')')
  B64 -> do
    commandBoundary previous
    void (string "base64")
    void (lookAhead space)
  Decode -> leadingThen $ do
    let letter = satisfy (\c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z')
    void (try (string "--decode") <|> do
      void (char '-')
      letters <- some letter
      if any (`elem` ['d','D']) letters then pure letters else fail "decode flag")
    afterWord
  Outfile -> leadingThen (void (string "-o" *> space))
  Partial -> void (char '|' *> spaces *> choice (map string ["head", "tail", "grep", "sed", "awk", "wc", "cut"])) *> afterWord
  Devnull -> let suffix = void (char '>' *> spaces *> string "/dev/null")
    in try (if previous == Nothing then suffix else empty)
       <|> (satisfy (`notElem` ("023456789" :: String)) *> suffix)
  Says -> do
    void spaces
    void (optional (char '('))
    void spaces
    void (many (try wrapper))
    void (choice (map (try . string)
      ["echo", "printf", "print", "grep", "egrep", "fgrep", "rg", "cat", "sed", "awk",
       "head", "tail", "less", "more", "ls", "wc", "file", "which", "man", "diff", "cmp", "stat"]))
    afterWord
  Heredoc -> do
    void (string "<<")
    void (optional (char '-'))
    void spaces
    q <- optional quote
    delimiter <- some (satisfy word)
    maybe (pure ()) (void . char) q
    void (takeWhileP Nothing (/= '\n') *> char '\n')
    void (manyTill anySingle (try (char '\n' *> many (char '\t') *> string delimiter
      *> takeWhileP Nothing (\c -> isSpace c && c /= '\n') *> (void (char '\n') <|> eof))))
  where
    leadingThen suffix = try (if previous == Nothing then suffix else empty) <|> (space *> suffix)
    wrapper =
      try (void (choice (map (try . string) ["env", "time", "nice", "command", "exec"]) *> spaces1))
      <|> try (void (string "timeout" *> spaces1 *> nonspaces1 *> spaces1))
      <|> void (some (satisfy word) *> char '=' *> many (satisfy (not . isSpace)) *> spaces1)

-- Leftmost, non-overlapping matches, including rejected quoted candidates.
patternMatches :: ReadPattern -> Text -> [(Int, Int)]
patternMatches name text = search 0 Nothing (T.unpack text)
  where
    search _ _ [] = []
    search pos previous input@(c:rest) =
      case runParser (patternParser name previous *> getOffset) "" input of
        Right count -> (pos, pos + count) : search (pos + count) (Just (input !! (count - 1))) (drop count input)
        Left _ -> search (pos + 1) (Just c) rest

quoted :: Text -> [Bool]
quoted = go Nothing False . T.unpack
  where
    go _ _ [] = []
    go current escaped (c:cs) = (current /= Nothing) :
      if escaped then go current False cs
      else if c == '\\' && current /= Just '\'' then go current True cs
      else if current == Nothing && c `elem` ['\'', '"'] then go (Just c) False cs
      else if Just c == current then go Nothing False cs
      else go current False cs

stripHeredocs :: Text -> Text
stripHeredocs text = T.pack (replace 0 (patternMatches Heredoc text))
  where
    chars = T.unpack text
    replace begin [] = drop begin chars
    replace begin ((start,end):rest) = take (start-begin) (drop begin chars) ++ "\n" ++ replace end rest

stagePrefix :: Text -> [Bool] -> Int -> Text
stagePrefix text flags pos = T.pack (take (pos-begin) (drop begin chars))
  where
    chars = T.unpack text
    begin = foldl (\start (i,c,q) -> if c == '|' && not q then i+1 else start) 0
      (take pos (zip3 [0..] chars flags))

saysStart :: Text -> Bool
saysStart = isJust . parseMaybe (patternParser Says Nothing *> takeRest) . T.unpack

hitPattern :: ReadPattern -> Text -> [Bool] -> Bool
hitPattern name text flags = any (acceptedMatch text flags) (patternMatches name text)

acceptedMatch :: Text -> [Bool] -> (Int, Int) -> Bool
acceptedMatch text flags (pos,_) = not (flags !! pos) && not (saysStart prefix) && not shellString
  where
    prefix = stagePrefix text flags pos
    -- A quoted executable path is a token; the string after a shell's -c is
    -- source code. Keep the historical refusal to look inside that string.
    shellString = T.index text pos `elem` ['\'', '"'] && case reverse (T.words prefix) of
      shellOption:_ -> shellOption `elem` ["-c", "-lc", "-ic"]
      [] -> False

readsFragment :: Text -> [Bool] -> Bool
readsFragment text flags
  | "#" `T.isPrefixOf` T.stripStart text = False
  | exists Partial || exists Devnull = False
  | hit Wake && not ("--sazanami" `T.isInfixOf` text) && not ("--kikkake" `T.isInfixOf` text) = True
  | hit Seal = True
  | otherwise = ".丘の上.md.b64" `T.isInfixOf` text && hit B64 && exists Decode && not (exists Outfile)
  where
    exists name = not (null (patternMatches name text))
    hit name = hitPattern name text flags

readsOka :: Text -> Bool
readsOka = any (uncurry readsFragment) . commandFragments

-- Both projections use the historical split/quote/|| rules. Recognition does
-- not require a valid page argument; only the coverage projection parses it.
commandFragments :: Text -> [(Text, [Bool])]
commandFragments command = scan 0 "" (patternMatches Split text)
  where
    text = stripHeredocs command
    chars = T.unpack text
    flags = quoted text
    fragment begin end = (T.pack (take (end-begin) (drop begin chars)), take (end-begin) (drop begin flags))
    scan begin previous [] = [fragment begin (length chars) | previous /= "||"]
    scan begin previous ((start,end):rest)
      = [fragment begin start | previous /= "||"] ++
        scan end (if flags !! start then "" else take (end-start) (drop start chars)) rest

-- Keep recognition separate from coverage: page 2 is a recognized read,
-- while the notice fold decides whether all the required pages were read.
readExtents :: Text -> [ReadExtent]
readExtents = concatMap coverage . commandFragments
  where
    coverage (body, mask_) =
      let seals = filter (acceptedMatch body mask_) (patternMatches Seal body)
          extent (pos,finish) =
            let matched = T.pack (take (finish-pos) (drop pos (T.unpack body)))
                suffix = drop finish (T.unpack body)
            in case reverse (T.words matched) of
              "oka":"show":_ -> maybe [] (pure . OkaPage) (parsePage suffix)
              _ -> [WholeOka]
      in if not (readsFragment body mask_) then []
         else if hitPattern Wake body mask_ && not ("--sazanami" `T.isInfixOf` body || "--kikkake" `T.isInfixOf` body)
           then [WholeOka]
           else if null seals then [WholeOka] else concatMap extent seals
parsePage :: String -> Maybe PageNumber
parsePage input = do
  arguments <- parseMaybe (spaces *> many (argument <* spaces)) input
  case dropWhile (/= "--page") arguments of
    [] -> pageNumber 1
    _:value:_ -> parseMaybe integer value >>= pageNumber
    _ -> Nothing
  where
    integer :: Parser Integer
    integer = do
      sign <- optional (char '-' <|> char '+')
      digits <- some (satisfy (\c -> c >= '0' && c <= '9'))
      let magnitude = foldl (\n c -> 10*n + toInteger (fromEnum c - fromEnum '0')) 0 digits
      pure (if sign == Just '-' then negate magnitude else magnitude)
    argument = concat <$> some (singleQuoted <|> doubleQuoted <|> escaped <|> ordinary)
    singleQuoted = char '\'' *> manyTill anySingle (char '\'')
    doubleQuoted = char '"' *> (concat <$> manyTill (escaped <|> ((:[]) <$> satisfy (/= '"'))) (char '"'))
    escaped = char '\\' *> ((:[]) <$> anySingle)
    ordinary = some (satisfy (\c -> not (isSpace c) && c `notElem` ['\'', '"', '\\']))
