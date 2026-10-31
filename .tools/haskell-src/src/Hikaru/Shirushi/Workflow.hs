{-# LANGUAGE OverloadedStrings #-}
-- | Read, parse once, decide, render. Paths, clock and legacy storage adapters
-- are supplied by Main; there are no Python values inside mark.
module Hikaru.Shirushi.Workflow
  ( MarkEnv(..), Options(..), loadMessages, analyzeWithSessions, runArguments, argumentDay, arguments ) where

import Control.Applicative (optional, some)
import Control.Exception (throwIO)
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty)
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.FilePath ((</>))
import Text.Megaparsec (sepBy1, oneOf, choice)
import Text.Megaparsec.Char (char, string, string')
import Text.Read (readMaybe)
import Hikaru.Shirushi.Analyze (LiveAnalysis, analyze)
import Hikaru.Shirushi.Id (Sessions, emptySessions)
import Hikaru.Shirushi.Json (toJson)
import qualified Hikaru.Shirushi.Parse as P
import Hikaru.Shirushi.Render (render)
import Hikaru.Shirushi.Repeats
import Hikaru.Shirushi.Syntax
import Hikaru.Shirushi.Types
import Hikaru.Shirushi.Usage (usage)
import Hikaru.RecordRead (readRecord)

data MarkEnv = MarkEnv
  { markTimeMachine :: !FilePath, markLoadSessions :: IO (Maybe Sessions)
  , markNow :: IO LocalTime, markOutput :: Text -> IO ()
  , markPage :: Text -> Integer -> IO Text }

data Options = Options
  { startDay :: !Day, endDay :: !Day, gap :: !(Result NominalDiffTime)
  , seed :: !Integer, page :: !Integer, asJson :: !Bool }
  deriving (Eq, Show)

require :: Result a -> IO a
require = either throwIO pure

loadMessages :: MarkEnv -> Day -> Day -> IO [Message]
loadMessages env first last_ = P.orderLoaded . concat <$> traverse readDay [first..last_]
  where
    readDay day = do
      let path = markTimeMachine env </> show day ++ ".md.b64"
      body <- (Just <$> readRecord path) `catchIOError` \err ->
        if isDoesNotExistError err then pure Nothing
        else ioError err
      msgs <- maybe (pure []) (require . (`P.parse` Just day)) body
      let (year, _, _) = toGregorian (addDays 1 day)
      if year > 9999 then throwIO (MarkError "day range outside 1..9999") else pure msgs

-- The caller loads the session table once, before traversing the blocks.
analyzeWithSessions :: MarkEnv -> Integer -> [Message] -> [Repetition] -> Maybe Sessions -> NonEmpty Message -> IO LiveAnalysis
analyzeWithSessions _ selectedSeed history repetitions provided block =
  require (analyze selectedSeed history repetitions (fromMaybe emptySessions provided) block)
{-# INLINE analyzeWithSessions #-}

argumentDay :: Text -> Result Day
argumentDay = parseArgument "date" day
  where
    -- The command's existing date grammar is narrower than day-file headings:
    -- months are ASCII; only days beginning in 1/2 admit a Unicode units digit.
    day = do
      year <- decimal 4 4 <* char '-'
      month <- choice [n <$ string (T.justifyRight 2 '0' (T.pack (show n))) | n <- [1..12 :: Int]]
      _ <- char '-'
      date <- choice
        [char '0' *> (digitToInt <$> oneOf ("123456789" :: String))
        ,do tens <- digitToInt <$> oneOf ("12" :: String)
            units <- decimal 1 1
            pure (10 * tens + units)
        ,char '3' *> ((30 +) . digitToInt <$> oneOf ("01" :: String))]
      maybe (fail "invalid calendar date") pure
        (if year == 0 then Nothing else fromGregorianValid (toInteger year) month date)

parseArgument :: Text -> Parser a -> Text -> Result a
parseArgument label parser value = maybe (Left (MarkError ("invalid " <> label <> ": " <> value))) Right (match parser value)

digitGroups :: Parser [Int]
digitGroups = concat <$> some (decimal 1 1) `sepBy1` char '_'

signed :: Num a => Parser a -> Parser a
signed parser = do
  sign <- optional (oneOf ("+-" :: String))
  value <- parser
  pure (if sign == Just '-' then negate value else value)

integer :: Parser Integer
integer = signed $ do
  digits <- digitGroups
  -- This is an existing argument limit: removing it would change exit codes.
  if length digits > 4300 then fail "integer exceeds 4300 digits" else pure (number digits)

number :: [Int] -> Integer
number = foldl' (\n d -> n * 10 + toInteger d) 0

floating :: Parser Double
floating = signed (choice [0/0 <$ string' "nan", 1/0 <$ string' "infinity", 1/0 <$ string' "inf", finite])

finite :: Parser Double
finite = do
  whole <- optional digitGroups
  fraction <- optional (char '.' *> optional digitGroups)
  case (whole, fraction) of
    (Nothing, Nothing) -> fail "expected a number"
    (Nothing, Just Nothing) -> fail "expected decimal digits"
    _ -> pure ()
  power <- fromMaybe 0 <$> optional (oneOf ("eE" :: String) *> signed (number <$> digitGroups))
  let decimals = fromMaybe [] (joinMaybe fraction)
      coefficient = number (fromMaybe [] whole ++ decimals)
      literal = show coefficient ++ "e" ++ show (power - toInteger (length decimals))
  -- Native Double conversion also handles exponents larger than machine Int.
  maybe (fail "invalid floating-point number") pure (readMaybe literal)
  where joinMaybe = (>>= id)

arguments :: [Text] -> Result (Maybe Options)
arguments args = case filter dayLike args of
  [first,last_] -> do
    start <- argumentDay first
    end <- argumentDay last_
    hours <- option "--gap" 4 floating
    selectedSeed <- option "--seed" 0 integer
    selectedPage <- option "--page" 1 integer
    Right (Just (Options start end (duration hours) selectedSeed selectedPage ("--json" `elem` args)))
  _ -> Right Nothing
  where
    dayLike = maybe False (const True) . match (decimal 4 4 *> char '-' *> decimal 2 2 *> char '-' *> decimal 2 2)
    option name initial parser = case elemIndex name args of
      Nothing -> Right initial
      Just index -> case drop (index + 1) args of
        value:_ -> parseArgument name parser (strip value)
        [] -> Left (MarkError ("missing value for " <> name))

-- Keep an unused interval error as a value. An empty range or one message per
-- session does not need an interval; once it is used, chunks returns the error.
duration :: Double -> Result NominalDiffTime
duration hours
  | isNaN hours || isInfinite hours = Left (MarkError "gap must be finite")
  | micros < negate (999999999 * 86400000000) || micros >= 1000000000 * 86400000000 =
      Left (MarkError "gap outside supported range")
  | otherwise = Right (fromRational (toRational micros / 1000000))
  where
    whole = truncate hours :: Integer
    micros = whole * 3600000000 + round ((hours - fromInteger whole) * 3600000000)

runArguments :: MarkEnv -> [Text] -> IO ()
runArguments env args = do
  options <- require (arguments args)
  case options of
    Nothing -> say usage
    Just opts -> do
      msgs <- loadMessages env (startDay opts) (endDay opts)
      if null msgs then say "その範囲にタイムマシンの記録がありません。" else do
        now <- markNow env
        since <- require (resolveSince now Automatic)
        let repeatOptions = defaultRepeatOptions {repeatSince = maybe AllDates FromDay since}
            fumi = filter ((== HumanSpeaker) . messageSpeaker) msgs
            claude = filter ((== Claude) . messageSpeaker) msgs
        repetitions <- require (repeats now repeatOptions claude)
        sessions <- markLoadSessions env
        blocks <- require (P.chunks (gap opts) msgs)
        results <- traverse (analyzeWithSessions env (seed opts) fumi repetitions sessions) blocks
        if asJson opts then say (toJson results) else do
          body <- require (render now results repetitions repeatOptions)
          markPage env body (page opts) >>= markOutput env
  where say text = markOutput env (text <> "\n")
