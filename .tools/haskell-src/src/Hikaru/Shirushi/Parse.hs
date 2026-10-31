{-# LANGUAGE OverloadedStrings #-}
-- | Day-file grammar, then chronological grouping. Parsing never reads files.
module Hikaru.Shirushi.Parse (parse, orderLoaded, before, chunks) where

import Control.Applicative ((<|>), optional)
import Control.Monad (foldM)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Hikaru.HouseDay (houseDayEntry, houseDayNamed)
import Hikaru.Shirushi.Syntax
import Hikaru.Shirushi.Types
import Text.Megaparsec (choice, count, satisfy, takeWhile1P, takeWhileP, try)
import Text.Megaparsec.Char (char, string)

data Heading = DayHead !Integer !Int !Int !Bool
             | SpeakerHead !Speaker !Int !Int !Int !(Maybe Text)

heading :: Parser Heading
heading = try dayHeading <|> speakerHeading
  where
    dayHeading = do
      _ <- string "# "
      year <- decimal 4 4 <* char '年'
      month <- decimal 1 2 <* char '月'
      day <- decimal 1 2 <* char '日' <* spaces
      note <- optional (char '（' *> takeWhileP Nothing (/= '）') <* char '）')
      spaces
      pure (DayHead (toInteger year) month day (maybe False nextDay note))
    nextDay = not . null . snd . scan (string "5時" *> choice
      [try (count n (satisfy (/= '\n')) *> char '翌') | n <- [0..2]])
    speakerHeading = do
      who <- HumanSpeaker <$ string "**ふ：**（" <|> Claude <$ string "**Claude：**（"
      hour <- decimal 2 2 <* char ':'
      minute <- decimal 2 2
      second <- fromMaybe 0 <$> optional (char ':' *> decimal 2 2)
      session <- optional (char '・' *> takeWhile1P (Just "session")
        (\c -> c /= '）' && c /= '・' && not (whiteSpace c)))
      _ <- char '）'
      spaces
      pure (SpeakerHead who hour minute second session)

unescape :: Text -> Text
unescape text = case T.uncons text of
  Just ('\\', rest)
    | let start = T.dropWhile (== '\\') rest
    , "**ふ：**（" `T.isPrefixOf` start || "**Claude：**（" `T.isPrefixOf` start
      || maybe False (const True) (match (string "# " *> decimal 4 4 *> takeWhileP Nothing (const True)) start)
    -> rest
  _ -> text

parse :: Text -> Maybe Day -> Result [Message]
parse input initial = go [] [] initial False Nothing (T.splitOn "\n" input)
  where
    flush out _ Nothing = out
    flush out buffer (Just msg) = msg {messageText = strip (T.intercalate "\n" (reverse buffer))} : out
    go out buffer _ _ active [] = Right (reverse (flush out buffer active))
    go out buffer current tomorrow active (line:rest) = case match heading line of
      Just (DayHead year month day next) -> do
        valid <- maybe (Left (MarkError "invalid day-file date")) Right
          (if year == 0 then Nothing else fromGregorianValid year month day)
        go (flush out buffer active) [] (Just valid) next Nothing rest
      Just (SpeakerHead who hour minute second session) | Just day <- current -> do
        clock <- maybe (Left (MarkError "invalid day-file time")) Right
          (makeTimeOfDayValid hour minute (fromIntegral second))
        -- makeTimeOfDayValid admits leap seconds; these headings never did.
        if second > 59 then Left (MarkError "invalid day-file second") else pure ()
        date <- if tomorrow then maybe (Left (MarkError "day-file date outside 1..9999")) Right
                  (houseDayEntry (houseDayNamed day) hour) else Right day
        let msg = Message (LocalTime date clock) who "" session Nothing
        go (flush out buffer active) [] current tomorrow (Just msg) rest
      _ -> go out (case active of Nothing -> buffer; Just _ -> unescape line:buffer) current tomorrow active rest

orderLoaded :: [Message] -> [Message]
orderLoaded = sortOn messageTime . zipWith (\index msg -> msg {messageSequence = Just index}) [0..]

before :: Message -> Message -> Bool
before a b = case compare (messageTime a) (messageTime b) of
  LT -> True
  GT -> False
  EQ -> case (messageSequence a, messageSequence b) of
    (Just x, Just y) -> x < y
    _ -> True

-- Interval construction errors matter only when a session has two messages
-- to compare. The parsed duration/error is shared, never parsed a second time.
chunks :: Result NominalDiffTime -> [Message] -> Result [NonEmpty Message]
chunks gap input = sortOn (key . NE.head) . concat <$> traverse (divide . sortOn key . reverse . snd) groups
  where
    groups = sortOn fst . M.elems $ foldl' insert M.empty (zip [0 :: Int ..] input)
    key m = (messageTime m, fromMaybe 0 (messageSequence m))
    insert table (index, msg) = M.alter
      (Just . maybe (index, [msg]) (\(first, ms) -> (first, msg:ms))) (messageSession msg) table
    divide [] = Right []
    divide (first:rest) = NE.toList . fmap NE.reverse . NE.reverse <$> foldM step ((first :| []) :| []) rest
    step (current@(previous :| _) :| done) msg = do
      duration <- gap
      pure $ if diffLocalTime (messageTime msg) (messageTime previous) >= duration
        then (msg :| []) :| (current:done)
        else (msg NE.<| current) :| done
