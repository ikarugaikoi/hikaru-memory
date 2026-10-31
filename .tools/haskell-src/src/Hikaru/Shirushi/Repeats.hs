{-# LANGUAGE OverloadedStrings #-}
module Hikaru.Shirushi.Repeats
  ( RepeatSince(..), RepeatOptions(..), defaultRepeatOptions, resolveSince, repeats ) where

import Control.Monad (foldM)
import Data.Char (ord)
import qualified Data.HashSet as HS
import Data.List (sortOn)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (Day, LocalTime, addDays, toGregorian)
import Hikaru.HouseDay (houseDayDate)
import Hikaru.Shirushi.Language (nameParts, squeeze, stripParentheses)
import qualified Hikaru.Shirushi.RepeatTable as Table
import Hikaru.Shirushi.Syntax (decimalChar)
import Hikaru.Shirushi.Types

data RepeatSince = Automatic | AllDates | FromDay !Day deriving (Eq, Show)
data RepeatOptions = RepeatOptions
  { repeatMinimumDays :: !Int, repeatMinimumCount :: !Int, repeatSince :: !RepeatSince }
  deriving (Eq, Show)

defaultRepeatOptions :: RepeatOptions
defaultRepeatOptions = RepeatOptions 3 4 Automatic

resolveSince :: LocalTime -> RepeatSince -> Result (Maybe Day)
resolveSince _ AllDates = Right Nothing
resolveSince _ (FromDay day) = Right (Just day)
resolveSince now Automatic = do
  day <- houseDayDate <$> hikaruDay now
  let start = addDays (-90) day
      (year, _, _) = toGregorian start
  if year < 1 then Left (MarkError "repeat since outside 1..9999") else Right (Just start)

data RepeatCount = RepeatCount !Int !(S.Set Day) !Int

repeats :: LocalTime -> RepeatOptions -> [Message] -> Result [Repetition]
repeats now options input = do
  since <- resolveSince now (repeatSince options)
  counts <- foldM (visit since) Table.empty (zip [0..] input)
  let candidates = Table.toListWhere eligible counts
      sorted = sortOn (\(g, RepeatCount n _ _) -> (negate (T.length g), negate n)) candidates
      kept = reverse (snd (foldl' keep (HS.empty, []) sorted))
  pure [Repetition g n (S.size days) | (g, RepeatCount n days _) <- kept]
  where
    visit since table (position, msg) = do
      day <- houseDayDate <$> hikaruDay (messageTime msg)
      pure $ if maybe False (day <) since then table else
        foldl' (segment position day) table (T.splitOn "\n" (stripParentheses (messageText msg)))
    segment position day table raw = foldl' accept table windows
      where
        text = squeeze raw
        size = T.length text
        windows = [T.take width suffix | width <- [5..min 16 size], suffix <- take (size - width + 1) (T.tails text)]
        accept acc g
          | T.head g `S.member` punct || T.last g `S.member` punct = acc
          | otherwise = Table.alter g (update position day) acc
    update position day Nothing = RepeatCount 1 (S.singleton day) position
    update position day (Just old@(RepeatCount n days previous))
      | previous == position = old
      | otherwise = RepeatCount (n + 1) (S.insert day days) position
    eligible g (RepeatCount n days _) = n >= repeatMinimumCount options
      && S.size days >= repeatMinimumDays options
      && not (T.all ((<128) . ord) g) && not (T.any decimalChar g)
      && not (any (`T.isInfixOf` g) nameParts)
    -- Every candidate has 5..16 characters. Membership in the substrings of
    -- already kept phrases is exactly the old infix test, without rescanning
    -- every longer phrase. Rejected phrases never contribute substrings.
    keep (covered, kept) candidate@(g, _)
      | HS.member g covered = (covered, kept)
      | otherwise = (foldl' (flip HS.insert) covered (substrings g), candidate:kept)
    substrings text = [T.take width suffix | suffix <- T.tails text, width <- [5..T.length suffix]]
    punct = S.fromList "。、，！？!?…：:；;「」『』“”\"（）()・"
