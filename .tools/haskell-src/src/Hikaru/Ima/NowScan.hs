{-# LANGUAGE OverloadedStrings #-}
-- | The SessionStart fold, shared by the full and incremental readers.
module Hikaru.Ima.NowScan
  ( NowSummary(..), initialNow, stepNow, nowResult, isTask, parseNowLine, advanceNow ) where

import Control.Exception (throw)
import Data.Text (Text)
import qualified Data.Text as T
import Hikaru.Conversation.Input (RowError(..))
import Hikaru.Conversation.Line
import Hikaru.Conversation.Types (Speaker(HumanSpeaker))
import Hikaru.Mezame.Input (InputError(..))

-- Force each fold result so the latest timestamp does not retain whole turns.
data NowSummary = NowSummary !(Maybe Bool) !(Maybe Text)
  deriving (Eq, Show)

initialNow :: NowSummary
initialNow = NowSummary Nothing Nothing

nowResult :: NowSummary -> (Maybe Bool, Maybe Text)
nowResult (NowSummary first latest) = (first, latest)

isTask :: Text -> Bool
isTask = speechIsTask . Typed

stepNow :: Text -> NowSummary -> Text -> NowSummary
stepNow identity previous raw = either (throw . InputError) (advanceNow previous) (parseNowLine identity raw)

parseNowLine :: Text -> Text -> Either Text Line
parseNowLine identity raw = case parseLine identity raw of
  Left BadJSON -> Left "invalid JSON conversation row"
  Left (BadRow reason) -> Left (T.pack reason)
  Right line -> Right line

advanceNow :: NowSummary -> Line -> NowSummary
advanceNow previous@(NowSummary first latest) line = case line of
  Said HumanSpeaker turn -> NowSummary
    (case first of Nothing -> Just (speechIsTask (turnSpeech turn)); Just _ -> first)
    (max latest (Just (turnStamp turn)))
  _ -> previous
