{-# LANGUAGE OverloadedStrings #-}
-- | Project the shared conversation boundary into a text-free privacy event.
-- Admission, content blocks and full-phrase semantics are the same as Collect.
module Hikaru.Mezame.Signal (SignalEvent, parseSignalLine, advanceSignal) where

import Control.Exception (throwIO)
import Data.Text (Text)
import qualified Data.Text as T
import Hikaru.Conversation.Input (RowError(..))
import Hikaru.Conversation.Line (Line(..), parseLine, recordedTurn)
import Hikaru.Conversation.Hidden (HiddenClass, HiddenState, advanceHidden, classifyUtterance)
import Hikaru.Mezame.Input (InputError(..))

data SignalEvent = Ignored | Heard !HiddenClass

advanceSignal :: HiddenState -> SignalEvent -> HiddenState
advanceSignal state Ignored = state
advanceSignal state (Heard classification) = advanceHidden state classification

parseSignalLine :: Text -> Text -> IO SignalEvent
parseSignalLine identity line = case parseLine identity line of
  Left BadJSON -> throwIO (InputError "invalid JSON conversation row")
  Left (BadRow reason) -> throwIO (InputError (T.pack reason))
  Right (Said _ turn) -> either (throwIO . InputError)
    (pure . Heard . classifyUtterance) (recordedTurn turn)
  Right _ -> pure Ignored
