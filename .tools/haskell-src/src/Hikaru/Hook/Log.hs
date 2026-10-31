{-# LANGUAGE OverloadedStrings #-}
-- | Add source locations to typed projection errors. scanConsumer supplies
-- only complete JSON rows, validated once by Choubo.Reader before this layer.
module Hikaru.Hook.Log (checkedConsumer) where

import Control.Exception (catch, throwIO)
import Hikaru.Choubo.Consumer
import Hikaru.Choubo.Types
import Hikaru.Conversation.JSONL
import Hikaru.Mezame.Input (InputError(..))

checkedConsumer :: FilePath -> Consumer input state checkpoint
                -> Consumer (Maybe (LocatedLine, input)) state checkpoint
checkedConsumer path consumer = consumer
  { consumerVersion = consumerVersion consumer + 1
  , consumerInput = \line -> do
      input <- consumerInput consumer line `catch` \(InputError reason) ->
        throwIO (LogError ("ValueError: " <> location line <> ": " <> reason))
      pure (Just (line, input))
  , consumerStep = \state input -> case input of
      Nothing -> Right (Continue state)
      Just (line, value) -> case consumerStep consumer state value of
        Left reason -> Left (location line <> ": " <> reason)
        Right next -> Right next
  }
  where location line = rowLocation path (rowOrdinal (locatedSpan line))
