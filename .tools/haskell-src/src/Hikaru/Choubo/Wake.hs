{-# LANGUAGE OverloadedStrings #-}
module Hikaru.Choubo.Wake (scanSignal, scanSignalDetailed, scanSignalOpened, signalConsumer) where

import Data.Text (Text)
import Hikaru.Mezame.Signal (SignalEvent, advanceSignal)
import Hikaru.Choubo.Consumer
import Hikaru.Choubo.ConsumerProjection
import Hikaru.Choubo.Hidden
import Hikaru.Choubo.Reader
import Hikaru.Choubo.Store
import Hikaru.Choubo.Types

scanSignal :: ScanEnv -> Text -> (Text -> IO SignalEvent) -> FilePath -> IO Bool
scanSignal env context parse path = scanState <$> scanSignalDetailed env context parse path

scanSignalDetailed :: ScanEnv -> Text -> (Text -> IO SignalEvent) -> FilePath
  -> IO (ScanResult Bool)
scanSignalDetailed env context parse path = withLogSource path (scanSignalOpened env context parse)

scanSignalOpened :: ScanEnv -> Text -> (Text -> IO SignalEvent) -> OpenedLog
  -> IO (ScanResult Bool)
scanSignalOpened env context parse source = do
  result <- scanConsumer env source (signalConsumer context parse)
  pure result {scanState=phraseOpen (scanState result)}

signalConsumer :: Text -> (Text -> IO SignalEvent) -> Consumer SignalEvent HiddenState HiddenState
signalConsumer context parse = Consumer
    { consumerName = "wake_signal"
    , consumerVersion = 6
    , consumerContext = context
    -- Keep the existing read-ahead policy; conversation boundaries are LF.
    , consumerReadPolicy = \mode -> if mode == ScanOff then ReadAhead8192DeferredCR else ReadAhead8192
    , consumerInput = parse . locatedText
    , consumerInitial = initialHidden
    , consumerStep = \state event -> Right (Continue (advanceSignal state event))
    , consumerRestore = Restored . Just
    , consumerProject = Just
    , consumerCodec = ProjectionCodec (\state -> PFields
        [("closed",PFlag (hiddenClosed state)),("tail",PFlag (hiddenTail state))])
        restore
    }
  where
    restore projection = do
      fields <- exactFields ["closed","tail"] projection
      closed <- flagField "closed" fields
      tail_ <- flagField "tail" fields
      if tail_ && not closed then Nothing else Just (HiddenState closed tail_)
