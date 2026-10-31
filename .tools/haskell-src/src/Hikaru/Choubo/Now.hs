{-# LANGUAGE OverloadedStrings #-}
module Hikaru.Choubo.Now (scanNowCached, scanNowOpened, nowConsumer, NowCheckpoint) where

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import System.IO.Error (catchIOError, isDoesNotExistError)
import Hikaru.Ima.NowScan
import Hikaru.Choubo.Consumer
import Hikaru.Choubo.Reader
import Hikaru.Choubo.Store
import Hikaru.Choubo.Types

scanNowCached :: ScanEnv -> Text -> FilePath -> IO (Maybe Bool, Maybe Text)
scanNowCached env identity path =
  withLogSource path (fmap scanState . scanNowOpened env identity) `catchIOError` \err ->
    if isDoesNotExistError err then pure (Nothing, Nothing) else ioError err

scanNowOpened :: ScanEnv -> Text -> OpenedLog -> IO (ScanResult (Maybe Bool, Maybe Text))
scanNowOpened env identity source = do
  context <- contextDigest ["now-typed-v2", TE.encodeUtf8 identity]
  result <- scanConsumer env source (nowConsumer identity context)
  pure result {scanState = nowResult (scanState result)}

-- An unchecked timestamp can affect this invocation without being persisted.
newtype NowCheckpoint = NowCheckpoint NowSummary

nowConsumer :: Text -> Text -> Consumer Text NowSummary NowCheckpoint
nowConsumer identity context = Consumer
  { consumerName = "now", consumerVersion = 7, consumerContext = context
  , consumerReadPolicy = const Line64K, consumerInput = pure . locatedText
  , consumerInitial = initialNow
  , consumerStep = \state line -> Right (Continue (stepNow identity state line))
  , consumerProject = project
  , consumerRestore = \(NowCheckpoint state) -> Restored (Just state)
  , consumerCodec = ProjectionCodec encodeCheckpoint (fmap NowCheckpoint . restore) }

project :: NowSummary -> Maybe NowCheckpoint
project state@(NowSummary _ latest)
  | maybe True safeTime latest = Just (NowCheckpoint state)
  | otherwise = Nothing

encodeCheckpoint :: NowCheckpoint -> Projection
encodeCheckpoint (NowCheckpoint (NowSummary first latest)) = PFields
  [("first", PList (maybe [] (pure . PFlag) first))
  ,("latest", PList (maybe [] (pure . PTime) latest))]

restore :: Projection -> Maybe NowSummary
restore (PFields fields) = do
  first <- case lookup "first" fields of
    Just (PList []) -> Just Nothing
    Just (PList [PFlag value]) -> Just (Just value)
    _ -> Nothing
  latest <- case lookup "latest" fields of
    Just (PList []) -> Just Nothing
    Just (PList [PTime value]) | safeTime value -> Just (Just value)
    _ -> Nothing
  pure (NowSummary first latest)
restore _ = Nothing
