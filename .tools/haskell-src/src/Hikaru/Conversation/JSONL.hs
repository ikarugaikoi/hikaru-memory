{-# LANGUAGE OverloadedStrings #-}
-- | Compatibility error/location API; conversation syntax lives in JsonLine.
module Hikaru.Conversation.JSONL (LogError(..), rowLocation) where

import Control.Exception (Exception(..))
import Data.Text (Text)
import qualified Data.Text as T

newtype LogError = LogError Text deriving (Eq, Show)
instance Exception LogError where
  displayException (LogError text) = T.unpack text

rowLocation :: FilePath -> Integer -> Text
rowLocation path ordinal = T.pack path <> ": line " <> T.pack (show (ordinal + 1))

