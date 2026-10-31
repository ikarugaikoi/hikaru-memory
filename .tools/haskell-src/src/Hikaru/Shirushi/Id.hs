{-# LANGUAGE OverloadedStrings #-}
-- | New IDs can only be made after resolving the short session name. The
-- JSON instance imports existing progress IDs verbatim: it adds no validation
-- to historical progress or to headings that seal read has already accepted.
module Hikaru.Shirushi.Id
  ( BlockId, blockId, blockIdText, FullSessionId, fullSessionText
  , Sessions, emptySessions, resolveSession, decodeSessions
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (LocalTime)
import Hikaru.Shirushi.Syntax (stamp)

newtype BlockId = BlockId Text deriving (Eq, Ord, Show)
newtype FullSessionId = FullSessionId (Maybe Text) deriving (Eq, Show)
newtype Sessions = Sessions (M.Map Text (Maybe Text)) deriving (Eq, Show)

instance FromJSON BlockId where
  parseJSON = withText "BlockId" (pure . BlockId)
instance ToJSON BlockId where
  toJSON = toJSON . blockIdText
  toEncoding = toEncoding . blockIdText

emptySessions :: Sessions
emptySessions = Sessions M.empty

instance FromJSON Sessions where
  parseJSON Null = pure emptySessions
  parseJSON value = Sessions <$> parseJSON value

decodeSessions :: BL.ByteString -> Either String Sessions
decodeSessions = eitherDecode

resolveSession :: Sessions -> Maybe Text -> FullSessionId
resolveSession (Sessions table) short = FullSessionId $ case short of
  Nothing -> Nothing
  Just name -> M.findWithDefault (Just name) name table

fullSessionText :: FullSessionId -> Maybe Text
fullSessionText (FullSessionId text) = text

blockId :: FullSessionId -> LocalTime -> BlockId
blockId (FullSessionId session) start = BlockId (prefix <> ":" <> stamp start)
  where prefix = case session of
          Just text | not (T.null text) -> text
          _ -> "----"

blockIdText :: BlockId -> Text
blockIdText (BlockId text) = text
