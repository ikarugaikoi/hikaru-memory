{-# LANGUAGE OverloadedStrings #-}
-- | Speaker is shared by all consumers. Message/Picture retain hikaru-owntime's
-- existing interface; Conversation.Parse adapts the single Line parser to it.
-- Collect and now consume Conversation.Line directly, without these JSON values.
module Hikaru.Conversation.Types
  ( Message(..), Speaker(..), speakerName, speakerText, Picture(..), TimeRange, MarkedMessage(..) ) where

import Data.Text (Text)
import Hikaru.Compat.Json (PyString, Value, pyStringFromText)
import Hikaru.Conversation.DateTime (DateTime)

data Picture = Picture
  { pictureName :: !PyString
  , pictureMediaType :: !Value
  , pictureData :: !Value
  } deriving Show

-- | だれが言ったか。記録の中では「ふ」と「Claude」の二つだけ（2026-10-22、
-- 重构清单の第 4 步。それまでは、この二つの文字列で持っていた）。
data Speaker = HumanSpeaker | Claude
  deriving (Eq, Ord, Show)

-- | 原文の見出し（**ふ：** / **Claude：**）や重なりを見る鍵に出す名前。
speakerName :: Speaker -> PyString
speakerName = pyStringFromText . speakerText

speakerText :: Speaker -> Text
speakerText HumanSpeaker = "ふ"
speakerText Claude = "Claude"

data Message = Message
  { messageTime :: !DateTime
  , messageRawTime :: !Value
  , messageSpeaker :: !Speaker
  , messageText :: !PyString
  , messageTyped :: !Bool
  , messagePictures :: ![Picture]
  , messageUUID :: !Value
  , messageSessionID :: !Value
  } deriving Show

type TimeRange = (DateTime, Maybe DateTime)

data MarkedMessage = MarkedMessage
  { markedMessage :: !Message
  , messageHidden :: !Bool
  } deriving Show
