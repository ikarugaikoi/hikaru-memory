{-# LANGUAGE OverloadedStrings #-}
-- | Compatibility boundary for hikaru-owntime's existing Message interface.
-- Admission and text construction belong solely to Conversation.Line. Collect
-- and now use that parser directly; neither travels through Python values.
module Hikaru.Conversation.Parse (parseLine, markHidden, startPhrase, endPhrase) where

import Control.Monad (foldM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (LocalTime)
import Hikaru.Compat.Json
import Hikaru.Compat.Render (dumpJson)
import Hikaru.Conversation.DateTime
import Hikaru.Conversation.Hidden
import qualified Hikaru.Conversation.Line as Line
import Hikaru.Conversation.Types

parseLine :: Monad m
  => (DateTime -> m (Either Text LocalTime)) -> PyString -> Value
  -> m (Either Text (Maybe Message))
parseLine _ identity doc = pure $ do
  project <- pyStringToText identity
  raw <- pyStringToText (dumpJson False Nothing False doc)
  line <- either (Left . fromStringError) Right (Line.parseLine project raw)
  case line of
    Line.Said _ turn -> do
      utterance <- Line.recordedTurn turn
      let text = pyStringFromText
          optional = maybe Null (String . text)
      time <- parseDateTimeText (Line.utteranceRawTime utterance)
      pure $ if T.null (Line.utteranceText utterance) then Nothing else Just Message
        { messageTime = time
        , messageRawTime = String (text (Line.utteranceRawTime utterance))
        , messageSpeaker = Line.utteranceSpeaker utterance
        , messageText = text (Line.utteranceText utterance)
        , messageTyped = Line.utteranceTyped utterance
        , messagePictures =
            [Picture (text (Line.pictureName p)) (optional (Line.pictureMedia p)) (String (text (Line.pictureData p)))
            | p <- Line.utterancePictures utterance]
        , messageUUID = optional (Line.utteranceUUID utterance)
        , messageSessionID = optional (Line.utteranceSession utterance) }
    _ -> pure Nothing
  where fromStringError = T.pack . show

markHidden :: [Message] -> [TimeRange] -> Either Text ([MarkedMessage], Bool)
markHidden messages ranges = do
  (out,state) <- foldM visit ([],initialHidden) messages
  pure (reverse out,phraseOpen state)
  where
    visit (out,state) message = do
      let state' = advanceHidden state (classifyHidden message)
      excluded <- if hiddenByPhrase state' then Right True else inRanges (messageTime message) ranges
      pure (MarkedMessage message excluded : out,state')
    inRanges _ [] = Right False
    inRanges stamp ((a,b):rest) = do
      begins <- compareDateTime a stamp
      if begins == GT then inRanges stamp rest else do
        ends <- case b of Nothing -> Right True; Just end -> (/= GT) <$> compareDateTime stamp end
        if ends then Right True else inRanges stamp rest
