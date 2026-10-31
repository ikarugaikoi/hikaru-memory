{-# LANGUAGE OverloadedStrings #-}
-- | The single admission boundary. A turn retains an invalid timestamp so
-- now can still deliver the clock; collect requires 'recordedTurn' and stops.
-- No JSON values escape this module and no turn grants archive rights.
module Hikaru.Conversation.Line
  ( Line(..), Turn, Speech(..), turnStamp, turnSpeech, recordedTurn
  , Utterance(..), LogPicture(..), ToolCall(..), ToolResult(..)
  , parseLine, speechIsTask, speakerText
  ) where

import Data.Aeson hiding (object)
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Decoding.Text as D
import qualified Data.Aeson.Decoding.Tokens as D
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Time
import Hikaru.Choubo.JsonLine (conversationText)
import Hikaru.Conversation.Input
import Hikaru.Conversation.Types (Speaker(..), speakerText)
import Hikaru.HouseDay (houseDayFromJST)

data Speech = Typed Text | Answers | ImageOnly deriving (Eq, Show)
-- The recorded projection is lazy: now only needs admission and the raw stamp.
data Turn = Turn Text Speech (Either Text Utterance) deriving Show

data LogPicture = LogPicture
  { pictureName :: !Text, pictureMedia :: !(Maybe Text), pictureData :: !Text
  } deriving (Eq, Show)
data Utterance = Utterance
  { utteranceTime :: !UTCTime
  , utteranceRawTime :: !Text
  , utteranceSpeaker :: !Speaker
  , utteranceText :: !Text
  , utteranceTyped :: !Bool
  , utterancePictures :: ![LogPicture]
  , utteranceUUID :: !(Maybe Text)
  , utteranceSession :: !(Maybe Text)
  } deriving (Eq, Show)
data ToolCall = ToolCall (Maybe Text) (Maybe Text) deriving (Eq, Show)
data ToolResult = ToolResult (Maybe Text) Bool deriving (Eq, Show)
data Line = Said Speaker Turn | Ran (NonEmpty ToolCall) | Got (NonEmpty ToolResult)
          | Compacted | Other deriving Show

turnStamp :: Turn -> Text
turnStamp (Turn stamp _ _) = stamp
turnSpeech :: Turn -> Speech
turnSpeech (Turn _ speech _) = speech
recordedTurn :: Turn -> Either Text Utterance
recordedTurn (Turn _ _ message) = message

speechIsTask :: Speech -> Bool
speechIsTask (Typed body) = any (`T.isPrefixOf` T.stripStart body)
  ["あなたは Claude です", "<scheduled-task"]
speechIsTask _ = False

parseLine :: Text -> Text -> Either RowError Line
parseLine identity raw =
  let json = conversationText raw
  in decodeRow (classify identity json) json

classify :: Text -> Text -> Value -> Parser Line
classify identity raw (Object object) = do
  side <- flag object "isSidechain"
  stamp <- optionalText object "timestamp"
  cwd <- object .:? "cwd" .!= ""
  if side || not (inProject identity cwd) then pure Other else case stamp of
    Nothing -> pure Other
    Just time -> do
      kind <- object .:? "type" :: Parser (Maybe Text)
      case kind of
        Just "user" -> userBody raw object time
        Just "attachment" -> attachmentBody object time
        Just "assistant" -> do
          apiError <- flag object "isApiErrorMessage"
          if apiError then pure Other else do
            content <- messageContent object
            body <- T.strip <$> contentText content
            if T.null body then pure (toolCalls content)
              else said object time Claude (Typed body) (Right body) False [] 0
        Just "system" -> do
          subtype <- object .:? "subtype" :: Parser (Maybe Text)
          pure (if subtype == Just "compact_boundary" then Compacted else Other)
        _ -> pure Other
classify _ _ _ = pure Other

human :: Value -> Bool
human (Object origin) = KM.lookup "kind" origin == Just (String "human")
human _ = False

messageContent :: Object -> Parser Value
messageContent object = do
  message <- object .:! "message" .!= mempty
  message .:? "content" .!= Null

userBody :: Text -> Object -> Text -> Parser Line
userBody raw object time = do
  result <- object .:? "toolUseResult" .!= Null
  let answerObject = case result of
        Object values -> case KM.lookup "answers" values of
          Just (Object answers) | not (KM.null answers) -> Just answers
          _ -> Nothing
        _ -> Nothing
  case answerObject of
    Just answers ->
      let rendered = do
            keys <- either (Left . T.pack) Right (answerOrder raw)
            parts <- either (Left . T.pack) Right (parseEither
              (\_ -> traverse (\key -> do
                answer <- answers .: K.fromText key
                pure ("【質問への答え】" <> key <> "\n" <> answer)) keys) ())
            pure (T.strip (T.intercalate "\n\n" parts))
      in said object time HumanSpeaker Answers rendered False [] 0
    Nothing -> do
      meta <- flag object "isMeta"
      compact <- flag object "isCompactSummary"
      origin <- object .:? "origin" .!= Null
      if compact then pure Compacted
      else if meta || not (human origin) then pure (toolResults (ignoredContent object))
      else do
        content <- messageContent object
        humanContent object time content

-- Ordinary user messages and queued prompts carry the same content blocks.
-- Keep image admission, ordering and placeholders at this single boundary.
humanContent :: Object -> Text -> Value -> Parser Line
humanContent object time content = do
  body <- T.strip <$> contentText content
  images <- case content of
    Array imageBlocks -> concat <$> traverse image (V.toList imageBlocks)
    _ -> pure []
  paste <- object .:? "imagePasteIds" .!= ([] :: [Value])
  if T.null body && null images && null paste then
    either (fail . T.unpack) (const (pure Other)) (timestamp time)
    else said object time HumanSpeaker (if T.null body then ImageOnly else Typed body)
           (Right body) True images (length paste)
  where
    image (Object block) = do
      kind <- block .:? "type" :: Parser (Maybe Text)
      if kind /= Just "image" then pure [] else do
        source <- block .:? "source" .!= mempty
        encoding <- source .:? "type" :: Parser (Maybe Text)
        if encoding /= Just "base64" then pure [] else do
          payload <- optionalText source "data"
          case payload of
            Nothing -> pure []
            Just bytes -> do
              let media = either (Left . T.pack) Right (parseEither (.:? "media_type") source)
              pure [(media,bytes)]
    image _ = pure []

attachmentBody :: Object -> Text -> Parser Line
attachmentBody object time = do
  attachment <- object .:? "attachment" .!= mempty
  kind <- attachment .:? "type" :: Parser (Maybe Text)
  if kind /= Just "queued_command" then pure Other else do
    humanTurn <- flag attachment "humanTurn"
    origin <- attachment .:? "origin" .!= Null
    mode <- attachment .:? "commandMode" :: Parser (Maybe Text)
    if not humanTurn || not (human origin) || mode `notElem` [Nothing, Just "prompt"]
      then pure Other else do
        prompt <- attachment .:? "prompt" .!= Null
        humanContent object time prompt

said :: Object -> Text -> Speaker -> Speech -> Either Text Text -> Bool -> [(Either Text (Maybe Text),Text)] -> Int -> Parser Line
said object raw speaker speech bodyResult typed images pasted =
  let recorded = do
        time <- timestamp raw
        body <- stripArchive <$> bodyResult
        uuid <- either (Left . T.pack) Right (parseEither (`optionalText` "uuid") object)
        sid <- either (Left . T.pack) Right (parseEither (.:? "sessionId") object)
        let local = utcToLocalTime jst time
        pics <- case images of
          [] -> Right []
          _ -> do
            day <- maybe (Left "hikaru day outside datetime year range") Right (houseDayFromJST local)
            let tag = maybe "0000" (T.take 4) uuid
                name n media = T.pack (show day <> "/" <> formatTime defaultTimeLocale "%H%M%S" local)
                  <> "_" <> tag <> "_" <> T.pack (show n) <> "." <> extension media
            traverse (\(n,(parsedMedia,bytes)) -> do
              media <- parsedMedia
              pure (LogPicture (name n media) media bytes)) (zip [1::Int ..] images)
        let text | not (null pics) = stripArchive (body <> "\n\n" <> T.intercalate "\n" ["【画像: " <> pictureName p <> "】" | p <- pics])
                 | pasted /= 0 = stripArchive (body <> "\n\n【画像" <> T.pack (show pasted) <> "枚（記録に残っていない）】")
                 | otherwise = body
        pure (Utterance time raw speaker text typed pics uuid sid)
  in pure (Said speaker (Turn raw speech recorded))
  where
    extension (Just "image/png") = "png"
    extension (Just "image/jpeg") = "jpg"
    extension (Just "image/webp") = "webp"
    extension (Just "image/gif") = "gif"
    extension _ = "bin"

-- Tool-only rows were ignored by both consumers. Classify their metadata
-- without imposing new validation on their unused payloads.
ignoredContent :: Object -> Value
ignoredContent object = case KM.lookup "message" object of
  Just (Object message) -> maybe Null id (KM.lookup "content" message)
  _ -> Null
fieldText :: Key -> Object -> Maybe Text
fieldText key object = case KM.lookup key object of
  Just (String text) -> Just text
  _ -> Nothing
toolCalls :: Value -> Line
toolCalls content = maybe Other Ran (nonEmpty
  [ToolCall (fieldText "id" o) (fieldText "name" o) | o <- blocks "tool_use" content])
toolResults :: Value -> Line
toolResults content = maybe Other Got (nonEmpty
  [ToolResult (fieldText "tool_use_id" o) (KM.lookup "is_error" o == Just (Bool True)) | o <- blocks "tool_result" content])
blocks :: Text -> Value -> [Object]
blocks kind (Array values) = [o | Object o <- V.toList values, KM.lookup "type" o == Just (String kind)]
blocks _ _ = []

-- Only called for a nonempty answers object, after Aeson validated the JSON.
-- Aeson tokens retain field order without a second generic JSON value model.
answerOrder :: Text -> Either String [Text]
answerOrder raw = fst <$> walk [] (D.textToTokens raw)
  where
    walk :: [Text] -> D.Tokens k String -> Either String ([Text], k)
    walk _ (D.TkLit _ k) = Right ([],k)
    walk _ (D.TkText _ k) = Right ([],k)
    walk _ (D.TkNumber _ k) = Right ([],k)
    walk path (D.TkArrayOpen items) = array path items
    walk path (D.TkRecordOpen fields) = record path fields
    walk _ (D.TkErr e) = Left e
    array path (D.TkItem item) = do
      (a,next) <- walk path item
      (b,k) <- array path next
      pure (a++b,k)
    array _ (D.TkArrayEnd k) = Right ([],k)
    array _ (D.TkArrayErr e) = Left e
    record path (D.TkPair key item) = do
      let name = K.toText key
      (a,next) <- walk (path++[name]) item
      (b,k) <- record path next
      pure ((if path == ["toolUseResult","answers"] then [name] else a)++b,k)
    record _ (D.TkRecordEnd k) = Right ([],k)
    record _ (D.TkRecordErr e) = Left e
