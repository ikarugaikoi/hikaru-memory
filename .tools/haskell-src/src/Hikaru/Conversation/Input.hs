{-# LANGUAGE OverloadedStrings #-}
-- | Shared JSON and timestamp boundary for conversation consumers.
module Hikaru.Conversation.Input
  ( RowError(..), decodeRow, optionalText, flag, contentText, inProject, timestamp, jst, archiveSpace, stripArchive ) where

import Control.Applicative ((<|>))
import Data.Aeson hiding (object)
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V

data RowError = BadJSON | BadRow String deriving (Eq, Show)

decodeRow :: (Value -> Parser a) -> Text -> Either RowError a
decodeRow parser raw = do
  value <- either (const (Left BadJSON)) Right (eitherDecodeStrictText raw)
  either (Left . BadRow) Right (parseEither parser value)

optionalText :: Object -> Key -> Parser (Maybe Text)
optionalText object key = fmap nonempty (object .:? key)
  where nonempty (Just text) | T.null text = Nothing
        nonempty value = value

flag :: Object -> Key -> Parser Bool
flag object key = object .:? key .!= False

contentText :: Value -> Parser Text
contentText (String text) = pure text
contentText (Array blocks) = T.concat <$> traverse block (V.toList blocks)
  where
    block (Object object) = do
      kind <- object .:? "type" :: Parser (Maybe Text)
      if kind == Just "text" then object .:! "text" .!= "" else pure ""
    block _ = pure ""
contentText _ = pure ""

inProject :: Text -> Text -> Bool
inProject identity cwd = cwd == identity || (identity <> "/") `T.isPrefixOf` cwd

jst :: TimeZone
jst = minutesToTimeZone 540

-- Logs have an explicit offset. The old Python ISO dialect is deliberately
-- not reproduced (the refactoring checklist permits narrowing that dialect).
timestamp :: Text -> Either Text UTCTime
timestamp raw = maybe (Left ("invalid timestamp: " <> raw)) Right (zulu <|> offset)
  where
    input = T.unpack raw
    zulu = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" input
    offset = zonedTimeToUTC <$> (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" input :: Maybe ZonedTime)


-- The exact Python 3 str.isspace() set (29 code points):
-- U+0009..U+000D, U+001C..U+0020, U+0085, U+00A0, U+1680,
-- U+2000..U+200A, U+2028, U+2029, U+202F, U+205F, U+3000.
-- Shared by archive edges and phrase classification, including OwnTime.
-- Interior whitespace, zero-width characters and BOM are never removed.
-- Splitting body lines remains literal LF.
archiveSpace :: Char -> Bool
archiveSpace c = (c >= '\x0009' && c <= '\x000d')
  || (c >= '\x001c' && c <= '\x0020')
  || c == '\x0085' || c == '\x00a0' || c == '\x1680'
  || (c >= '\x2000' && c <= '\x200a')
  || c == '\x2028' || c == '\x2029' || c == '\x202f'
  || c == '\x205f' || c == '\x3000'

stripArchive :: Text -> Text
stripArchive = T.dropAround archiveSpace
