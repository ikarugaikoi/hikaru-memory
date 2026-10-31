{-# LANGUAGE OverloadedStrings #-}
-- | Only the diary/shiori checkpoints used by wake enter the display layer.
-- Known checkpoint fields are typed on entry, before any display or write.
module Hikaru.Mezame.Progress (Progress, readProgress, movedDays) where

import Control.Monad (filterM)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Data.Scientific (Scientific)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addDays, toGregorian)
import Hikaru.Conversation.DateTime (DateTime, compareDateTime)
import Hikaru.Mezame.Input (parseDay, parseStamp)
import System.IO.Error (catchIOError, isDoesNotExistError)

data Checkpoint = Checkpoint !Scientific !DateTime
type Table = M.Map Text (Maybe Checkpoint)
data Progress = Progress Table Table

instance FromJSON Progress where
  parseJSON (Object fields) = Progress <$> table "diary" <*> table "shiori"
    where
      table key = case KM.lookup key fields of
        Nothing -> pure M.empty
        Just Null -> pure M.empty
        Just (Object dates) -> M.fromList <$> traverse (entry key) (KM.toList dates)
        Just _ -> fail (T.unpack (Key.toText key) ++ " is not an object")
      entry tableName (key, value) = (,) (Key.toText key) <$> checkpoint
        (Key.toText tableName <> "[" <> T.pack (show (Key.toText key)) <> "]") (tableName == "shiori") value
  parseJSON _ = fail "progress is not an object"

checkpoint :: Text -> Bool -> Value -> Parser (Maybe Checkpoint)
checkpoint _ _ Null = pure Nothing
checkpoint _ _ (Object fields) | KM.null fields = pure Nothing
checkpoint label shiori (Object fields) = do
    count <- case KM.lookup "n" fields of
      Nothing | shiori -> pure 0
              | otherwise -> fail (T.unpack (label <> ".n is missing"))
      Just n -> countValue n
    raw <- case KM.lookup "until" fields of
      Just (String stamp) -> pure stamp
      Nothing -> fail (T.unpack (label <> ".until is missing"))
      Just _ -> fail (T.unpack (label <> ".until is not a string"))
    stamp <- either (const (fail (T.unpack (label <> ".until is not a valid timestamp: " <> raw)))) pure (parseStamp raw)
    pure (Just (Checkpoint count stamp))
  where
    countValue (Bool yes) = pure (if yes then 1 else 0)
    countValue (Number number) = pure number
    countValue _ = fail (T.unpack (label <> ".n is not a number"))
checkpoint label _ _ = fail (T.unpack (label <> " is not an object"))

readProgress :: FilePath -> IO (Either Text Progress)
readProgress path = do
  body <- B.readFile path `catchIOError` \err ->
    if isDoesNotExistError err then pure "{}" else ioError err
  pure $ case eitherDecodeStrict' body :: Either String Value of
    Left reason -> Left ("JSONDecodeError: invalid progress file: " <> T.pack path <> ": " <> T.pack reason)
    Right value -> either (Left . (("ValueError: invalid progress file: " <> T.pack path <> ": ") <>) . T.pack)
      Right (parseEither parseJSON value)

movedDays :: Text -> Progress -> Either Text (S.Set (Int, Int))
movedDays first (Progress diary shiori) = do
  monday <- either (const (Left ("ValueError: invalid diary date: " <> first))) Right (parseDay first)
  days <- filterM (moved diary shiori) [addDays n monday | n <- [0..6]]
  pure (S.fromList [(m, d) | day <- days, let (_, m, d) = toGregorian day])
  where
    moved current copied day = do
      let (year, _, _) = toGregorian day
          key = T.pack (show day)
          lookupDay table = M.findWithDefault Nothing key table
          before = lookupDay current
          after = lookupDay copied
      if year < 1 || year > 9999 then Left "ValueError: diary day outside 1..9999" else pure ()
      case (before, after) of
        (Just (Checkpoint oldCount older), Just (Checkpoint newCount newer)) ->
          if newCount < oldCount then pure False else
            either (const (Left "TypeError: can't compare offset-naive and offset-aware datetimes"))
              (Right . (/= LT)) (compareDateTime newer older)
        _ -> pure False
