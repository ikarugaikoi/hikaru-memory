{-# LANGUAGE OverloadedStrings #-}
-- | Decode a record at its input boundary; retain the file in the diagnostic.
module Hikaru.RecordRead (readRecord) where

import Control.Exception (throwIO)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import Hikaru.Mezame.Input (InputError(..))
import Hikaru.Sealed (Plain(..), SealError(..), unsealBytes)

readRecord :: FilePath -> IO Text
readRecord path = do
  bytes <- B.readFile path
  case unsealBytes bytes of
    Right (Plain text) -> pure text
    Left (BadBase64 reason) -> throwIO (InputError
      ("binascii.Error: invalid base64 in sealed file: " <> T.pack path <> ": " <> T.pack (show reason)))
    Left (BadUtf8 _) -> throwIO (InputError
      ("UnicodeDecodeError: invalid UTF-8 in sealed file: " <> T.pack path))
