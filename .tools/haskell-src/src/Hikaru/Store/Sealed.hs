{-# LANGUAGE OverloadedStrings #-}
-- | Storage mechanics only. Business callers own backup, lock and version
-- policy; like the baseline, writing a sealed file alone does not take a lock.
-- A record file only ever receives a 'Sealed' value (see "Hikaru.Sealed").
module Hikaru.Store.Sealed (readSealed, writeSealed, sealText) where

import Control.Exception (bracketOnError, finally, throwIO)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Hikaru.Compat.Base64 (base64ErrorMessage)
import Hikaru.Sealed
import System.Directory (createDirectoryIfMissing, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hClose, openBinaryTempFile)
import System.IO.Error (catchIOError, isDoesNotExistError)

readSealed :: FilePath -> IO Text
readSealed path = do
  bytes <- BS.readFile path
  case unsealBytes bytes of
    Right (Plain text) -> pure text
    Left (BadBase64 err) -> ioError (userError (base64ErrorMessage err))
    Left (BadUtf8 err) -> throwIO err

-- | Seal before writing. The round-trip check names the target file, as the
-- baseline's single write function did.
sealText :: FilePath -> Text -> IO Sealed
sealText path text = maybe (ioError (userError ("封の確認に失敗: " ++ takeFileName path))) pure
  (sealPlain (Plain text))

writeSealed :: FilePath -> Sealed -> IO ()
writeSealed path sealed = do
  let parent = takeDirectory path
  createDirectoryIfMissing True parent
  bracketOnError (openBinaryTempFile parent ".tmp-.b64") cleanup $ \(temporary, handle) ->
    (do BS.hPut handle (sealedBytes sealed)
        hClose handle
        renameFile temporary path) `finally` removeTemporary temporary
  where
    removeTemporary temporary = removeFile temporary `catchIOError` \err ->
      if isDoesNotExistError err then pure () else ioError err
    cleanup (temporary, handle) = hClose handle `finally` removeTemporary temporary
