{-# LANGUAGE OverloadedStrings #-}
-- | A published directory contains one complete original and its receipt.
-- The receipt has a path identity and byte count, never a message/body index.
-- Callers hold the record lock across backup and record replacement.
module Hikaru.Store.Backup (saveOriginal, backupDirectory, readOriginal) where

import Control.Exception (bracketOnError)
import Control.Monad (unless, when)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hikaru.HouseDay (HouseDay)
import System.Directory (canonicalizePath, createDirectoryIfMissing, removeDirectoryRecursive, renameDirectory)
import System.FilePath ((</>), isRelative, makeRelative, splitDirectories, takeFileName)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.Files (getSymbolicLinkStatus)
import System.Posix.Temp (mkdtemp)

data Receipt = Receipt T.Text Integer

instance A.ToJSON Receipt where
  toJSON (Receipt identity size) = A.object
    ["version" A..= (1 :: Int), "identity" A..= identity, "bytes" A..= size]

instance A.FromJSON Receipt where
  parseJSON = A.withObject "backup receipt" $ \o -> do
    version <- o A..: "version"
    unless (version == (1 :: Int)) (fail "unknown backup receipt version")
    identity <- o A..: "identity"
    size <- o A..: "bytes"
    when (size < 0) (fail "negative backup size")
    pure (Receipt identity size)

-- Use lstat so a dangling link is evidence, not an absent backup.
present :: FilePath -> IO Bool
present path = (getSymbolicLinkStatus path >> pure True) `catchIOError` \err ->
  if isDoesNotExistError err then pure False else ioError err

recordIdentity :: FilePath -> FilePath -> IO T.Text
recordIdentity root target = do
  base <- canonicalizePath root
  path <- canonicalizePath target
  let relative = makeRelative base path
  pure $ T.pack $ if isRelative relative && ".." `notElem` splitDirectories relative
    then "relative:" ++ relative
    else "absolute:" ++ path

directoryFor :: FilePath -> HouseDay -> T.Text -> FilePath
directoryFor backups day identity = backups </> show day </> "v2" </>
  show (hash (TE.encodeUtf8 identity) :: Digest SHA256)

-- The hash is only a filesystem-safe path key. The full identity is checked
-- on read, so a key collision cannot silently merge two records.
backupDirectory :: FilePath -> FilePath -> HouseDay -> FilePath -> IO FilePath
backupDirectory backups root day target = directoryFor backups day <$> recordIdentity root target

readReceipt :: FilePath -> T.Text -> IO B.ByteString
readReceipt directory expected = do
  raw <- B.readFile (directory </> "receipt.json")
  Receipt identity size <- either (ioError . userError . ("invalid backup receipt: " ++)) pure
    (A.eitherDecodeStrict raw)
  unless (identity == expected) (ioError (userError "backup identity mismatch; original retained"))
  bytes <- B.readFile (directory </> "original")
  unless (toInteger (B.length bytes) == size)
    (ioError (userError "incomplete backup; original retained"))
  pure bytes

readOriginal :: FilePath -> FilePath -> HouseDay -> FilePath -> IO B.ByteString
readOriginal backups root day target = do
  identity <- recordIdentity root target
  readReceipt (directoryFor backups day identity) identity

saveOriginal :: FilePath -> FilePath -> HouseDay -> FilePath -> IO ()
saveOriginal backups root day target = do
  identity <- recordIdentity root target
  let directory = directoryFor backups day identity
      parent = backups </> show day </> "v2"
      legacy = backups </> show day </> dropWhile (== '.') (takeFileName target)
  saved <- present directory
  if saved then readReceipt directory identity >> pure () else do
    original <- B.readFile target
    old <- present legacy
    when old $ do
      bytes <- B.readFile legacy
      -- Flat legacy backups have no ownership/completion proof. A valid
      -- base64 prefix is not proof either. Only equality with the untouched
      -- current original allows a new receipt; never replace old evidence.
      unless (not (B.null bytes) && bytes == original) $
        ioError (userError ("unverified legacy backup: " ++ legacy ++ "; original and draft retained"))
    createDirectoryIfMissing True parent
    bracketOnError (mkdtemp (parent </> ".pending-")) removeDirectoryRecursive $ \temporary -> do
      B.writeFile (temporary </> "original") original
      BL.writeFile (temporary </> "receipt.json") (A.encode (Receipt identity (toInteger (B.length original))))
      -- Both closed files become visible together. A killed writer can only
      -- leave an unpublished .pending-* directory, never a published prefix.
      -- rename also refuses to replace an existing nonempty first backup.
      renameDirectory temporary directory
