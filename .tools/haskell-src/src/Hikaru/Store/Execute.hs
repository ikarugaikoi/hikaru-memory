{-# LANGUAGE OverloadedStrings #-}
-- | The small interpreter shared by editing and automated record writers.
module Hikaru.Store.Execute
  ( execute, locked, Committed, commit, readCommitted, committedPath, committedPlain
  , JournalResult(..), appendJournal ) where

import Control.Exception (IOException, throwIO, try)
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text (Text)
import Data.Void (absurd)
import Hikaru.Sealed (Plain(..))
import Hikaru.Store.Actions
import Hikaru.Store.Coordinator (StoreEnv, backup, withRecordLock)
import Hikaru.Store.Sealed (writeSealed, readSealed)
import Hikaru.Store.Work (workFilePath)
import System.Directory (removeFile, doesPathExist)
import System.IO.Error (catchIOError, isDoesNotExistError)

execute :: StoreEnv -> (progress -> IO ()) -> (Text -> IO ()) -> [Action progress] -> IO ()
execute store save output = mapM_ run
  where
    run (Backup path) = backup store path
    run (WriteRecord path sealed) = writeSealed path sealed
    run (WriteWork file (Plain text)) = B.writeFile (workFilePath file) (TE.encodeUtf8 text)
    run (RemoveWork file) = removeFile (workFilePath file) `catchIOError` \err ->
      if isDoesNotExistError err then pure () else throwIO err
    run (RemoveSource path) = removeFile path
    run (SaveProgress progress) = save progress
    run (Say text) = output (text <> "\n")

locked :: StoreEnv -> Lock -> IO a -> IO a
locked store scope action = case scope of
  TargetLock target -> withRecordLock store False target (const action)
  CollectThenTarget target -> withRecordLock store True target (const action)

-- A data (not newtype) with a hidden constructor: neither Plain, Sealed nor
-- a prepared plan is a receipt. A completed atomic write, or a successful
-- read of an already sealed record, is required to obtain one.
data Committed = Committed FilePath Plain

commit :: StoreEnv -> RecordWrite -> IO Committed
commit store plan = do
  execute store absurd (const (pure ())) (writeActions plan)
  pure (Committed (writeTarget plan) (writePlain plan))

readCommitted :: FilePath -> IO Committed
readCommitted path = Committed path . Plain <$> readSealed path

committedPath :: Committed -> FilePath
committedPath (Committed path _) = path
committedPlain :: Committed -> Plain
committedPlain (Committed _ plain) = plain

data JournalResult = JournalSaved | JournalFailed deriving (Eq, Show)

-- Only this newly specified system journal has non-fatal write failures.
-- Catch the concrete IO/UTF-8 errors from its storage boundary, never async
-- exceptions or arbitrary programming failures. The caller displays failure.
appendJournal :: StoreEnv -> FilePath -> IO Plain -> IO JournalResult
appendJournal store target makeAddition = do
  result <- try @IOException (try @UnicodeException append)
  pure $ case result of
    Left _ -> JournalFailed
    Right (Left _) -> JournalFailed
    Right (Right saved) -> saved
  where
    append = do
      Plain addition <- makeAddition
      locked store (TargetLock target) $ do
        exists <- doesPathExist target
        old <- if exists then readSealed target else pure ""
        case prepareWrite target (Plain (old <> addition)) of
          Left _ -> pure JournalFailed
          Right plan -> do
            -- This append-only system journal retains every previous row in
            -- its replacement. It is not an edited business record: invoking
            -- Backup here would also prune unrelated business backups when
            -- a newly created diary/shiori had not triggered that cleanup.
            _ <- commit store plan
            pure JournalSaved
