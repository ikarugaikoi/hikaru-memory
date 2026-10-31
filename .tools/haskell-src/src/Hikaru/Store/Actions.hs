-- | Storage effects are data. Record writes have no plaintext alternative.
-- The progress payload belongs to the caller; its existing file format is
-- interpreted at the storage boundary, independently of record contents.
{-# LANGUAGE OverloadedStrings #-}
module Hikaru.Store.Actions
  ( Action(..), Lock(..), recordLock, replacement
  , RecordWrite, prepareWrite, writeTarget, writePlain, writeActions
  , CommitCommand(..)
  ) where

import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Hikaru.Sealed (Plain, Sealed, sealPlain)
import Hikaru.Store.Work (WorkFile)
import System.FilePath (takeFileName)

data Action progress
  = Backup FilePath
  | WriteRecord FilePath Sealed
  | WriteWork WorkFile Plain
  | RemoveWork WorkFile
  | RemoveSource FilePath
  | SaveProgress progress
  | Say Text

-- Taking and releasing a lock form one scope, never two unrelated actions.
-- The interpreter uses bracket, including when an action throws or is killed.
data Lock = TargetLock FilePath | CollectThenTarget FilePath
  deriving (Eq, Show)

recordLock :: String -> FilePath -> Lock
recordLock name = if "tm:" `isPrefixOf` name then CollectThenTarget else TargetLock

replacement :: Bool -> FilePath -> Sealed -> [Action progress]
replacement exists target sealed = [Backup target | exists] ++ [WriteRecord target sealed]

-- The source and the verified seal cannot be chosen independently. This is a
-- plan, not evidence that the write has happened; Execute owns that evidence.
data RecordWrite = RecordWrite FilePath Plain Sealed

prepareWrite :: FilePath -> Plain -> Either Text RecordWrite
prepareWrite target plain = maybe
  (Left ("封の確認に失敗: " <> T.pack (takeFileName target)))
  (Right . RecordWrite target plain) (sealPlain plain)

writeTarget :: RecordWrite -> FilePath
writeTarget (RecordWrite target _ _) = target
writePlain :: RecordWrite -> Plain
writePlain (RecordWrite _ plain _) = plain
writeActions :: RecordWrite -> [Action progress]
writeActions (RecordWrite target _ sealed) = [WriteRecord target sealed]

data CommitCommand = Close | Append | Diary deriving (Eq, Show)
