-- | pathlib's POSIX stat predicates. Permission and unexpected filesystem
-- failures propagate; only its explicit ignored errno set means "absent".
module Hikaru.Compat.FileSystem (pathExists, pathIsDirectory, pathIsFile) where
import Control.Exception (catch,throwIO)
import Foreign.C.Error (Errno(..),eNOENT,eNOTDIR,eBADF,eLOOP)
import GHC.IO.Exception (IOException(..))
import System.Posix.Files (FileStatus,getFileStatus,isDirectory,isRegularFile)

statPredicate :: (FileStatus -> Bool) -> FilePath -> IO Bool
statPredicate predicate path
  | '\0' `elem` path = pure False
  | otherwise = (predicate <$> getFileStatus path) `catch` \e ->
      if maybe False (\code -> Errno code `elem` [eNOENT,eNOTDIR,eBADF,eLOOP]) (ioe_errno e)
        then pure False else throwIO (e :: IOException)

pathExists, pathIsDirectory, pathIsFile :: FilePath -> IO Bool
pathExists = statPredicate (const True)
pathIsDirectory = statPredicate isDirectory
pathIsFile = statPredicate isRegularFile
