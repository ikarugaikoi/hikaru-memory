-- | Stable work paths used by separate open/close processes. Environment
-- lookup and default project identity are configured by the entrypoint.
-- Separate invocations resolve the same conversation-scoped directory.
module Hikaru.Store.Work
  ( WorkEnv(..), workDir, stem
  , WorkFile, WorkPart(..), workFile, workFilePath, readPlainFile
  , loadAsked, saveAsked ) where

import Data.List (isPrefixOf)
import Hikaru.Compat.Exception (fromPython)
import Hikaru.Compat.Json (Value(Null), pyStringToText)
import Hikaru.Compat.FileSystem (pathExists)
import Hikaru.Store.Coordinator (readJson,writeJson,readUtf8)
import Hikaru.Sealed (Plain(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>),takeDirectory,normalise)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.Files (setFileMode)

data WorkEnv = WorkEnv
  { workOverride :: Maybe FilePath
  , workSessionId :: String
  , workScratchRoot :: FilePath
  , workProjectSlug :: String
  , workTemporaryRoot :: FilePath
  }

-- scratchRoot is normally /private/tmp/claude-<uid>, but tests and alternate
-- deployments supply a private root. An absolute-looking session fragment
-- is concatenated as in the baseline f-string, not as a path-join override.
workDir :: WorkEnv -> IO FilePath
workDir env = do
  base <- case workOverride env of
    Just explicit | not(null explicit) -> pure explicit
    _ | null(workSessionId env) -> pure fallback
      | otherwise -> do
          let candidate=normalise(workScratchRoot env++"/"++workProjectSlug env++"/"++workSessionId env++"/scratchpad")
          parentExists<-pathExists(takeDirectory candidate)
          pure(if parentExists then candidate else fallback)
  let directory=normalise(base </> "hikaru")
  createDirectoryIfMissing True directory
  setFileMode directory 0o700
  pure directory
  where fallback=workTemporaryRoot env </> "hikaru_work"

stem :: String -> String
stem name
  | "ichifure:" `isPrefixOf` name="diaryweek-"++drop 9 name
  | otherwise=map(\c->if c==':' then '-' else c)name

-- Only paths resolved in the configured work directory can be written as
-- plaintext by the action interpreter. The constructor is intentionally hidden.
data WorkFile = WorkFile FilePath
data WorkPart = Draft | FingerprintFile | Base | Mine
workFile :: WorkEnv -> String -> WorkPart -> IO WorkFile
workFile env name part = do
  directory <- workDir env
  let suffix = case part of
        Draft -> ".md"
        FingerprintFile -> ".sha1"
        Base -> ".base.md"
        Mine -> ".mine.md"
  pure (WorkFile (directory </> (stem name ++ suffix)))
workFilePath :: WorkFile -> FilePath
workFilePath (WorkFile path) = path

readPlainFile :: FilePath -> IO Plain
-- Keep the established decoding/error boundary, and leave its representation
-- here: every edit decision receives Plain exactly once at ingress.
readPlainFile path = readUtf8 path >>= fromPython . pyStringToText >>= pure . Plain

loadAsked :: WorkEnv -> IO Value
loadAsked env = do
  directory <- workDir env
  readJson (directory </> "asked.json") `catchIOError` \err ->
    if isDoesNotExistError err then pure Null else ioError err
saveAsked :: WorkEnv -> Value -> IO ()
saveAsked env value=workDir env >>= \directory->writeJson False Nothing(directory </> "asked.json")value
