{-# LANGUAGE OverloadedStrings #-}
-- | Actual seal content protection. Version checks and file commits remain
-- in Store.Edit; this layer cannot bypass them, including with --force.
module Hikaru.Memory.Validation
  ( ValidationEnv(..), correctionCheck, correctionGuards, readStrobe, requireFade
  , readYuugao, requireCorrection, yuugaoGrown ) where
import Control.Exception (throw)
import Control.Monad (forM,unless)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Hikaru.HouseDay (HouseDay)
import qualified Hikaru.Memory.Strobe as Strobe
import Hikaru.Compat.Json (PyString, pyStringFromChars, pyStringToChars)
import Hikaru.Compat.Diagnostic (Diagnostic(..),raiseDiagnostic)
import qualified Hikaru.Memory.Fade as F
import qualified Hikaru.Memory.Correction.Parse as P
import qualified Hikaru.Memory.Correction.Check as C
import qualified Hikaru.Memory.Correction.Annotate as A
import qualified Hikaru.Memory.Yuugao as Y
import Hikaru.Store.Sealed (readSealed)
import System.IO.Error (catchIOError,isDoesNotExistError)

data ValidationEnv = ValidationEnv
  { validationDay :: IO HouseDay
  , validationResolve :: String -> IO FilePath
  , validationYuugao :: FilePath
  , validationOutput :: PyString -> IO ()
  }

-- Existing seal diagnostics are adapted only at this IO boundary.
requireFade :: Either F.FadeError a -> IO a
requireFade = either (raiseDiagnostic . diagnostic) pure
  where
    diagnostic (F.InvalidDateTitle title) = Diagnostic "ValueError"
      (pyStringFromChars ("invalid strobe date heading: " ++ Strobe.titleText title))
    diagnostic F.CutoffBeforeCalendar = Diagnostic "OverflowError" "strobe fade cutoff is before year 1"
    diagnostic (F.GuardWithoutIds guard) = Diagnostic "ValueError"
      (pyStringFromChars ("invalid correction protection: missing revision id for entry: " ++ F.guardEntry guard))

readStrobe :: String -> IO Strobe.Strobe
readStrobe = either (ioError . userError) pure . Strobe.parseStrobe

requireCorrection :: Either Y.CorrectionError a -> IO a
requireCorrection = either failCorrection pure
  where
    failCorrection Y.SinceAfterCalendar = raiseDiagnostic
      (Diagnostic "ValueError" "invalid correction since field: year 10000 is out of range")
    failCorrection Y.BeforeCalendar = raiseDiagnostic
      (Diagnostic "OverflowError" "correction boundary is before year 1")
    failCorrection (Y.YuugaoSyntax reason) = ioError (userError reason)
    failCorrection (Y.RecordSyntax reason) = ioError (userError reason)

readYuugao :: String -> IO Y.Yuugao
readYuugao = requireCorrection . Y.parseYuugao

-- Legacy post-commit callback has a pure PyString signature. Keep that adapter
-- here, while the counting decision consumes only parsed documents.
yuugaoGrown :: PyString -> PyString -> [PyString]
yuugaoGrown old new = either (throw . userError . show) (map pyStringFromChars)
  (Y.grown <$> Y.parseYuugao (pyStringToChars old) <*> Y.parseYuugao (pyStringToChars new))

correctionCheck :: ValidationEnv -> Y.Yuugao -> IO ([String],[String])
correctionCheck env input=do
  records<-M.fromList . catMaybes <$> forM (P.namesUsed input)(\name->do
    target<-validationResolve env name
    (Just . (P.normName name,) . T.unpack <$> readSealed target) `catchIOError` \err ->
      if isDoesNotExistError err then pure Nothing else ioError err)
  requireCorrection (C.check input (Just records))

correctionGuards :: ValidationEnv -> IO [F.Guard]
correctionGuards env = do
  doc <- readSealed (validationYuugao env) >>= readYuugao . T.unpack
  -- The lossless parser retains malformed correction drafts. Those cannot
  -- supply a complete protection set, even though their text was readable.
  (stops, _) <- requireCorrection (C.check doc Nothing)
  let syntax = [C.whereProblem doc p | p <- Y.indexProblems doc, not (Y.problemStop p)]
      problems = stops ++ syntax
  unless (null problems) $ ioError (userError ("invalid correction protection:\n" ++ unlines problems))
  pure (A.fadeGuard doc)

