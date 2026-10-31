{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hikaru.OwnTime.Errors
  ( withDiagnostics, invalidFile, diagnosticText, fileDiagnostic, operationFailed,
    StateFailure(..), stateFailure ) where

import Control.Exception (Exception, Handler(..), IOException, catches, throwIO)
import Hikaru.Compat.Diagnostic
import qualified Hikaru.Compat.Json as J
import Hikaru.Compat.Render (pyRepr)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

diagnosticText :: Diagnostic -> String
diagnosticText = J.pyStringToChars . renderDiagnostic

-- Recovery needs the source path, without parsing a rendered error message.
data StateFailure = StateFailure FilePath String
instance Show StateFailure where
  show (StateFailure path why) = diagnosticText (fileDiagnostic path why)
instance Exception StateFailure

stateFailure :: FilePath -> String -> IO a
stateFailure path = throwIO . StateFailure path

withDiagnostics :: IO a -> IO a
withDiagnostics action = action `catches`
  [ Handler (\(StateFailure path why) -> stop (fileDiagnostic path why))
  , Handler (\(DiagnosticFailure detail) -> stop detail)
  , Handler (\(err :: IOException) -> ioDiagnostic Nothing Nothing err >>= stop)
  ]
  where stop detail = hPutStrLn stderr (diagnosticText detail) >> exitFailure

fileDiagnostic :: FilePath -> String -> Diagnostic
fileDiagnostic path why = Diagnostic "ValueError" $
  J.pyStringConcat ["invalid file ", pyRepr (J.String (J.pyStringFromChars path)), ": ", J.pyStringFromChars why]

invalidFile :: FilePath -> String -> IO a
invalidFile = stateFailure

operationFailed :: String -> IO a
operationFailed = raiseDiagnostic . Diagnostic "RuntimeError" . J.pyStringFromChars
