module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import Hikaru.OwnTime.Errors (withDiagnostics)
import Hikaru.OwnTime.Mihari (talkMark, presence, check)
import Hikaru.OwnTime.Tokimori (run, nowCmd, status)

main :: IO ()
main = withDiagnostics command

command :: IO ()
command = do
  args <- getArgs
  case args of
    ["talk-mark"] -> void (try talkMark :: IO (Either SomeException ()))  -- hook は、何があっても会話を止めない
    ["turn-end"] -> pure ()  -- 2026-10-22 の夜の形の名残。何もしない
    ("presence" : rest) -> presence rest
    ("check" : rest) -> check rest
    ("run" : rest) -> run rest
    ["now"] -> nowCmd
    ["status"] -> status
    _ -> do
      hPutStrLn stderr "使い方: hikaru-owntime talk-mark | presence 所 様子 [どこ] | presence clear | presence talk | check --dry-run | run | now | status"
      exitWith (ExitFailure 2)

