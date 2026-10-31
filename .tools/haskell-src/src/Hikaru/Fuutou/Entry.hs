-- | The former launcher's policy, decided before any record IO. Only append
-- and diary can request a language check; only the two Python tools can exec.
{-# LANGUAGE OverloadedStrings #-}
module Hikaru.Fuutou.Entry
  ( FuutouPlan, LanguageCommand(..), LanguageCheck(..), LanguageOptionError(..)
  , planFuutou, runFuutou, languageArguments, languageOptionMessage
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON(..), eitherDecodeStrict', withObject, (.:))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hikaru.Config (Config(..))
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStr, hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorString)
import System.Posix.Process (executeFile)
import System.Process (proc, readCreateProcessWithExitCode)

data LanguageCommand = Append | Diary deriving (Eq, Show)
data LanguageCheck = LanguageCheck LanguageCommand String FilePath deriving (Eq, Show)
data LanguageOptionError = LanguageOptionError deriving (Eq, Show)
data PythonTool = Prune | Koyori deriving (Eq, Show)
data FuutouPlan
  = Native [String] (Maybe LanguageCheck)
  | Python PythonTool [String]
  deriving (Eq, Show)

-- Exit 1 alone is ambiguous: Python also uses it when a module cannot load.
-- The CLI emits this receipt only after check() has returned. Human messages
-- stay on stderr; the receipt is consumed here, never forwarded to the caller.
newtype CheckReceipt = CheckReceipt Bool
instance FromJSON CheckReceipt where
  parseJSON = withObject "language check receipt" $ \fields -> CheckReceipt <$> fields .: "stopped"

languageOptionMessage :: String
languageOptionMessage = "--lang-ok は append と diary にだけつける（言い方の検査を、その一回だけ越える印）。何もしていません。"

planFuutou :: [String] -> Either LanguageOptionError FuutouPlan
planFuutou original
  | languageOK && command `notElem` [Just "append", Just "diary"] = Left LanguageOptionError
  | command == Just "prune" = Right (Python Prune original)
  | command == Just "koyori" = Right (Python Koyori (filter (`notElem` ["koyori", "--force"]) original))
  | otherwise = Right (Native arguments check)
  where
    languageOK = "--lang-ok" `elem` original
    arguments = filter (/= "--lang-ok") original
    positional = filter (/= "--force") arguments
    command = case positional of name:_ -> Just name; [] -> Nothing
    check | languageOK = Nothing
          | otherwise = case positional of
              "append":name:path:_ -> Just (LanguageCheck Append name path)
              "diary":name:path:_ -> Just (LanguageCheck Diary name path)
              _ -> Nothing

languageArguments :: FilePath -> LanguageCheck -> [String]
languageArguments root (LanguageCheck command name path) =
  ["--check", case command of Append -> "append"; Diary -> "diary", name, path, root]

pythonProgram :: IO FilePath
pythonProgram = maybe "python3" id <$> lookupEnv "HIKARU_PYTHON"

runFuutou :: Config -> FuutouPlan -> ([String] -> IO ()) -> IO ()
runFuutou config plan native = case plan of
  Python tool arguments -> do
    python <- pythonProgram
    let script = case tool of Prune -> "_prune.py"; Koyori -> "_koyori.py"
    executeFile python True (["-B", tools </> script] ++ arguments) Nothing
  Native arguments check -> do
    mapM_ checkLanguage check
    native arguments
  where
    tools = codeRoot config </> ".tools"
    checkLanguage request = do
      python <- pythonProgram
      result <- try (readCreateProcessWithExitCode
        (proc python (["-B", tools </> "_kotoba.py"] ++ languageArguments (dataRoot config) request)) "")
      case result of
        Left (err :: IOException) -> unavailable ("could not start " ++ python ++ ": " ++ ioeGetErrorString err)
        Right (status, receipt, messages) -> case (status, eitherDecodeStrict' (TE.encodeUtf8 (T.pack receipt))) of
          (ExitSuccess, Right (CheckReceipt False)) -> hPutStr stderr messages
          (ExitFailure 1, Right (CheckReceipt True)) -> hPutStr stderr messages >> exitFailure
          _ -> unavailable (case status of ExitSuccess -> "missing or invalid check receipt"; ExitFailure code -> "exit " ++ show code)
    unavailable reason = do
      hPutStrLn stderr ("RuntimeError: language check did not complete: " ++ tools </> "_kotoba.py" ++ ": " ++ reason)
      exitFailure
