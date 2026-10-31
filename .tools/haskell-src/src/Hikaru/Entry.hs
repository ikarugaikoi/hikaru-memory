-- | Parse the process boundary once; dispatch only receives a known tool.
module Hikaru.Entry (Invocation(..), Usage(..), parseInvocation) where

import System.FilePath (takeFileName)

data Invocation
  = Ima | Oshirase
  | Koyomi [String] | Ochibo [String] | Shirushi [String]
  | Mezame [String] | Fuutou [String]
  | InvalidateAll | InvalidateConversation FilePath
  deriving (Eq, Show)

data Usage = ToolUsage | LedgerUsage deriving (Eq, Show)

parseInvocation :: String -> [String] -> Either Usage Invocation
parseInvocation program args = case takeFileName program of
  "hikaru-core" -> case args of
    name:rest -> named name rest
    [] -> Left ToolUsage
  name -> named name args
  where
    named name rest = case name of
      -- The two hook wrappers historically ignored their argument lists.
      "ima" -> Right Ima
      "oshirase" -> Right Oshirase
      "koyomi" -> Right (Koyomi rest)
      "ochibo" -> Right (Ochibo rest)
      "shirushi" -> Right (Shirushi rest)
      "mezame" -> Right (Mezame rest)
      "fuutou" -> Right (Fuutou rest)
      "choubo" -> case rest of
        ["invalidate", "--all"] -> Right InvalidateAll
        ["invalidate", "--conversation", path] -> Right (InvalidateConversation path)
        _ -> Left LedgerUsage
      _ -> Left ToolUsage
