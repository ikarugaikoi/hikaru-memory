{-# LANGUAGE OverloadedStrings #-}
module Hikaru.Conversation.Tobari (phrasePairs, punctuation, startPhrase, endPhrase) where
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Hikaru.Local.Settings as Local
phrasePairs :: [(Text, Text)]
phrasePairs = Local.phrasePairs
punctuation :: Text
punctuation = "、。，．？?,.｡､"
startPhrase, endPhrase :: IsString a => a
startPhrase = fromString (T.unpack (fst (head phrasePairs)))
endPhrase = fromString (T.unpack (snd (head phrasePairs)))
