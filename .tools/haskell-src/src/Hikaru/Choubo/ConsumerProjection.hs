{-# LANGUAGE OverloadedStrings #-}
-- | Fixed-schema position helpers. These never accept a business Value or
-- message string, and cannot serialise one under a different field name.
module Hikaru.Choubo.ConsumerProjection
  ( spanProjection, restoreSpan, exactFields, flagField, positionField ) where

import Data.List (sort)
import Data.Text (Text)
import Hikaru.Choubo.Types

spanProjection :: RowSpan -> Projection
spanProjection span_ = PFields
  [ ("begin", PPosition (rowBegin span_))
  , ("end", PPosition (rowEnd span_))
  , ("ordinal", PPosition (rowOrdinal span_)) ]

exactFields :: [Text] -> Projection -> Maybe [(Text, Projection)]
exactFields names (PFields fields)
  | sort (map fst fields) == sort names = Just fields
exactFields _ _ = Nothing

flagField :: Text -> [(Text, Projection)] -> Maybe Bool
flagField name fields = case lookup name fields of
  Just (PFlag value) -> Just value
  _ -> Nothing

positionField :: Text -> [(Text, Projection)] -> Maybe Integer
positionField name fields = case lookup name fields of
  Just (PPosition value) | value >= 0 -> Just value
  _ -> Nothing

restoreSpan :: Projection -> Maybe RowSpan
restoreSpan value = do
  fields <- exactFields ["begin", "end", "ordinal"] value
  begin <- positionField "begin" fields
  end <- positionField "end" fields
  ordinal <- positionField "ordinal" fields
  if end > begin && ordinal <= begin then Just (RowSpan begin end ordinal) else Nothing
