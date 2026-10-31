-- | Whitespace in the existing record grammar. In particular U+2028/U+2029
-- are whitespace within a line, not line delimiters: records split on LF only.
-- Both Text records and Memory.Text's code-point strings use this predicate.
module Hikaru.RecordText (space, strip, stripStart) where

import Data.Char (GeneralCategory(..), generalCategory)
import Data.Text (Text)
import qualified Data.Text as T

space :: Char -> Bool
space c = generalCategory c `elem` [Space, LineSeparator, ParagraphSeparator]
          || c `elem` ['\t', '\n', '\v', '\f', '\r', '\x85', '\x1c', '\x1d', '\x1e', '\x1f']

strip, stripStart :: Text -> Text
strip = T.dropAround space
stripStart = T.dropWhile space
