{-# LANGUAGE OverloadedStrings #-}

-- | String operations needed by the first hook migration.  Whitespace is
-- the explicit CPython 3.12 str.isspace set, rather than the host Unicode table.
module Hikaru.Compat.Text
  ( pySpace, pyStrip ) where

import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T

pySpace :: Char -> Bool
pySpace c =
  let n = ord c
  in (n >= 0x09 && n <= 0x0d) || (n >= 0x1c && n <= 0x20)
     || n == 0x85 || n == 0xa0 || n == 0x1680
     || (n >= 0x2000 && n <= 0x200a)
     || n `elem` [0x2028, 0x2029, 0x202f, 0x205f, 0x3000]

pyStrip :: Text -> Text
pyStrip = T.dropAround pySpace
