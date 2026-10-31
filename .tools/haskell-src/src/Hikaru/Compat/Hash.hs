{-# LANGUAGE OverloadedStrings #-}
-- Scalar keys for standard ordered containers; Python numeric equality is retained.
module Hikaru.Compat.Hash (HashContext, loadHashContext, Scalar(..), scalarKey) where
import Data.Text (Text)
import Hikaru.Compat.Json

data HashContext = HashContext
loadHashContext :: IO HashContext
loadHashContext = pure HashContext
data Scalar = None | Numeric Rational | MinusInfinity | PlusInfinity | NaN | Textual PyString deriving (Eq,Ord)
scalarKey :: Value -> Either Text Scalar
scalarKey Null = Right None
scalarKey (Bool x) = Right (Numeric (if x then 1 else 0))
scalarKey (String x) = Right (Textual x)
scalarKey (Number (PyInteger n)) = Right (Numeric (fromInteger n))
scalarKey (Number PyNaN) = Right NaN
scalarKey (Number (PyFloat n))
  | isNaN n = Right NaN
  | isInfinite n = Right (if n < 0 then MinusInfinity else PlusInfinity)
  | otherwise = Right (Numeric (toRational n))
scalarKey (Array _) = Left "unhashable list"
scalarKey (Object _) = Left "unhashable dict"
