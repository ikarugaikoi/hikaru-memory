{-# LANGUAGE OverloadedStrings #-}
-- | Python's visible binary64 formatting. This does not move numerical or
-- business work into the model process. See the synthetic Python comparison.
module Hikaru.Compat.Number (pyFloatRepr) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Ratio ((%))
import Numeric (floatToDigits)

negative :: Double -> Bool
negative value = value < 0 || isNegativeZero value

pyFloatRepr :: Double -> Text
pyFloatRepr value
  | isNaN value = "nan"
  | isInfinite value = if value < 0 then "-inf" else "inf"
  | value == 0 = if negative value then "-0.0" else "0.0"
  | otherwise = sign <> rendered
  where
    sign = if negative value then "-" else ""
    (text, position) = shortestDecimal (abs value)
    count = T.length text
    rendered
      | position > (-4) && position <= 16 =
          if position <= 0 then "0." <> T.replicate (-position) "0" <> text
          else if position >= count then text <> T.replicate (position-count) "0" <> ".0"
          else T.take position text <> "." <> T.drop position text
      | otherwise = T.take 1 text <> (if count > 1 then "." <> T.drop 1 text else "")
          <> "e" <> (if position-1 < 0 then "-" else "+") <> exponentDigits
    exponentDigits = let s = T.pack (show (abs (position-1))) in T.replicate (max 0 (2-T.length s)) "0" <> s

-- GHC's floatToDigits can keep an unnecessary final digit and rounds some
-- decimal ties upward. Use it only to bound the search, then choose the
-- nearest exact decimal which rounds back to the same binary64 (even ties).
-- Try adjacent grid points too: binary intervals around powers of two are
-- asymmetric. Each failed coarser grid also rules out all its coarser grids.
shortestDecimal :: Double -> (Text, Int)
shortestDecimal value = normalize coefficient decimalPower
  where
    (boundDigits, position) = floatToDigits 10 value
    exact = toRational value
    power10 exponent_ = if exponent_ >= 0 then (10 ^ exponent_) % 1 else 1 % (10 ^ (-exponent_))
    candidate precision =
      let decimalPower_ = position - precision
          unit = power10 decimalPower_
          nearest = round (exact / unit) :: Integer
          options = [(n, decimalPower_) | n <- [nearest-1, nearest, nearest+1], n > 0,
                     (fromRational (fromInteger n * unit) :: Double) == value]
          score (n, _) = (abs (fromInteger n * unit - exact), odd n)
      in if null options then Nothing else Just (minimumBy (comparing score) options)
    upperPrecision = length boundDigits
    initial = case candidate upperPrecision of
      Just result -> result
      Nothing -> (read (concatMap show boundDigits), position-upperPrecision)
    descend precision best
      | precision < 1 = best
      | otherwise = case candidate precision of
          Nothing -> best
          Just result -> descend (precision-1) result
    (coefficient, decimalPower) = descend (upperPrecision-1) initial
    normalize n exponent_
      | n `mod` 10 == 0 = normalize (n `div` 10) (exponent_+1)
      | otherwise = let digits = T.pack (show n) in (digits, T.length digits + exponent_)
