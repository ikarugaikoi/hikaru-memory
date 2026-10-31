{-# LANGUAGE ForeignFunctionInterface #-}
-- | Character properties from the system ICU category and decimal APIs.
-- Categories define the existing classes; no version filter or override table.
module Hikaru.Compat.Unicode (pyIsDecimal,pyIsPrintable,pyDecimalValue) where
import Data.Char (ord)
import Foreign.C.Types
foreign import ccall unsafe "u_charType" category :: CInt -> CSChar
foreign import ccall unsafe "u_charDigitValue" decimalValue :: CInt -> CInt
pyIsDecimal,pyIsPrintable :: Char -> Bool
pyIsDecimal c = category (fromIntegral (ord c)) == 9
pyIsPrintable c = c == ' ' || (k /= 0 && (k < 12 || k > 18))
  where k = category (fromIntegral (ord c))
pyDecimalValue :: Char -> Maybe Int
pyDecimalValue c = let n = decimalValue (fromIntegral (ord c))
                  in if n < 0 then Nothing else Just (fromIntegral n)
