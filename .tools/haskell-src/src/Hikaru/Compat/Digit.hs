{-# LANGUAGE ForeignFunctionInterface #-}
-- | ICU Numeric_Type Decimal and Digit properties, with no local ranges.
module Hikaru.Compat.Digit (pyIsDigit) where
import Data.Char (ord)
import Foreign.C.Types
foreign import ccall unsafe "u_getIntPropertyValue" property :: CInt -> CInt -> CInt
pyIsDigit :: Char -> Bool
pyIsDigit c = let kind = property (fromIntegral (ord c)) 0x1009
              in kind == 1 || kind == 2
