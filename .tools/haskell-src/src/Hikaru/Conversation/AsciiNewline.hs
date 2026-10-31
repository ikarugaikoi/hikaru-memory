{-# LANGUAGE BangPatterns, MagicHash, PatternSynonyms, OverloadedStrings #-}
-- Pinned text-2.1.3 / GHC-9.10.3. The caller has already strictly decoded
-- Text. This module searches every byte for fixed ASCII CR/LF, without
-- touching any byte outside the supplied Text slice.
module Hikaru.Conversation.AsciiNewline (breakLF,normalizeNewlines) where
import Data.Bits
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import GHC.Exts (Int(I#), Word(W#), indexWord8ArrayAsWord#)

-- Borrow propagation can mark additional lanes, but only existence is used.
-- The first matching byte in a flagged word is found with exact byte reads.
zeroByte :: Word -> Bool
zeroByte x = (x - 0x0101010101010101) .&. complement x .&. 0x8080808080808080 /= 0
{-# INLINE zeroByte #-}

wordAt :: A.Array -> Int -> Word
wordAt (A.ByteArray a) (I# i) = W# (indexWord8ArrayAsWord# a i)
{-# INLINE wordAt #-}

firstAscii :: Word -> Text -> Int
firstAscii needle (TI.Text arr off len) = chunks 0
  where
    !repeated = needle * 0x0101010101010101
    chunks !i
      | len - i >= 8 && not (zeroByte (wordAt arr (off+i) `xor` repeated)) = chunks (i+8)
      | otherwise = scalar i
    scalar !i
      | i == len = len
      | fromIntegral (A.unsafeIndex arr (off+i)) == needle = i
      | otherwise = scalar (i+1)
{-# INLINE firstAscii #-}

-- LF is a single ASCII byte and cannot occur within a multibyte encoding,
-- so the two Text slices always start/end at valid UTF-8 boundaries.
breakLF :: Text -> (Text,Text)
breakLF text@(TI.Text arr off len)
  | finiteBitSize (0 :: Word) /= 64 = T.breakOn "\n" text
  | otherwise = let !n=firstAscii 10 text
                in (TI.Text arr off n,TI.Text arr (off+n) (len-n))
{-# INLINE breakLF #-}

-- A no-CR input is returned verbatim: no copy and no codepoint traversal.
-- If CR is present, keep the original CRLF-then-CR replacement order.
normalizeNewlines :: Text -> Text
normalizeNewlines text@(TI.Text _ _ len)
  | finiteBitSize (0 :: Word) /= 64 = original text
  | firstAscii 13 text == len = text
  | otherwise = original text
  where original = T.replace "\r" "\n" . T.replace "\r\n" "\n"
{-# INLINE normalizeNewlines #-}
