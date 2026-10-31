{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The permissive byte-input semantics used by Python base64.b64decode.
-- Invalid alphabet bytes are ignored; a completed padding group terminates
-- decoding even when more data follows. See CPython v3.12.12 binascii.c.
module Hikaru.Compat.Base64
  ( Base64Error(..), decodeBase64, encodeBase64, base64ErrorMessage ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr,plusPtr)
import Foreign.Storable (peekByteOff,pokeByteOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

data Base64Error = InvalidLength !Int | IncorrectPadding
  deriving (Eq, Show)

base64ErrorMessage :: Base64Error -> String
base64ErrorMessage IncorrectPadding = "Incorrect padding"
base64ErrorMessage (InvalidLength n) = "Invalid base64-encoded string: number of data characters ("
  ++ show n ++ ") cannot be 1 more than a multiple of 4"

alphabet :: BS.ByteString
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

sextet :: Word8 -> Word8
sextet c
  | c >= 65 && c <= 90 = c - 65
  | c >= 97 && c <= 122 = c - 71
  | c >= 48 && c <= 57 = c + 4
  | c == 43 = 62
  | c == 47 = 63
  | otherwise = 255

-- Fixed total lookup: Word8 indexes cover exactly the 256 initialized bytes.
-- Invalid alphabet entries remain 255; the permissive fallback still uses
-- its original sextet/byte-state logic below.
decodeSextets :: BS.ByteString
decodeSextets = BS.pack [sextet c | c <- [0..255]]
{-# NOINLINE decodeSextets #-}

-- The only IO below allocates and initializes a fresh immutable bytestring.
-- No external state is observed. Each write stays below the allocated upper
-- bound, and createAndTrim' publishes only initialized bytes after success.
decodeBase64 :: BS.ByteString -> Either Base64Error BS.ByteString
decodeBase64 input = unsafeDupablePerformIO $
  withForeignPtr source $ \base -> withForeignPtr tableSource $ \tableBase -> do
    (output,result) <- BSI.createAndTrim' size $ \destination -> do
      (written,result) <- go (tableBase `plusPtr` tableOffset) (base `plusPtr` offset) destination 0 0 0 0 0
      pure (0,written,result)
    pure (output <$ result)
  where
    (source,offset,size) = BSI.toForeignPtr input
    (tableSource,tableOffset,_) = BSI.toForeignPtr decodeSextets
    go :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Int -> Int -> Word8 -> Int -> Int -> IO (Int,Either Base64Error ())
    go table src dst !i !position !remaining !pads !written
      | i == size = pure $ case position of
          0 -> (written,Right ())
          1 -> (0,Left (InvalidLength ((written `div` 3) * 4 + 1)))
          _ -> (0,Left IncorrectPadding)
      | position == 0 && size-i >= 4 = do
          -- All four reads are inside the input slice. Non-alphabet bytes,
          -- including padding, return to the unchanged one-byte machine.
          a <- peekByteOff src i
          b <- peekByteOff src (i+1)
          c <- peekByteOff src (i+2)
          d <- peekByteOff src (i+3)
          va <- peekByteOff table (fromIntegral (a::Word8))
          vb <- peekByteOff table (fromIntegral (b::Word8))
          vc <- peekByteOff table (fromIntegral (c::Word8))
          vd <- peekByteOff table (fromIntegral (d::Word8))
          -- Each value is in [0,63] or exactly 255, so one OR test is
          -- equivalent to four alphabet checks, including all invalid bytes.
          if (va .|. vb .|. vc .|. vd) < (64::Word8) then do
            pokeByteOff dst written ((va `shiftL` 2) .|. (vb `shiftR` 4))
            pokeByteOff dst (written+1) ((vb `shiftL` 4) .|. (vc `shiftR` 2))
            pokeByteOff dst (written+2) ((vc `shiftL` 6) .|. vd)
            go table src dst (i+4) 0 0 0 (written+3)
          else single table src dst i position remaining pads written a
      | otherwise = do
          byte <- peekByteOff src i
          single table src dst i position remaining pads written byte
    single :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Int -> Int -> Word8 -> Int -> Int -> Word8 -> IO (Int,Either Base64Error ())
    single table src dst !i !position !remaining !pads !written !byte
      | byte == 61 =
          if position >= 2 && position + pads + 1 >= 4
          then pure (written,Right ())
          else go table src dst (i+1) position remaining (if position >= 2 then pads+1 else pads) written
      | value >= 64 = go table src dst (i+1) position remaining pads written
      | otherwise = case position of
          0 -> go table src dst (i+1) 1 value 0 written
          1 -> emit ((remaining `shiftL` 2) .|. (value `shiftR` 4)) 2 (value .&. 15)
          2 -> emit ((remaining `shiftL` 4) .|. (value `shiftR` 2)) 3 (value .&. 3)
          _ -> emit ((remaining `shiftL` 6) .|. value) 0 0
      where
        value = sextet byte
        emit out next rest = do
          pokeByteOff dst written out
          go table src dst (i+1) next rest 0 (written+1)
{-# NOINLINE decodeBase64 #-}

encodeBase64 :: BS.ByteString -> BS.ByteString
encodeBase64 input = unsafeDupablePerformIO $
  withForeignPtr source $ \base -> withForeignPtr digitsSource $ \digitsBase ->
    BSI.create outputSize $ \destination ->
      go (base `plusPtr` offset) (digitsBase `plusPtr` digitsOffset) destination 0 0
  where
    (source,offset,size) = BSI.toForeignPtr input
    (digitsSource,digitsOffset,_) = BSI.toForeignPtr alphabet
    !outputSize = ((size + 2) `div` 3) * 4
    -- Every digit expression is in [0,63]. Byte writes require no machine
    -- alignment or byte-order assumption; all output bytes are initialized.
    emitFull :: Ptr Word8 -> Ptr Word8 -> Int -> Word8 -> Word8 -> Word8 -> IO ()
    emitFull digits dst o a b c = do
      x <- peekByteOff digits (fromIntegral (a `shiftR` 2)) :: IO Word8
      y <- peekByteOff digits (fromIntegral (((a .&. 3) `shiftL` 4) .|. (b `shiftR` 4))) :: IO Word8
      z <- peekByteOff digits (fromIntegral (((b .&. 15) `shiftL` 2) .|. (c `shiftR` 6))) :: IO Word8
      w <- peekByteOff digits (fromIntegral (c .&. 63)) :: IO Word8
      pokeByteOff dst o x
      pokeByteOff dst (o+1) y
      pokeByteOff dst (o+2) z
      pokeByteOff dst (o+3) w
    go :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Int -> Int -> IO ()
    go src digits dst !i !o
      | size-i >= 3 = do
          a <- peekByteOff src i
          b <- peekByteOff src (i+1)
          c <- peekByteOff src (i+2)
          emitFull digits dst o a b c
          go src digits dst (i+3) (o+4)
      | i >= size = pure ()
      | otherwise = do
          a <- peekByteOff src i
          b <- if size-i == 2 then peekByteOff src (i+1) else pure 0
          x <- peekByteOff digits (fromIntegral ((a::Word8) `shiftR` 2)) :: IO Word8
          y <- peekByteOff digits (fromIntegral (((a .&. 3) `shiftL` 4) .|. (b `shiftR` 4))) :: IO Word8
          pokeByteOff dst o x
          pokeByteOff dst (o+1) y
          if size-i == 2 then do
            z <- peekByteOff digits (fromIntegral ((b .&. 15) `shiftL` 2)) :: IO Word8
            pokeByteOff dst (o+2) z
            else pokeByteOff dst (o+2) (61::Word8)
          pokeByteOff dst (o+3) (61::Word8)
{-# NOINLINE encodeBase64 #-}
