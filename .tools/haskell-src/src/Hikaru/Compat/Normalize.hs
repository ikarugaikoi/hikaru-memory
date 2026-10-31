{-# LANGUAGE ForeignFunctionInterface #-}
-- | NFC normalization supplied by the system ICU library.
module Hikaru.Compat.Normalize (normalizeNFC) where

import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign
import Foreign.C.Types
import Hikaru.Compat.Json (PyString, pyStringFromChars, pyStringToChars)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "unorm2_getNFCInstance"
  getNFC :: Ptr CInt -> IO (Ptr ())
foreign import ccall unsafe "unorm2_normalize"
  normalize :: Ptr () -> Ptr Word16 -> CInt -> Ptr Word16 -> CInt -> Ptr CInt -> IO CInt

{-# NOINLINE nfcInstance #-}
nfcInstance :: Ptr ()
nfcInstance = unsafePerformIO $ alloca $ \status -> do
  poke status 0
  instance_ <- getNFC status
  code <- peek status
  when (code > 0 || instance_ == nullPtr) (ioError (userError "ICU NFC unavailable"))
  pure instance_

{-# NOINLINE normalizeNFC #-}
normalizeNFC :: PyString -> PyString
normalizeNFC input = unsafePerformIO $
  BU.unsafeUseAsCStringLen encoded $ \(source, bytes) -> alloca $ \status -> do
    when (bytes `div` 2 > fromIntegral (maxBound :: CInt)) (ioError (userError "ICU input too long"))
    let run capacity = allocaArray capacity $ \output -> do
          poke status 0
          count <- normalize nfcInstance (castPtr source) (fromIntegral (bytes `div` 2)) output (fromIntegral capacity) status
          code <- peek status
          if code == 15 then run (fromIntegral count + 1) else do
            when (code > 0) (ioError (userError ("ICU normalization error " ++ show code)))
            result <- B.packCStringLen (castPtr output, 2 * fromIntegral count)
            pure (pyStringFromChars (T.unpack (TE.decodeUtf16LE result)))
    run (bytes + 32)
  where encoded = TE.encodeUtf16LE (T.pack (pyStringToChars input))
