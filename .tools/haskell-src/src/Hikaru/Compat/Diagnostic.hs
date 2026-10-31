{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Python exception class/message at display boundaries, without fabricating
-- a Python traceback. Contextual callers supply messages lost by older APIs.
module Hikaru.Compat.Diagnostic
  ( Diagnostic(..), DiagnosticFailure(..), raiseDiagnostic, renderDiagnostic
  , exceptionDiagnostic, ioDiagnostic, namedDiagnostic, base64Diagnostic
  , decodeUtf8Diagnostic, decodeUtf8Chunk, validateUtf8Chunk, encodeUtf8Diagnostic, decodeSealedDiagnostic, readSealedDiagnostic
  , dateDiagnostic ) where
import Control.Exception
import Data.Bits ((.&.))
import Data.Char (ord)
import qualified Data.ByteString as B
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.C.String (CString,peekCString)
import Foreign.C.Types (CInt(..))
import GHC.IO.Exception (IOException(..))
import Hikaru.Compat.Base64
import Hikaru.Compat.Exception (PythonFailure(..))
import Hikaru.Compat.Json
import Hikaru.Compat.Render (pyRepr)
import Data.Time (fromGregorianValid)
import Numeric (showHex)

data Diagnostic = Diagnostic { diagnosticClass :: !Text, diagnosticMessage :: !PyString } deriving (Eq,Show)
newtype DiagnosticFailure = DiagnosticFailure Diagnostic deriving Show
instance Exception DiagnosticFailure
raiseDiagnostic :: Diagnostic -> IO a
raiseDiagnostic = throwIO . DiagnosticFailure
renderDiagnostic :: Diagnostic -> PyString
renderDiagnostic (Diagnostic cls message) = pyStringConcat [pyStringFromText cls,": ",message]
namedDiagnostic :: Text -> Diagnostic
namedDiagnostic cls = Diagnostic cls (case cls of "OverflowError"->"date value out of range";_->"")
base64Diagnostic :: Base64Error -> Diagnostic
base64Diagnostic = Diagnostic "Error" . pyStringFromChars . base64ErrorMessage

-- Called at an already-failing datetime/date constructor boundary.
dateDiagnostic :: Integer -> Integer -> Integer -> Diagnostic
dateDiagnostic year month day=Diagnostic "ValueError" $
  if year<1 || year>9999 then pyStringFromChars("year "++show year++" is out of range")
  else if month<1 || month>12 then "month must be in 1..12"
  else case fromGregorianValid year (fromInteger month)(fromInteger day) of
    Nothing->"day is out of range for month"
    Just _->"invalid date"

foreign import ccall unsafe "strerror" c_strerror :: CInt -> IO CString
-- Explicit source/destination names override the names retained by GHC. This
-- is useful for rename errors where Python includes both paths in its message.
ioDiagnostic :: Maybe PyString -> Maybe PyString -> IOException -> IO Diagnostic
ioDiagnostic given target err = do
  let fallback
        | "is a directory" `isInfixOf` ioe_description err = Just 21
        | otherwise = Nothing
      number = case ioe_errno err of Just n->Just n;Nothing->fallback
      filename = case given of Just p->Just p;Nothing->pyStringFromChars <$> ioe_filename err
  case number of
    Just n -> do
      message <- c_strerror (fromIntegral n) >>= peekCString
      let cls = case n of
            1->"PermissionError";2->"FileNotFoundError";3->"ProcessLookupError";4->"InterruptedError"
            10->"ChildProcessError";13->"PermissionError";17->"FileExistsError";20->"NotADirectoryError"
            21->"IsADirectoryError";32->"BrokenPipeError";35->"BlockingIOError";60->"TimeoutError";_->"OSError"
          suffix = case filename of Nothing->[];Just p->[": ",pyRepr(String p)]
          second = case target of Nothing->[];Just p->[" -> ",pyRepr(String p)]
      pure $ Diagnostic cls (pyStringConcat (["[Errno ",pyStringFromChars(show n),"] ",pyStringFromChars message]++suffix++second))
    Nothing -> pure $ Diagnostic "OSError" (pyStringFromChars $ ioe_description err)

exceptionDiagnostic :: SomeException -> IO Diagnostic
exceptionDiagnostic err
  | Just (DiagnosticFailure d) <- fromException err = pure d
  | Just (PythonFailure cls) <- fromException err = pure (namedDiagnostic cls)
  | Just (io :: IOException) <- fromException err =
      if ioe_description io `elem` ["ValueError","OverflowError","StopIteration"]
      then pure(namedDiagnostic $ T.pack $ ioe_description io)
      else if "Invalid base64-encoded string:" `isInfixOf` ioe_description io || ioe_description io=="Incorrect padding"
      then pure(Diagnostic "Error" $ pyStringFromChars $ ioe_description io)
      else ioDiagnostic Nothing Nothing io
  | otherwise = case err of
      SomeException payload -> pure $ Diagnostic "RuntimeError" (pyStringFromChars $ displayException payload)

-- CPython's strict UTF-8 error span. The lower/upper second-byte bounds reject
-- overlong encodings, surrogates and >U+10FFFF at the same earliest byte.
decodeUtf8Diagnostic :: B.ByteString -> Either Diagnostic Text
decodeUtf8Diagnostic bytes = fst <$> decodeUtf8Chunk True bytes

-- Incremental decoding returns an incomplete trailing sequence only when the
-- caller will read again. Error positions are relative to pending+new bytes,
-- matching TextIOWrapper's incremental decoder instead of whole-file offsets.
decodeUtf8Chunk :: Bool -> B.ByteString -> Either Diagnostic (Text,B.ByteString)
decodeUtf8Chunk final bytes
  -- A read boundary in the middle of a valid code point is ordinary for
  -- Japanese logs. Validate its complete prefix with the fast decoder; only
  -- malformed input needs the byte-by-byte diagnostic scan below.
  | not final, Just start <- incompleteSuffix bytes
  , Right value <- TE.decodeUtf8' (B.take start bytes) =
      Right (value,B.drop start bytes)
  | otherwise = decodeWhole
  where
    decodeWhole = case TE.decodeUtf8' bytes of
      Right value -> Right (value,B.empty)
      Left _ -> case scan 0 of
        Just (start,_,"unexpected end of data") | not final ->
          (\text -> (text,B.drop start bytes)) <$> decodeUtf8Diagnostic (B.take start bytes)
        Just (start,end,reason) -> Left $ Diagnostic "UnicodeDecodeError" $ pyStringFromChars $
          "'utf-8' codec can't decode " ++ (if end==start+1
            then "byte 0x"++hex (at start)++" in position "++show start
            else "bytes in position "++show start++"-"++show(end-1)) ++ ": "++reason
        Nothing -> Left (Diagnostic "UnicodeDecodeError" "invalid UTF-8")
    size=B.length bytes
    at=B.index bytes
    hex n=let s=showHex n "" in replicate(2-length s)'0'++s
    scan i
      | i>=size=Nothing
      | at i<0x80=scan(i+1)
      | at i>=0xc2 && at i<=0xdf=following i 2 0x80 0xbf
      | at i>=0xe0 && at i<=0xef=following i 3 (if at i==0xe0 then 0xa0 else 0x80) (if at i==0xed then 0x9f else 0xbf)
      | at i>=0xf0 && at i<=0xf4=following i 4 (if at i==0xf0 then 0x90 else 0x80) (if at i==0xf4 then 0x8f else 0xbf)
      | otherwise=Just(i,i+1,"invalid start byte")
    following start width low high=go 1
      where
        go k
          | k==width=scan(start+width)
          | start+k>=size=Just(start,size,"unexpected end of data")
          | (k==1 && (at(start+k)<low || at(start+k)>high)) || (k>1 && at(start+k) .&. 0xc0 /= 0x80)=Just(start,start+k,"invalid continuation byte")
          | otherwise=go(k+1)

-- Validate read-ahead bytes without allocating decoded Text that the reader
-- would discard. Match the original incomplete-suffix branch first (including
-- its ED A0..BF two-byte buffering). Any unproven input keeps the full original
-- decoder and exact diagnostic; only positive valid-byte cases bypass it.
validateUtf8Chunk :: Bool -> B.ByteString -> Either Diagnostic B.ByteString
validateUtf8Chunk final bytes
  | not final, Just start <- incompleteSuffix bytes
  , B.isValidUtf8 (B.take start bytes) = Right (B.drop start bytes)
  | B.isValidUtf8 bytes = Right B.empty
  | otherwise = snd <$> decodeUtf8Chunk final bytes

-- Inspect at most the final three bytes. A malformed/in-range-complete suffix
-- takes the unchanged decoder path, preserving its earliest error and span.
incompleteSuffix :: B.ByteString -> Maybe Int
incompleteSuffix bytes = seek (size-1) 0
  where
    size=B.length bytes
    seek :: Int -> Int -> Maybe Int
    seek i trailing
      | i<0 || trailing>2 = Nothing
      | byte>=0x80 && byte<=0xbf = seek (i-1) (trailing+1)
      | byte>=0xc2 && byte<=0xdf = accept i 2 0x80 0xbf
      -- CPython buffers ED A0..BF when only two bytes are available; it
      -- rejects the surrogate encoding when the third byte (or EOF) arrives.
      | byte>=0xe0 && byte<=0xef = accept i 3 (if byte==0xe0 then 0xa0 else 0x80) 0xbf
      | byte>=0xf0 && byte<=0xf4 = accept i 4 (if byte==0xf0 then 0x90 else 0x80) (if byte==0xf4 then 0x8f else 0xbf)
      | otherwise = Nothing
      where byte=B.index bytes i
    accept start width low high
      | size-start>=width = Nothing
      | start+1<size && (B.index bytes(start+1)<low || B.index bytes(start+1)>high) = Nothing
      | otherwise = Just start

decodeSealedDiagnostic :: B.ByteString -> Either Diagnostic Text
decodeSealedDiagnostic bytes = either (Left . base64Diagnostic) decodeUtf8Diagnostic (decodeBase64 bytes)
readSealedDiagnostic :: FilePath -> IO Text
readSealedDiagnostic path = do
  bytes <- B.readFile path `catch` (\(err::IOException)->ioDiagnostic (Just $ pyStringFromChars path) Nothing err >>= raiseDiagnostic)
  either raiseDiagnostic pure (decodeSealedDiagnostic bytes)

-- CPython strict UTF-8 encoder: the failing group is a maximal adjacent run
-- of surrogate code points. Valid input retains Text's normal fast encoder.
encodeUtf8Diagnostic :: PyString -> Either Diagnostic B.ByteString
encodeUtf8Diagnostic value = case pyStringToText value of
  Right text -> Right (TE.encodeUtf8 text)
  Left _ ->
    let chars=pyStringToChars value
        surrogate c=ord c>=0xd800 && ord c<=0xdfff
        (prefix,rest)=break surrogate chars
        invalid=takeWhile surrogate rest
        first=length prefix
        count=length invalid
        detail=case invalid of
          [c]->"character '\\u"++showHex(ord c)""++"' in position "++show first
          _->"characters in position "++show first++"-"++show(first+count-1)
    in Left(Diagnostic "UnicodeEncodeError" $ pyStringFromChars $
      "'utf-8' codec can't encode "++detail++": surrogates not allowed")
