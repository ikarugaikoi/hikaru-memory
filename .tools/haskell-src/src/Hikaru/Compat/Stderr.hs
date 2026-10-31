{-# LANGUAGE OverloadedStrings #-}
-- | CPython's default UTF-8 stderr boundary. stdout and persisted text retain
-- their separate strict encoders; only stderr uses backslashreplace.
module Hikaru.Compat.Stderr (encodeStderr,writeStderr,writeStderrLine) where
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char (ord)
import Numeric (showHex)
import System.IO (stderr)
import Hikaru.Compat.Json (PyString,pyStringToChars,pyStringConcat)

encodeStderr :: PyString -> B.ByteString
encodeStderr=TE.encodeUtf8 . T.pack . concatMap scalar . pyStringToChars
  where scalar c | ord c>=0xd800 && ord c<=0xdfff="\\u"++showHex(ord c)""
                 | otherwise=[c]

writeStderr :: PyString -> IO ()
writeStderr=B.hPut stderr . encodeStderr

writeStderrLine :: PyString -> IO ()
writeStderrLine message=writeStderr(pyStringConcat[message,"\n"])
