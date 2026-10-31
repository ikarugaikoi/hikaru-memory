{-# LANGUAGE OverloadedStrings #-}
-- | Positioned, strictly decoded source reads. OpenedLog never writes source.
module Hikaru.Choubo.Reader
  ( OpenedLog, withLogSource, readLocated, sourceStamp, sourceRegular, sourcePath
  , readRange, readCounts, readDetailedCounts, ReadResult(..), foldLocated, contextDigest, validateStopped
  ) where

import Control.Exception (bracket, finally, onException)
import Control.Monad (unless,when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Hikaru.Compat.Diagnostic (Diagnostic(..),decodeUtf8Diagnostic,validateUtf8Chunk,raiseDiagnostic)
import Hikaru.Compat.Json (pyStringFromText)
import Hikaru.Compat.SHA256 (sha256)
import Hikaru.Choubo.Types
import Hikaru.Choubo.JsonLine
import System.IO
import System.Posix.Files hiding (fileSize)
import qualified System.Posix.Files as PF
import System.Posix.IO
import System.Posix.Types (Fd)

data OpenedLog = OpenedLog FilePath Fd Handle (IORef Integer) (IORef Integer) (IORef Integer)
sourcePath :: OpenedLog -> FilePath
sourcePath (OpenedLog path _ _ _ _ _) = path

withLogSource :: FilePath -> (OpenedLog -> IO a) -> IO a
withLogSource path = bracket acquire release
  where
    acquire = do
      fd <- openFd path ReadOnly defaultFileFlags{cloexec=True}
      handle <- fdToHandle fd `onException` closeFd fd
      (do hSetBinaryMode handle True
          hSetBuffering handle NoBuffering
          OpenedLog path fd handle <$> newIORef 0 <*> newIORef 0 <*> newIORef 0) `onException` hClose handle
    release (OpenedLog _ _ handle _ _ _) = hClose handle

contextDigest :: [B.ByteString] -> IO Text
contextDigest parts = T.pack <$> sha256 (B.concat [BC.pack(show(B.length b)++":")<>b|b<-parts])

sourceStamp :: OpenedLog -> IO FileStamp
sourceStamp (OpenedLog _ fd _ _ _ _) = do
  s <- getFdStatus fd
  identity <- contextDigest [BC.pack(show(deviceID s)),BC.pack(show(fileID s)),BC.pack(show(fileMode s))]
  pure (FileStamp identity (fromIntegral$fileSize' s) (round$modificationTimeHiRes s*1000000000) (round$statusChangeTimeHiRes s*1000000000))
  where fileSize' = PF.fileSize

sourceRegular :: OpenedLog -> IO Bool
sourceRegular (OpenedLog _ fd _ _ _ _) = isRegularFile <$> getFdStatus fd

readCounts :: OpenedLog -> IO (Integer,Integer)
readCounts (OpenedLog _ _ _ count materialized _) = (,) <$> readIORef count <*> readIORef materialized

readDetailedCounts :: OpenedLog -> IO (Integer,Integer,Integer)
readDetailedCounts (OpenedLog _ _ _ count materialized streamed) =
  (,,) <$> readIORef count <*> readIORef streamed <*> readIORef materialized

readRange :: OpenedLog -> Integer -> Integer -> IO B.ByteString
readRange (OpenedLog _ _ handle count _ _) begin size = do
  unless (begin>=0 && size>=0 && size<=toInteger(maxBound::Int)) (ioError$userError"invalid source byte range")
  old <- hTell handle
  (do hSeek handle AbsoluteSeek begin
      bytes <- B.hGet handle (fromInteger size)
      modifyIORef' count (+toInteger(B.length bytes))
      pure bytes) `finally` hSeek handle AbsoluteSeek old

readLocated :: OpenedLog -> RowSpan -> IO LocatedLine
readLocated source@(OpenedLog _ _ _ _ count _) span_ = do
  bytes <- readRange source (rowBegin span_) (rowEnd span_-rowBegin span_)
  unless(toInteger(B.length bytes)==rowEnd span_-rowBegin span_) (ioError$userError"source row truncated")
  modifyIORef' count (+toInteger(B.length bytes))
  text <- either raiseDiagnostic pure (decodeUtf8Diagnostic bytes)
  pure (LocatedLine span_ text)

data ReadResult s = ReadResult
  { readState :: s, readPrefixState :: s, readResume :: ResumePoint
  , readStopped :: Bool, readDecodedThrough :: Integer, readTail :: Bool }

-- A stopped consumer did not ask for another decoder read. In particular a
-- pending UTF-8 suffix at short EOF is not finalized before the task return.
validateStopped :: OpenedLog -> ReadPolicy -> Integer -> IO Integer
validateStopped source policy through=do
  let block=if policy==Line64K then 65536 else 8192
      anchor=max 0 ((through-1) `div` block*block)
  prior<-if policy==Line64K then pure B.empty else
    pendingPrefix <$> readRange source(max 0(anchor-3))(min 3 anchor)
  bytes<-readRange source anchor block
  when(policy/=Line64K) $ either raiseDiagnostic (const$pure()) (validateUtf8Chunk False(prior<>bytes))
  pure(anchor+toInteger(B.length bytes))

pendingPrefix :: B.ByteString -> B.ByteString
pendingPrefix before =
  let trailing=B.reverse(B.takeWhile(\b->b>=128&&b<192)(B.reverse before))
      n=B.length before-B.length trailing
  in if n<=0 then B.empty else
     let lead=B.index before(n-1);suffix=B.drop(n-1)before
         width | lead>=240=4 | lead>=224=3 | lead>=192=2 | otherwise=1
     in if B.length suffix<width then suffix else B.empty

-- Restored state is before resumeByte. EOF's unterminated row is returned for
-- this invocation, but its state never replaces that stable checkpoint.
foldLocated :: OpenedLog -> ReadPolicy -> ResumePoint
  -> (s -> LocatedLine -> IO (Step s)) -> s -> IO (ReadResult s)
foldLocated source@(OpenedLog _ _ handle counter _ streamed) policy origin step initial = do
  let block = if policy==Line64K then 65536 else 8192
      anchor = if policy==Line64K then resumeByte origin else resumeByte origin `div` 8192*8192
  prior <- if anchor==0 || policy==Line64K then pure B.empty else do
    before <- readRange source (max 0 (anchor-3)) (min 3 anchor)
    pure (pendingPrefix before)
  regular<-sourceRegular source
  when regular (hSeek handle AbsoluteSeek anchor)
  loop block anchor prior initial initial origin [] (resumeByte origin) Outside 0
  where
    finish s stable point stopped decoded tail_ = pure(ReadResult s stable point stopped decoded tail_)
    nextBlock block absolute pending = do
      bytes <- B.hGetSome handle block
      modifyIORef' counter (+toInteger(B.length bytes))
      modifyIORef' streamed (+toInteger(B.length bytes))
      let end=absolute+toInteger(B.length bytes)
      nextPending <- if policy==Line64K then pure B.empty else
        either raiseDiagnostic pure (validateUtf8Chunk False (pending<>bytes))
      pure (bytes,end,nextPending)
    loop block absolute pending state stable point pieces lineStart quote lines_ = do
      (bytes,end,nextPending) <- nextBlock block absolute pending
      if B.null bytes then do
        let raw = B.concat (reverse pieces)
        if quote /= Outside then do
          -- A writer may stop inside both a string and its final UTF-8 scalar.
          -- Validate the available prefix, leaving only that scalar pending.
          _ <- either raiseDiagnostic pure (validateUtf8Chunk False raw)
          finish state stable point False absolute True
        else do
          when (policy /= Line64K) $
            either raiseDiagnostic (const (pure ())) (validateUtf8Chunk True pending)
          if null pieces then finish state stable point False absolute False else do
            line <- decodeLine raw
            case classifyRecord False quote line of
              IncompleteJson -> finish state stable point False absolute True
              InvalidJson -> invalid point
              CompleteJson -> do
                result <- step state (LocatedLine(RowSpan lineStart absolute(resumeRow point))line)
                case result of
                  Continue lastState -> finish lastState stable point False absolute True
                  Finish lastState -> finish lastState stable point True absolute True
      else do
        let skipped=max 0 (resumeByte origin-absolute)
            remaining=B.drop(fromInteger skipped)bytes
            start=absolute+min skipped(toInteger$B.length bytes)
        consume block end nextPending state stable point pieces lineStart quote lines_ start remaining
    consume block end pending state stable point pieces lineStart quote lines_ absolute bytes
      | B.null bytes = loop block end pending state stable point pieces lineStart quote lines_
      | otherwise = case recordBoundary quote bytes of
          (nextQuote,moreLines,Nothing) ->
            loop block end pending state stable point (bytes:pieces) lineStart nextQuote (lines_+moreLines)
          (_,moreLines,Just n) -> do
            let raw = B.take (n+1) bytes
                physicalEnd = absolute+toInteger n+1
                remaining = B.drop (n+1) bytes
            line <- decodeLine (B.concat (reverse (raw:pieces)))
            unless (classifyRecord True Outside line == CompleteJson) (invalid point)
            result <- step state (LocatedLine (RowSpan lineStart physicalEnd (resumeRow point)) line)
            let next = ResumePoint physicalEnd (resumeRow point+toInteger (lines_+moreLines)) False
            case result of
              Finish s -> finish s s next True end False
              Continue s -> consume block end pending s s next [] physicalEnd Outside 0 physicalEnd remaining
    decodeLine = either raiseDiagnostic pure . decodeUtf8Diagnostic
    invalid point = raiseDiagnostic (Diagnostic "JSONDecodeError"
      (pyStringFromText (jsonLineMessage (sourcePath source) (resumeRow point+1))))
