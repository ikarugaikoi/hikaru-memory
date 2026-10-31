{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Durable health events. Business locks are always outside health.lock.
module Hikaru.OwnTime.Health
  ( Worker(..), Fault(..), Stall(..), EventKind(..), Event(..), Health(..), Attempt(..)
  , watchHealth, markStuck, clearStuck, recoverFile, resumeRepair, readHealth, readStalls
  , Notice(..), wakeNotice, hookNotice, acknowledge, renderNotice, healthStatus
  , recoveryAllowed, checkedJSON, goodPath, writeGood, restoreGood
  ) where

import Control.Exception (Handler(..), IOException, catches, throwIO, mask_)
import Control.Monad (filterM, unless, when)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime, zonedTimeToUTC, utcToLocalTime, minutesToTimeZone)
import GHC.Generics (Generic)
import Hikaru.Compat.Diagnostic (DiagnosticFailure(..), ioDiagnostic)
import Hikaru.Config (configuredNow)
import Hikaru.OwnTime.Errors
import Hikaru.OwnTime.Files
import System.Directory (createDirectoryIfMissing, getModificationTime, removeFile)
import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Posix.Files (getFileStatus)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError, ioeGetFileName)

-- These names are part of the v1 on-disk protocol.
data Worker = Check | Run | Launch | Talk deriving (Eq, Show, Generic)
instance A.ToJSON Worker
instance A.FromJSON Worker

data Fault = Fault { file :: Maybe FilePath, reason :: String } deriving (Eq, Show, Generic)
instance A.ToJSON Fault
instance A.FromJSON Fault

data Stall = Stall
  { worker :: Worker, fault :: Fault, first :: UTCTime, latest :: UTCTime
  , consecutive :: Int, event :: Int
  } deriving (Eq, Show, Generic)
instance A.ToJSON Stall
instance A.FromJSON Stall

data EventKind = Stopped | AutoRecovered | Resumed deriving (Eq, Show, Generic)
instance A.ToJSON EventKind
instance A.FromJSON EventKind

data Event = Event
  { serial :: Int, owner :: Worker, at :: UTCTime, kind :: EventKind
  , problem :: Fault, method :: Maybe String, broken :: Maybe FilePath
  } deriving (Eq, Show, Generic)
instance A.ToJSON Event
instance A.FromJSON Event

data Attempt = Attempt
  { by :: Worker, cause :: Fault, repairs :: Int } deriving (Eq, Show, Generic)
instance A.ToJSON Attempt
instance A.FromJSON Attempt

data Health = Health
  { version :: Int, events :: [Event], attempts :: [Attempt] } deriving (Eq, Show, Generic)
instance A.ToJSON Health
instance A.FromJSON Health

data RepairPlan = RepairPlan { repairEvent :: Event, replacementDigest :: String, originalDigest :: String }
  deriving (Show, Generic)
instance A.ToJSON RepairPlan
instance A.FromJSON RepairPlan

digestOf :: B.ByteString -> String
digestOf bytes = show (hash bytes :: Digest SHA256)

emptyHealth :: Health
emptyHealth = Health 1 [] []

data Marker = Marker { markerVersion :: Int, failures :: [Stall] } deriving (Show, Generic)
instance A.ToJSON Marker
instance A.FromJSON Marker

checkedJSON :: A.FromJSON a => FilePath -> B.ByteString -> IO a
checkedJSON path bytes = either (stateFailure path . ("invalid health JSON: " ++)) pure (A.eitherDecodeStrict' bytes)

readHealth :: FilePath -> IO Health
readHealth dir = do
  let path = dir </> "health.json"
  bytes <- missingAsNothing (B.readFile path)
  case bytes of
    Nothing -> pure emptyHealth
    Just raw -> do
      h <- checkedJSON path raw
      unless (version h == 1) (stateFailure path "unsupported health version")
      pure h

readStalls :: FilePath -> IO [Stall]
readStalls dir = do
  let path = dir </> "stuck"
  bytes <- missingAsNothing (B.readFile path)
  case bytes of
    Nothing -> pure []
    Just raw -> case A.eitherDecodeStrict' raw of
      Right (Marker 1 xs) -> pure xs
      _ -> case TE.decodeUtf8' raw of
        Right text | let (name, detail) = T.breakOn " " text
                   , name `elem` ["state.json", "talk.json", "presence.json"]
                   , not (T.null detail) -> do
            stamp <- getModificationTime path
            let target = dir </> T.unpack name
                why = diagnosticText (fileDiagnostic target (T.unpack (T.drop 1 detail)))
            pure [Stall Check (Fault (Just target) why) stamp stamp 1 0]
        _ -> stateFailure path "invalid stuck marker or unsupported version"

saveStalls :: FilePath -> [Stall] -> IO ()
saveStalls dir [] = do
  _ <- missingAsNothing (removeFile (dir </> "stuck"))
  pure ()
saveStalls dir xs = writeAtomic (dir </> "stuck") (A.encode (Marker 1 xs))

next :: Health -> Int
next h = case reverse (events h) of
  [] -> 1
  e:_ -> serial e + 1

saveHealth :: FilePath -> Health -> IO ()
saveHealth dir h = writeAtomic (dir </> "health.json") (A.encode h)

healthLock :: FilePath -> IO a -> IO a
healthLock dir action = do
  createDirectoryIfMissing True dir
  withLock (dir </> "health.lock") action

markStuck :: Worker -> FilePath -> UTCTime -> Fault -> IO ()
markStuck who dir now why = healthLock dir $ do
  h <- readHealth dir
  xs <- readStalls dir
  let prior = find ((== who) . worker) xs
      same = maybe False ((== why) . fault) prior
      n = next h
      current = case prior of
        Just s | same -> s { latest = now, consecutive = consecutive s + 1 }
        _ -> Stall who why now now 1 n
  -- The marker is written first: a failure must remain visible even if logging fails.
  saveStalls dir (filter ((/= who) . worker) xs ++ [current])
  unless same $ do
    saveHealth dir h { events = events h ++ [Event n who now Stopped why Nothing Nothing] }
    runLog dir now ("Tokimori stopped: " ++ reason why)

clearStuckLocked :: Worker -> FilePath -> UTCTime -> Health -> IO Health
clearStuckLocked who dir now h = do
  xs <- readStalls dir
  case find ((== who) . worker) xs of
    Nothing -> pure h
    Just s -> do
      -- Record resumption before removing the marker.
      runLog dir now ("Tokimori resumed: " ++ reason (fault s))
      let h' = h { events = events h ++ [Event (next h) who now Resumed (fault s) Nothing Nothing] }
      saveHealth dir h'
      saveStalls dir (filter ((/= who) . worker) xs)
      pure h'

clearStuck :: Worker -> FilePath -> UTCTime -> IO ()
clearStuck who dir now = do
  xs <- readStalls dir
  when (any ((== who) . worker) xs) $
    healthLock dir $ readHealth dir >>= clearStuckLocked who dir now >> pure ()

-- Only a genuinely clean invocation resets the automatic-repair streak.
-- A repaired invocation is successful, but is not evidence that corruption stopped.
finishHealth :: Worker -> FilePath -> UTCTime -> Int -> [Fault] -> IO ()
finishHealth who dir now start pending = do
  h <- readHealth dir
  xs <- readStalls dir
  when (any ((== who) . worker) xs || any ((== who) . by) (attempts h)) $ healthLock dir $ do
    current <- readHealth dir
    let recent = filter (\e -> owner e == who && serial e >= start) (events current)
    unless (any ((== Stopped) . kind) recent) $ do
      resumed <- clearStuckLocked who dir now current
      let repaired = pending ++ [problem e | e <- recent, kind e == AutoRecovered]
      saveHealth dir resumed { attempts = filter (\a -> by a /= who || cause a `elem` repaired) (attempts resumed) }

watchHealth :: Worker -> IO a -> IO a
watchHealth who action = do
  dir <- stateDir
  let handleFault why err = do
        now <- zonedTimeToUTC <$> configuredNow
        markStuck who dir now why `catchIOError` \loggingError ->
          hPutStrLn stderr ("RuntimeError: cannot record Tokimori failure: " ++ show loggingError)
        throwIO err
      monitored = do
        h <- readHealth dir
        _ <- readStalls dir
        -- A recovery whose event was committed before a crash is still a
        -- recovery on this invocation; finishing it must not reset its streak.
        pending <- filterM (\why -> case file why of
          Nothing -> pure False
          Just path -> maybe False (const True) <$> missingAsNothing (getFileStatus (path ++ ".repair.json")))
          [cause a | a <- attempts h, by a == who]
        result <- action
        now <- zonedTimeToUTC <$> configuredNow
        finishHealth who dir now (next h) pending
        pure result
  monitored `catches`
    [ Handler (\e@(StateFailure path why) -> handleFault (Fault (Just path) (diagnosticText (fileDiagnostic path why))) e)
    , Handler (\e@(DiagnosticFailure detail) -> handleFault (Fault Nothing (diagnosticText detail)) e)
    , Handler (\(e :: IOException) -> do
        detail <- ioDiagnostic Nothing Nothing e
        handleFault (Fault (ioeGetFileName e) (diagnosticText detail)) e)
    ]

recoveryAllowed :: Worker -> Fault -> Health -> Bool
recoveryAllowed who why h = maybe True ((< 3) . repairs) (find (\a -> by a == who && cause a == why) (attempts h))

-- Caller holds the file's business lock. Compute and validate the replacement
-- before touching the original. Copy exact bad bytes, then atomically replace:
-- there is no crash window in which a missing original looks like a first run.
recoverFile :: Worker -> FilePath -> UTCTime -> FilePath -> String -> String -> IO B.ByteString -> IO B.ByteString
recoverFile who dir now path why how rebuild = do
  let issue = Fault (Just path) (diagnosticText (fileDiagnostic path why))
  h <- readHealth dir
  unless (recoveryAllowed who issue h) $
    stateFailure path (why ++ "; automatic recovery limit reached (3); manual repair required")
  replacement <- rebuild
  original <- B.readFile path
  healthLock dir $ mask_ $ do
    current <- readHealth dir
    unless (recoveryAllowed who issue current) $
      stateFailure path (why ++ "; automatic recovery limit reached (3); manual repair required")
    let n = next current
        archive = dir </> "broken" </> (formatTime defaultTimeLocale "%Y%m%dT%H%M%S%qZ" now ++ "-" ++ show n ++ "-" ++ takeFileName path)
        repaired = Event n who now AutoRecovered issue (Just how) (Just archive)
    createDirectoryIfMissing True (dir </> "broken")
    existing <- missingAsNothing (getFileStatus archive)
    case existing of
      Just _ -> stateFailure path ("recovery archive already exists: " ++ archive)
      Nothing -> pure ()
    -- Stage the replacement before creating an archive (permission / disk errors
    -- must not move or truncate the source).
    writeAtomic (path ++ ".repair") (BL.fromStrict replacement)
    writeAtomic archive (BL.fromStrict original)
    -- A durable intent makes a crash after replacement observable. The receipt
    -- is completed by finishRepair, or the next invocation of this same loader.
    writeAtomic (path ++ ".repair.json") (A.encode (RepairPlan repaired (digestOf replacement) (digestOf original)))
    finishRepair dir path replacement repaired current
  pure replacement

-- Resume only the exact staged replacement named by a durable repair intent.
-- It is called under the same business lock before classifying missing files.
resumeRepair :: FilePath -> FilePath -> IO ()
resumeRepair dir path = do
  pending <- missingAsNothing (B.readFile (path ++ ".repair.json"))
  case pending of
    Nothing -> pure ()
    Just raw -> healthLock dir $ mask_ $ do
      RepairPlan repaired expected originalHash <- checkedJSON (path ++ ".repair.json") raw
      replacement <- B.readFile (path ++ ".repair")
      unless (file (problem repaired) == Just path && digestOf replacement == expected) $
        stateFailure path "invalid staged recovery identity"
      current <- B.readFile path
      unless (digestOf current `elem` [originalHash, expected]) $
        stateFailure path "current state changed since staged recovery; manual repair required"
      case broken repaired of
        Nothing -> stateFailure path "staged recovery has no preserved original"
        Just archive -> do
          unless (takeDirectory archive == dir </> "broken") $
            stateFailure path "staged recovery archive is outside broken directory"
          preserved <- B.readFile archive
          unless (digestOf preserved == originalHash) $
            stateFailure path "staged recovery original bytes do not match"
      h <- readHealth dir
      finishRepair dir path replacement repaired h

finishRepair :: FilePath -> FilePath -> B.ByteString -> Event -> Health -> IO ()
finishRepair dir path replacement repaired h = do
  writeAtomic path (BL.fromStrict replacement)
  let who = owner repaired
      issue = problem repaired
      previous = find (\a -> by a == who && cause a == issue) (attempts h)
      count = maybe 1 ((+1) . repairs) previous
  unless (any ((== broken repaired) . broken) (events h)) $
    saveHealth dir h
      { events = events h ++ [repaired { serial = next h }]
      , attempts = filter (\a -> by a /= who || cause a /= issue) (attempts h) ++ [Attempt who issue count] }
  let message = "Tokimori automatic recovery: " ++ reason issue ++ "; " ++ maybe "" id (method repaired)
                ++ "; broken: " ++ maybe "" id (broken repaired)
      line = TE.encodeUtf8 (T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
        (utcToLocalTime (minutesToTimeZone 540) (at repaired)) ++ " " ++ message))
  logged <- missingAsNothing (B.readFile (dir </> "run.log"))
  -- The archive name identifies this repair. A crash after append but before
  -- receipt removal must not append the same completed log line again.
  unless (maybe False (elem line . B.split 10) logged) (runLog dir (at repaired) message)
  removeFile (path ++ ".repair.json")
  removeFile (path ++ ".repair")

goodPath :: FilePath -> FilePath
goodPath path = path ++ ".good"

-- The validated new body is also the last known good version for the next
-- invocation. Both atomic writes run under the caller's existing business lock.
writeGood :: FilePath -> BL.ByteString -> IO ()
writeGood path bytes = do
  writeIfChanged path bytes
  writeIfChanged (goodPath path) bytes

restoreGood :: FilePath -> (B.ByteString -> Either String a) -> IO B.ByteString
restoreGood path validate = do
  raw <- missingAsNothing (B.readFile (goodPath path))
  case raw of
    Nothing -> stateFailure path "no last known good version is available"
    Just bytes -> either (stateFailure (goodPath path)) (const (pure bytes)) (validate bytes)

data Notice = Notice { through :: Int, consumer :: FilePath, text :: T.Text } deriving (Eq, Show)

readSeen :: FilePath -> IO Int
readSeen path = do
  raw <- missingAsNothing (B.readFile path)
  maybe (pure (-1)) (checkedJSON path) raw

noticeFor :: FilePath -> Bool -> FilePath -> IO (Maybe Notice)
noticeFor dir wake key = do
  h <- readHealth dir
  xs <- readStalls dir
  if null (events h) && null xs then pure Nothing else do
    let path = dir </> "notified" </> key
    seen <- readSeen path
    let changed = filter ((> seen) . serial) (events h)
        shown = if wake then filter ((== AutoRecovered) . kind) changed else changed
        active = if wake then xs else filter ((> seen) . event) xs
        end = maximum (seen : map serial (events h) ++ map event xs)
    pure $ if null shown && null active then Nothing else
      Just (Notice end path (renderNotice xs shown))

wakeNotice :: IO (Maybe Notice)
wakeNotice = stateDir >>= \dir -> noticeFor dir True "mezame.json"

hookNotice :: T.Text -> IO (Maybe Notice)
hookNotice sid = do
  dir <- stateDir
  let digest = show (hash (TE.encodeUtf8 sid) :: Digest SHA256)
  noticeFor dir False ("oshirase-" ++ digest ++ ".json")

acknowledge :: Notice -> IO ()
acknowledge note = do
  dir <- stateDir
  healthLock dir $ do
    createDirectoryIfMissing True (dir </> "notified")
    prior <- readSeen (consumer note)
    writeAtomic (consumer note) (A.encode (max prior (through note)))

renderNotice :: [Stall] -> [Event] -> T.Text
renderNotice xs es = T.unlines $
  [if null xs then "時守は、いまは動いています。" else "時守が止まっています。次の一分にも、また確かめます。"]
  ++ concatMap stallLines xs
  ++ concatMap eventLines es
  where
    stamp = T.pack . formatTime defaultTimeLocale "%m/%d %H:%M" . utcToLocalTime (minutesToTimeZone 540)
    detail why = ["原因: " <> T.pack (reason why)] ++ maybe [] (\p -> ["対象: " <> T.pack p]) (file why)
    stallLines s = ["止まった最初: " <> stamp (first s) <> "／最近: " <> stamp (latest s)
                  <> "／続けて: " <> T.pack (show (consecutive s)) <> " 回"] ++ detail (fault s)
    eventLines e = case kind e of
      AutoRecovered -> ["時守が、自分で立ち直りました（" <> stamp (at e) <> "）。"]
        ++ detail (problem e) ++ maybe [] (\m -> ["復旧: " <> T.pack m]) (method e)
        ++ maybe [] (\p -> ["壊れた状態の保存先: " <> T.pack p]) (broken e)
      Resumed -> ["時守が、また動きはじめました（" <> stamp (at e) <> "）。"]
      Stopped -> if any ((== serial e) . event) xs then [] else detail (problem e)

healthStatus :: IO T.Text
healthStatus = do
  dir <- stateDir
  h <- readHealth dir
  xs <- readStalls dir
  pure $ if null xs && null (events h) then "" else renderNotice xs (filter ((== AutoRecovered) . kind) (events h))
