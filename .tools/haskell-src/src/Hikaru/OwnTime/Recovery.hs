{-# LANGUAGE OverloadedStrings #-}
module Hikaru.OwnTime.Recovery
  ( Policy(..), loadHealing, parseJSON, readDone, validateDone, restoreDoneBytes, DoneFile(..) ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Scientific (toBoundedInteger)
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Hikaru.Choubo.JsonLine (JsonLine(..), classifyJsonLine)
import Hikaru.OwnTime.Errors (stateFailure)
import Hikaru.OwnTime.Health
import Hikaru.OwnTime.Runtime (Load(..), loadParsed, missingAsNothing)
import System.FilePath ((</>))

data Policy = LastGood | Rebuild String (IO B.ByteString)

parseJSON :: (A.Object -> Either String a) -> B.ByteString -> Either String a
parseJSON parse bytes = do
  value <- A.eitherDecodeStrict' bytes
  case value of
    A.Object object -> parse object
    _ -> Left "expected a JSON object"

loadHealing :: Worker -> FilePath -> UTCTime -> FilePath -> (A.Object -> Either String a) -> a -> Policy -> IO a
loadHealing who dir now path parse initial policy = do
  resumeRepair dir path
  loaded <- loadParsed parse path
  case loaded of
    Absent -> pure initial
    Got value -> pure value
    Bad why -> do
      let (how, rebuild) = case policy of
            LastGood -> ("restore last known good version", restoreGood path (parseJSON parse))
            Rebuild method action -> (method, action)
      raw <- recoverFile who dir now path why how $ do
        bytes <- rebuild
        either (stateFailure path) (const (pure bytes)) (parseJSON parse bytes)
      either (stateFailure path) pure (parseJSON parse raw)

data DoneFile = DoneFile { doneBytes :: B.ByteString, doneRows :: [A.Object] }

-- Each row keeps its original bytes. A torn final JSON value is the sole
-- discardable row; syntactically invalid tails and complete bad rows are errors.
data Row = Valid B.ByteString A.Object | Torn | Corrupt String

isTorn :: Row -> Bool
isTorn Torn = True
isTorn _ = False

-- A write can end inside a UTF-8 code point as well as inside JSON syntax.
-- Complete only that final code point, then ask the shared JSON prefix parser;
-- malformed UTF-8 elsewhere or an unquoted non-ASCII value still fails.
unfinishedUtf8JSON :: B.ByteString -> Bool
unfinishedUtf8JSON raw = any unfinished [1 .. min 3 (B.length raw)]
  where
    unfinished size =
      let suffix = B.drop (B.length raw - size) raw
          lead = B.head suffix
          width | lead >= 0xc2 && lead <= 0xdf = 2
                | lead >= 0xe0 && lead <= 0xef = 3
                | lead >= 0xf0 && lead <= 0xf4 = 4
                | otherwise = 0
          firstByte | lead == 0xe0 = 0xa0
                    | lead == 0xf0 = 0x90
                    | otherwise = 0x80
          padding = if size == 1 then B.singleton firstByte <> B.replicate (max 0 (width-size-1)) 0x80
                    else B.replicate (max 0 (width-size)) 0x80
      in width > size && case TE.decodeUtf8' (raw <> padding) of
          Right text -> classifyJsonLine text == IncompleteJson
          Left _ -> False

rows :: B.ByteString -> [Row]
rows bytes = zipWith parse [1 :: Int ..] physical
  where
    physical = if B.null bytes then [] else
      let parts = BC.split '\n' bytes in if B.last bytes == 10 then init parts else parts
    terminated = not (B.null bytes) && B.last bytes == 10
    parse n raw =
      let bad why = Corrupt ("line " ++ show n ++ ": " ++ why)
          final = n == length physical && not terminated
      in case TE.decodeUtf8' raw of
        Left _ | final && unfinishedUtf8JSON raw -> Torn
               | otherwise -> bad "invalid UTF-8"
        Right text -> case classifyJsonLine text of
          IncompleteJson | final -> Torn
          CompleteJson -> case A.eitherDecodeStrict' raw of
            Right (A.Object object) | Just (A.Number value) <- KM.lookup "run" object
                                   , Just runId <- (toBoundedInteger value :: Maybe Int), runId >= 0 -> Valid raw object
            _ -> bad "expected a completion object with a nonnegative integer run"
          _ -> bad "invalid JSON"

validateDone :: B.ByteString -> Either String DoneFile
validateDone bytes = do
  checked <- mapM complete (rows bytes)
  let valid = [(raw, object) | Just (raw, object) <- checked]
      body = B.concat [raw <> "\n" | (raw, _) <- valid]
  pure (DoneFile body (map snd valid))
  where
    complete (Corrupt why) = Left why
    complete Torn = Right Nothing
    complete (Valid raw object) = Right (Just (raw, object))

-- Restore damaged positions only if the backup proves them. Every existing
-- valid row, including good rows after the damage or beyond the backup, stays
-- byte-for-byte. A stale or conflicting backup cannot erase newer good rows.
restoreDone :: FilePath -> B.ByteString -> IO B.ByteString
restoreDone path original = do
  saved <- restoreGood path $ \bytes -> do
    valid <- validateDone bytes
    if any isTorn (rows bytes) then Left "last known good completion file has an unfinished row" else Right valid
  either (stateFailure path) pure (restoreDoneBytes original saved)

restoreDoneBytes :: B.ByteString -> B.ByteString -> Either String B.ByteString
restoreDoneBytes original saved = do
  _ <- validateDone saved
  if any isTorn (rows saved) then Left "last known good completion file has an unfinished row" else pure ()
  let backupRows = rows saved
      choose n current = case current of
        Valid raw _ -> case drop n backupRows of
          Valid previous _ : _ | previous /= raw -> Left ("line " ++ show (n+1) ++ ": backup conflicts with an existing good row")
          _ -> pure (raw <> "\n")
        Torn -> pure B.empty
        Corrupt why -> case drop n backupRows of
          Valid raw _ : _ -> pure (raw <> "\n")
          _ -> Left (why ++ "; last known good version cannot recover this row")
  restored <- mapM (uncurry choose) (zip [0..] (rows original))
  let suffix = drop (length (rows original)) backupRows
  pure (B.concat restored <> B.concat [raw <> "\n" | Valid raw _ <- suffix])

readDone :: FilePath -> UTCTime -> IO DoneFile
readDone dir now = do
  let path = dir </> "done.jsonl"
  resumeRepair dir path
  bytes <- missingAsNothing (B.readFile path)
  case bytes of
    Nothing -> pure (DoneFile B.empty [])
    Just raw -> case validateDone raw of
      Right valid | not (any isTorn (rows raw)) -> pure valid
                  | otherwise -> recover "unfinished final JSONL row" "keep all complete rows; remove unfinished final row" (pure (doneBytes valid))
      Left why -> recover why "restore damaged completion rows from last known good version; preserve all good rows" (restoreDone path raw)
  where
    recover why how rebuild = do
      let path = dir </> "done.jsonl"
      raw <- recoverFile Run dir now path why how rebuild
      either (stateFailure path) pure (validateDone raw)
