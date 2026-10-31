{-# LANGUAGE OverloadedStrings #-}
-- | Read -> typed session rows -> pure global merge and closure. Only this
-- module constructs CollectedMessage, after all copies' privacy flags merge.
module Hikaru.Ochibo.Read
  ( CollectReadEnv(..), CollectedMessage, Closure, closureMessages, closureOpen, closureWaiting, closureCutoff
  , collectedMessage, collectedOrder, collectedHidden, collectedClosed
  , SessionRow, Session, sessionRows, sessionOpen, parseSession, mergeSessions
  , excludeRanges, readSession, loadAll, chunks, markClosed, saveable, archived, lastRaw
  , messageKey, gap, parseCollectTimestamp, requireCollectTime, globNames, globLogs, sortMessages
  ) where

import Data.List (sort, sortOn, isSuffixOf)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError)
import Hikaru.Conversation.Input (RowError(..), timestamp, archiveSpace, stripArchive)
import Hikaru.Conversation.Hidden
import Hikaru.Conversation.Line
import Hikaru.Conversation.Types (Speaker)
import Hikaru.Conversation.ReadLines (readTextUniversal, splitLines)
import Hikaru.Choubo.Reader (withLogSource, foldLocated, readState)
import Hikaru.Choubo.Types
import Hikaru.Choubo.JsonLine

data CollectReadEnv = CollectReadEnv
  { collectLogsRoot :: !FilePath
  , collectExcludePath :: !FilePath
  , collectLastPath :: !FilePath
  , collectProjectIdentity :: !Text
  }

data CollectedMessage = CollectedMessage
  { rowMessage :: !Utterance, rowOrder :: !Int, rowHidden :: !Bool, rowClosed :: !(Maybe Bool)
  } deriving (Eq, Show)
collectedMessage :: CollectedMessage -> Utterance
collectedMessage = rowMessage
collectedOrder :: CollectedMessage -> Int
collectedOrder = rowOrder
collectedHidden :: CollectedMessage -> Bool
collectedHidden = rowHidden
collectedClosed :: CollectedMessage -> Maybe Bool
collectedClosed = rowClosed

-- SessionRow and Session cannot be unpacked or manufactured by callers.
newtype SessionRow = SessionRow CollectedMessage deriving Show
data Session = Session [SessionRow] Bool deriving Show
sessionRows :: Session -> [SessionRow]
sessionRows (Session rows _) = rows
sessionOpen :: Session -> Bool
sessionOpen (Session _ opened) = opened

type TimeRange = (UTCTime, Maybe UTCTime)
data Closure = Closure
  { closureMessages :: ![CollectedMessage], closureOpen :: !Int
  , closureWaiting :: !Int, closureCutoff :: !(Maybe UTCTime)
  } deriving Show

gap :: NominalDiffTime
gap = 4 * 3600
requireCollectTime :: Either Text a -> IO a
requireCollectTime = either (ioError . userError . T.unpack) pure
parseCollectTimestamp :: Text -> IO UTCTime
parseCollectTimestamp = requireCollectTime . timestamp

stamp :: CollectedMessage -> UTCTime
stamp = utteranceTime . collectedMessage

excludeRanges :: CollectReadEnv -> IO [TimeRange]
excludeRanges env = do
  text <- readTextUniversal (collectExcludePath env) `catchIOError` \err ->
    if isDoesNotExistError err then pure "" else ioError err
  concat <$> traverse parse (splitLines text)
  where
    parse line = case filter (not . T.null) (T.split archiveSpace line) of
      [a,b] -> do
        start <- parseCollectTimestamp a
        end <- if b == "-" then pure Nothing else Just <$> parseCollectTimestamp b
        pure [(start,end)]
      _ -> pure []

-- IO and pure reads share the byte framing in Choubo.JsonLine.
data Reading = Reading Int Bool [(Int,Utterance)]
data ReadStep = More Reading | Task
initialReading :: Reading
initialReading = Reading 0 False []

stepSession :: Text -> Reading -> Text -> Either Text ReadStep
stepSession identity (Reading n seen rows) line =
  let body = T.dropWhileEnd (== '\n') line
      next = Right (More (Reading (n+1) seen rows))
      handle parsed = case parsed of
        Left BadJSON -> Left "invalid JSON"
        Left (BadRow reason) -> Left (T.pack reason)
        Right (Said _ turn) -> do
          message <- recordedTurn turn
          if T.null (utteranceText message) then next
          else if utteranceTyped message && not seen && speechIsTask (Typed (utteranceText message))
            then Right Task
            else Right (More (Reading (n+1) (seen || utteranceTyped message) ((n,message):rows)))
        Right _ -> next
  in handle (parseLine identity body)

finishSession :: [TimeRange] -> ReadStep -> Session
finishSession _ Task = Session [] False
finishSession ranges (More (Reading _ _ reversed)) =
  let step (out,state) (n,message) =
        let state' = advanceHidden state (classifyUtterance message)
            time = utteranceTime message
            hidden = hiddenByPhrase state' || any (\(a,b) -> a <= time && maybe True (time <=) b) ranges
        in (SessionRow (CollectedMessage message n hidden Nothing):out,state')
      (rows,final) = foldl' step ([],initialHidden) (reverse reversed)
  in Session (reverse rows) (phraseOpen final)

parseSession :: Text -> [TimeRange] -> [Text] -> Either Text Session
parseSession identity ranges = fmap (finishSession ranges) . go initialReading . splitRecords . TE.encodeUtf8 . T.concat . delimit
  where
    -- The public pure API accepts one record/physical line per list item.
    -- Retain existing callers that omit LF; do not add LF after the last item.
    delimit [] = []
    delimit [line] = [line]
    delimit (line:rest) = (if T.isSuffixOf "\n" line then line else line<>"\n") : delimit rest
    go state [] = Right (More state)
    go state (record:rest) =
      let raw = TE.decodeUtf8 (recordBytes record)
      in case classifyRecord (recordTerminated record) (recordQuote record) raw of
        CompleteJson -> stepSession identity state raw >>= \next -> case next of
          Task -> Right Task
          More state' -> go state' rest
        IncompleteJson | not (recordTerminated record) -> Right (More state)
        _ -> Left (jsonLineMessage "<session>" (recordLine record))

readSession :: CollectReadEnv -> FilePath -> [TimeRange] -> IO Session
readSession env path ranges = withLogSource path $ \source -> do
  result <- foldLocated source ReadAhead8192DeferredCR (ResumePoint 0 0 False) step (More initialReading)
  pure (finishSession ranges (readState result))
  where
    step Task _ = pure (Finish Task)
    step (More state) line = do
      next <- requireCollectTime (stepSession (collectProjectIdentity env) state (locatedText line))
      pure (case next of Task -> Finish Task; More _ -> Continue next)

data MessageKey = UUID Text | Body Text Speaker Text deriving (Eq, Ord, Show)
messageKey :: CollectedMessage -> MessageKey
messageKey row = case utteranceUUID m of
  Just uuid -> UUID uuid
  Nothing -> Body (utteranceRawTime m) (utteranceSpeaker m) (utteranceText m)
  where m = collectedMessage row

-- Directory matches remain matches; a later reader decides whether a file
-- is required. A not-yet-created root is the only IO failure meaning no matches.
globNames :: FilePath -> IO [FilePath]
globNames root = listDirectory root `catchIOError` (\err ->
    if isDoesNotExistError err
    then pure [] else ioError err)

globLogs :: FilePath -> IO [FilePath]
globLogs root = do
  names <- globNames root
  pure [root </> name | name <- sort names, ".jsonl" `isSuffixOf` name]

loadAll :: CollectReadEnv -> IO ([CollectedMessage],Int)
loadAll env = do
  ranges <- excludeRanges env
  files <- globLogs (collectLogsRoot env)
  mergeSessions <$> traverse (\path -> readSession env path ranges) files

mergeSessions :: [Session] -> ([CollectedMessage],Int)
mergeSessions sessions =
  let add (next,positions,rows) (SessionRow row) = case M.lookup (messageKey row) positions of
        Nothing -> (next+1,M.insert (messageKey row) next positions,IM.insert next row rows)
        Just index -> (next,positions,IM.adjust (\first -> first {rowHidden = rowHidden first || rowHidden row}) index rows)
      (_,_,merged) = foldl' add (0,M.empty,IM.empty) (concatMap sessionRows sessions)
  in (sortMessages (IM.elems merged),length (filter sessionOpen sessions))

sortMessages :: [CollectedMessage] -> [CollectedMessage]
sortMessages = sortOn (\row -> (stamp row, collectedOrder row))

-- Map supplies lookup, the index preserves first-seen session order for ties.
indexedChunks :: [CollectedMessage] -> [NonEmpty (Int,CollectedMessage)]
indexedChunks messages = sortOn (stamp . snd . NE.head) (concatMap splitGroup (IM.elems groups))
  where
    (_,_,groups) = foldl' add (0,M.empty,IM.empty) (zip [0..] messages)
    add (next,positions,acc) row@(_,message) =
      let sid = utteranceSession (collectedMessage message)
      in case M.lookup sid positions of
        Nothing -> (next+1,M.insert sid next positions,IM.insert next [row] acc)
        Just index -> (next,positions,IM.adjust (row:) index acc)
    splitGroup backwards = case sortOn (\(_,row) -> (stamp row,collectedOrder row)) (reverse backwards) of
      [] -> []
      x:xs -> let (done,current) = foldl' step ([],x :| []) xs
              in reverse (NE.reverse current:done)
    step (done,current@(previous :| _)) row
      | diffUTCTime (stamp (snd row)) (stamp (snd previous)) >= gap = (NE.reverse current:done,row :| [])
      | otherwise = (done,row NE.<| current)

chunks :: [CollectedMessage] -> [[CollectedMessage]]
chunks = map (map snd . NE.toList) . indexedChunks

markClosed :: [CollectedMessage] -> UTCTime -> Closure
markClosed messages now =
  let cs = indexedChunks messages
      wait = [not (null (seen c)) && diffUTCTime now (stamp (snd (NE.last c))) < gap | c <- cs]
      cutoff = minimumMaybe [stamp (snd first) | (c,True) <- zip cs wait, first:_ <- [seen c]]
      (finalWait,nWait,finalCutoff) = closeOverlaps cs wait 0 cutoff
      statuses = IM.fromList [(i,not w) | (c,w) <- zip cs finalWait, (i,_) <- NE.toList c]
      updated = [m {rowClosed=Just (statuses IM.! i)} | (i,m) <- zip [0..] messages]
  in Closure updated (length (filter id wait)) nWait finalCutoff
  where
    minimumMaybe [] = Nothing
    minimumMaybe xs = Just (minimum xs)
    seen = filter (not . collectedHidden . snd) . NE.toList
    closeOverlaps _ wait count Nothing = (wait,count,Nothing)
    closeOverlaps cs wait count (Just cutoff) =
      let (reversed,added,next) = foldl' visit ([],0,cutoff) (zip cs wait)
          wait' = reverse reversed
      in if added == 0 then (wait',count,Just next)
         else closeOverlaps cs wait' (count+added) (Just next)
    visit (wait,added,cutoff) (_,True) = (True:wait,added,cutoff)
    visit (wait,added,cutoff) (c,False) = case seen c of
      [] -> (False:wait,added,cutoff)
      first:rest | stamp (snd (last (first:rest))) >= cutoff ->
        (True:wait,added+1,min cutoff (stamp (snd first)))
      _ -> (False:wait,added,cutoff)

saveable :: Closure -> Maybe UTCTime -> UTCTime -> [CollectedMessage]
saveable closure after now =
  [row | row <- closureMessages closure, collectedClosed row == Just True
       , maybe True (stamp row >) after
       , maybe (stamp row <= addUTCTime (-60) now) (stamp row <) (closureCutoff closure)]

lastRaw :: CollectReadEnv -> IO (Maybe Text)
lastRaw env = do
  value <- stripArchive <$> (readTextUniversal (collectLastPath env) `catchIOError` \err ->
    if isDoesNotExistError err then pure "" else ioError err)
  pure (if T.null value then Nothing else Just value)

archived :: Closure -> Maybe UTCTime -> UTCTime -> [CollectedMessage]
archived closure done now = case done of
  Just lastTime -> filter ((<=lastTime) . stamp) (closureMessages closure)
  Nothing -> saveable closure Nothing now
