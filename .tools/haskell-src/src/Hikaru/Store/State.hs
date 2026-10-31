{-# LANGUAGE OverloadedStrings #-}
-- | State is checked at ingress. Original checkpoint objects are retained
-- only for their persisted field order, optional fields and number spelling.
module Hikaru.Store.State
  ( emptyProgress, loadProgressValue, prepareProgress
  , Progress, Checkpoint, Stamp, Asked, DayTable, Sessions, Result, require
  , initialProgress, decodeProgress, encodeProgress, preparedProgress
  , decodeAsked, encodeAsked, emptyAsked, statusAsked, rememberDiary
  , askedDays, askedDiary, askedLast, progressDiary, progressSeen, progressReadIds
  , checkpoint, checkpointTime, behind, humanTime, stamp, rawBefore
  , diaryBehind, advanceDiary, advanceShiori, advanceIndex, appendRead
  , decodeSessions, sessionsForRead, progressError, askedError, sessionsError
  ) where

import Control.Exception (throw)
import Hikaru.Compat.Hash (HashContext)
import Control.Monad (filterM, foldM)
import Data.Bifunctor (first)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import Hikaru.Compat.Json
import Hikaru.Conversation.DateTime
import System.Exit (die)

type Result = Either T.Text
type DayTable = [(String, Checkpoint)]
type Sessions = M.Map T.Text (Maybe T.Text)
data Stamp = Stamp T.Text DateTime
data Checkpoint = Checkpoint Value Integer Stamp
data Progress = Progress DayTable DayTable (Maybe Stamp) [T.Text]
data Asked = Asked [(PyString,Value)] (Maybe (Maybe Stamp)) DayTable DayTable

progressError, askedError, sessionsError :: T.Text
progressError = "ValueError: invalid progress file: .タイムマシン/.progress.json"
askedError = "ValueError: invalid status file: asked.json"
sessionsError = "ValueError: invalid sessions file: .タイムマシン/.sessions.json"
require :: Result a -> IO a
require = either (die . T.unpack) pure
fields :: T.Text -> Value -> Result [(PyString,Value)]
fields context value@(Object object) = unicode context value >> Right (objectPairs object)
fields context _ = Left (context <> ": expected an object")
text :: T.Text -> Value -> Result T.Text
text context (String value) = first (const (context <> ": contains an unpaired surrogate")) (pyStringToText value)
text context _ = Left (context <> ": expected a string")
fieldContext :: T.Text -> PyString -> T.Text
fieldContext context key = context <> "[" <> TE.decodeUtf8 (BL.toStrict (A.encode (pyStringToChars key))) <> "]"
-- Retained extension fields must also be writable as UTF-8, before any commit.
unicode :: T.Text -> Value -> Result ()
unicode context value@(String _) = () <$ text context value
unicode context (Array values) = sequence_ [unicode (context <> "[" <> T.pack (show i) <> "]") value | (i,value) <- zip [0::Int ..] (V.toList values)]
unicode context (Object object) = mapM_ (\(key,value) -> let at = fieldContext context key in text at (String key) >> unicode at value) (objectPairs object)
unicode _ _ = Right ()
optional :: (Value -> Result a) -> Value -> Result (Maybe a)
optional _ Null = Right Nothing
optional decode value = Just <$> decode value
stamp :: T.Text -> Result Stamp
stamp value = stampAt "ValueError: invalid timestamp: .タイムマシン/.last" value
stampAt :: T.Text -> T.Text -> Result Stamp
stampAt context value = Stamp value <$> first (const (context <> ": invalid ISO datetime")) (parseDateTime (pyStringFromText value))
readStamp :: T.Text -> Value -> Result Stamp
readStamp context value = text context value >>= stampAt context
stampValue :: Stamp -> Value
stampValue (Stamp value _) = String (pyStringFromText value)
humanTime :: Stamp -> IO String
humanTime (Stamp raw value) = do
  local <- dateTimeToJST value >>= require . first (const ("ValueError: timestamp cannot be converted to JST: " <> raw))
  pure (formatTime defaultTimeLocale "%m-%d %H:%M" local)
compareStamp :: Stamp -> Stamp -> Result Ordering
compareStamp (Stamp _ a) (Stamp _ b) = first (const "TypeError: can't compare offset-naive and offset-aware datetimes") (compareDateTime a b)
rawBefore :: Stamp -> Stamp -> Result Bool
rawBefore a b = (==LT) <$> compareStamp a b

readCheckpoint :: T.Text -> Value -> Result Checkpoint
readCheckpoint context value = do
  object <- fields context value
  let endContext = fieldContext context "until"
  end <- maybe (Left (endContext <> ": missing required field")) (readStamp endContext) (lookup "until" object)
  n <- count (maybe (Number (PyInteger 0)) id (lookup "n" object))
  pure (Checkpoint value n end)
  where
    count (Number (PyInteger n)) | n>=0 = Right n
    count (Number (PyFloat n)) | not (isNaN n || isInfinite n), n>=0, n==fromInteger (truncate n) = Right (truncate n)
    count _ = Left (fieldContext context "n" <> ": expected a non-negative integer")
checkpoint :: T.Text -> Int -> Result Checkpoint
checkpoint end n = readCheckpoint "ValueError: invalid checkpoint" (objectFromPairs [("until",String (pyStringFromText end)),("n",Number (PyInteger (toInteger n)))])
checkpointTime :: Checkpoint -> Stamp
checkpointTime (Checkpoint _ _ end) = end
readDays :: T.Text -> Value -> Result DayTable
readDays context value = fields context value >>= traverse (\(key,v) -> let at = fieldContext context key in (,) <$> (T.unpack <$> text at (String key)) <*> readCheckpoint at v)
writeDays :: DayTable -> Value
writeDays days = objectFromPairs [(pyStringFromChars key,value) | (key,Checkpoint value _ _)<-days]
behind :: Maybe Checkpoint -> Checkpoint -> Result Bool
behind Nothing _ = Right True
behind (Just (Checkpoint _ n end)) (Checkpoint _ current now) = do
  order <- compareStamp end now
  pure (current>n || order==LT)

initialProgress :: Progress
initialProgress = Progress [] [] Nothing []
decodeProgress :: Value -> Result Progress
decodeProgress value = do
  object <- fields progressError value
  let get key = maybe Null id (lookup key object)
      at = fieldContext progressError
      days _ Null = Right []
      days key v = readDays (at key) v
      ids Null = Right []
      ids (Array xs) = sequence [text (at "read_ids" <> "[" <> T.pack (show i) <> "]") v | (i,v) <- zip [0::Int ..] (V.toList xs)]
      ids _ = Left (at "read_ids" <> ": expected an array")
  Progress <$> days "diary" (get "diary") <*> days "shiori" (get "shiori") <*> optional (readStamp (at "index_seen")) (get "index_seen") <*> ids (get "read_ids")
encodeProgress :: Progress -> Value
encodeProgress (Progress diary shiori seen ids) = objectFromPairs
  [("diary",writeDays diary),("shiori",writeDays shiori),("index_seen",maybe Null stampValue seen),
   ("read_ids",Array (V.fromList (map (String . pyStringFromText) ids)))]
preparedProgress :: Progress -> Value
preparedProgress (Progress diary shiori seen ids) = encodeProgress
  (Progress (sortOn fst diary) (sortOn fst shiori) seen (S.toAscList (S.fromList ids)))
progressDiary :: Progress -> DayTable
progressDiary (Progress diary _ _ _) = diary
progressSeen :: Progress -> Maybe Stamp
progressSeen (Progress _ _ seen _) = seen
progressReadIds :: Progress -> S.Set T.Text
progressReadIds (Progress _ _ _ ids) = S.fromList ids
diaryBehind :: Progress -> Result [String]
diaryBehind (Progress diary shiori _ _) = map fst <$> filterM (\(key,cp) -> behind (lookup key shiori) cp) diary
insert :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
insert key value entries
  | key `elem` map fst entries = [(k,if k==key then value else v) | (k,v)<-entries]
  | otherwise = entries++[(key,value)]
advance :: DayTable -> DayTable -> Result (DayTable,[String])
advance seen previous = foldM step (previous,[]) seen
  where
    step (days,moved) (key,cp) = do
      needed <- behind (lookup key days) cp
      pure (if needed then (insert key cp days,moved++[key]) else (days,moved))
advanceDiary :: String -> Checkpoint -> Progress -> Result (Progress,())
advanceDiary key cp (Progress diary shiori seen ids) = do
  (next,_) <- advance [(key,cp)] diary
  pure (Progress next shiori seen ids,())
advanceShiori :: DayTable -> Progress -> Result (Progress,[String])
advanceShiori days (Progress diary shiori seen ids) = do
  (next,moved) <- advance days shiori
  pure (Progress diary next seen ids,moved)
advanceIndex :: Stamp -> Progress -> Result (Progress,Bool)
advanceIndex end p@(Progress diary shiori seen ids) = do
  newer <- maybe (Right True) (`rawBefore` end) seen
  pure (if newer then Progress diary shiori (Just end) ids else p,newer)
appendRead :: [T.Text] -> Progress -> Progress
appendRead added (Progress diary shiori seen ids) = Progress diary shiori seen (ids++added)

emptyAsked :: Asked
emptyAsked = Asked [] Nothing [] []
decodeAsked :: Value -> Result Asked
decodeAsked value = do
  object <- fields askedError value
  let at = fieldContext askedError
      days key = maybe (Right []) (readDays (at key)) (lookup key object)
  Asked object <$> traverse (optional (readStamp (at "last"))) (lookup "last" object) <*> days "days" <*> days "diary"
encodeAsked :: Asked -> Value
encodeAsked (Asked object _ _ _) = objectFromPairs object
askedDays, askedDiary :: Asked -> DayTable
askedDays (Asked _ _ days _) = days
askedDiary (Asked _ _ _ diary) = diary
askedLast :: Asked -> Maybe (Maybe Stamp)
askedLast (Asked _ end _ _) = end
statusAsked :: Maybe Stamp -> DayTable -> DayTable -> Asked
statusAsked end days diary = Asked
  [("last",maybe Null stampValue end),("days",writeDays days),("diary",writeDays diary)] (Just end) days diary
rememberDiary :: String -> Checkpoint -> Asked -> Asked
rememberDiary key cp (Asked object end days diary) =
  let nextDays = if key `elem` map fst days then days else days++[(key,cp)]
      nextDiary = insert key cp diary
  in Asked (insert "diary" (writeDays nextDiary) (insert "days" (writeDays nextDays) object)) end nextDays nextDiary

decodeSessions :: Value -> Result Sessions
decodeSessions value = fields sessionsError value >>= fmap M.fromList . traverse
  (\(key,v) -> let at = fieldContext sessionsError key in (,) <$> text at (String key) <*> optional (text at) v)
sessionsForRead :: Sessions -> [(String,String)] -> (S.Set T.Text,S.Set String)
sessionsForRead sessions = foldl' step (S.empty,S.empty)
  where
    step (ids,unknown) (short,time) = case if short=="----" then Just "----" else M.findWithDefault Nothing (T.pack short) sessions of
      Just full | not (T.null full) -> (S.insert (full<>T.pack (":"++time++"+09:00")) ids,unknown)
      _ -> (ids,S.insert short unknown)

emptyProgress :: Value
emptyProgress = encodeProgress initialProgress

loadProgressValue :: Value -> Value
loadProgressValue = either (throw . userError . T.unpack) encodeProgress . decodeProgress

prepareProgress :: HashContext -> Value -> Either T.Text Value
prepareProgress _ = fmap preparedProgress . decodeProgress

