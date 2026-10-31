{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | 引擎の状態をファイルに置いて、戻す（2026-10-28。時守が本当に走るようになったので、R、U、借り、
-- 材料ごとの慣れと飽き、先行量 A、ドリフトを、運行と運行のあいだで持ち越すため）。
--
-- 形は JSON（engine.json）。数は 'Unit' と 'Minutes' の確かめを通して戻す：範囲の外の数、NaN、無限大は
-- 戻らない（'Left' で止まる。黙って直さない）。疲れのほうは 'Hikaru.OwnTime.Fatigue.restoreFatigue' が確かめる。
-- 版（v）を持つ。知らない版は戻さない。
module Hikaru.OwnTime.Store
  ( engineValue, parseEngine, originText, originOf, ledgerValue, parseLedger, placeOf, placeFields,
    situationOf, describeMark, describeChange, TalkFile(..), emptyTalk, parseTalk, talkValue, observedOf, Marks(..),
    emptyMarks, marksValue, parseMarks, settleMarks, parseState, stateValue, describe, RunnerFile(..),
    emptyRunnerFile, runnerValue, parseRunner, segKeyText, Parse, parseStamp, textField, stampValue, field,
    stamp, stampWith, stampOrNull, nullableField, bool, count, textOrNull, objectOf, objectOrNull, textOf,
    intOf
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, ZonedTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Hikaru.OwnTime.Engine
import Hikaru.OwnTime.Fatigue
import Hikaru.OwnTime.Michikusa (MaterialId(..), SegmentId(..), originOf, originText)
import Hikaru.OwnTime.Trigger
import Hikaru.OwnTime.Unit
import System.Posix.Types (ProcessID)

-- 書く ---------------------------------------------------------------------------

engineValue :: Engine -> A.Value
engineValue e = A.object
  [ "v" A..= (1 :: Int)
  , "fatigue" A..= fatigueValue (snapshotFatigue (enFatigue e))
  , "source" A..= A.object [ "related" A..= slRelated (enSource e), "all" A..= slAll (enSource e), "unknown" A..= slUnknown (enSource e) ]
  , "materials" A..= A.object [ K.fromText (materialText k) A..= materialValue m | (k, m) <- Map.toList (enMaterials e) ]
  , "activity" A..= maybe A.Null activityValue (enActivity e)
  , "advance" A..= [ A.object [ "material" A..= pendMaterial q, "minutes" A..= pendMinutes q ] | q <- enAdvance e ]
  , "clock" A..= enClock e
  , "busy" A..= [ A.object [ "start" A..= ivStart iv, "end" A..= ivEnd iv ] | iv <- enBusy e ]
  , "rechosen" A..= Set.toList (enRechosen e)
  ]

fatigueValue :: FatigueSnapshot -> A.Value
fatigueValue sn = A.object
  [ "r" A..= snapR sn, "u" A..= snapU sn, "debt" A..= snapDebt sn
  , "windowUsed" A..= snapWindowUsed sn, "windowCap" A..= snapWindowCap sn
  , "dayReport" A..= snapDayReport sn
  , "dayFrozen" A..= fmap (\(um, b, d) -> A.object [ "um" A..= um, "bDay" A..= b, "dStart" A..= d ]) (snapDayFrozen sn)
  , "dayEffort" A..= snapDayEffort sn, "dayUsed" A..= snapDayUsed sn ]

materialValue :: MaterialState -> A.Value
materialValue m = A.object
  [ "f" A..= unitValue (famF (msFam m)), "s" A..= unitValue (famS (msFam m))
  , "updated" A..= msUpdated m, "origin" A..= originText (msOrigin m)
  , "interest" A..= unitValue (msInterest m), "burden" A..= unitValue (msBurden m) ]

activityValue :: Activity -> A.Value
activityValue a = A.object
  [ "material" A..= acMaterial a
  , "driftX" A..= driftX (acDrift a)
  , "stage" A..= (case driftStage (acDrift a) of { FirstRun -> "first" :: Text; Rechosen -> "rechosen" })
  , "unintegrated" A..= acUnintegrated a
  , "line" A..= (case acLine a of { NotYet -> "notYet" :: Text; FirstCrossing -> "first"; SecondCrossing -> "second" }) ]

-- 読む ---------------------------------------------------------------------------

number :: K.Key -> A.Object -> Parse Double
number k o = field k o >>= \v -> case v of
  A.Number n -> let x = realToFrac n in if isNaN x || isInfinite x then Left (K.toString k ++ ": expected a finite number") else Right x
  _ -> Left (K.toString k ++ ": expected a finite number")

unitField :: K.Key -> A.Object -> Parse Unit
unitField k o = number k o >>= \x -> maybe (Left (K.toString k ++ ": expected a number in [0, 1]")) Right (unit x)

nonNeg :: K.Key -> A.Object -> Parse Double
nonNeg k o = number k o >>= \x -> if x < 0 then Left (K.toString k ++ ": expected a nonnegative number") else Right x

array :: K.Key -> A.Object -> Parse [A.Value]
array k o = field k o >>= \v -> case v of
  A.Array xs -> Right (foldr (:) [] xs)
  _ -> Left (K.toString k ++ ": expected an array")

asObject :: String -> A.Value -> Parse A.Object
asObject what v = case v of
  A.Object x -> Right x
  _ -> Left (what ++ ": invalid value")

parseEngine :: Params -> A.Object -> Parse Engine
parseEngine p o = case KM.lookup "v" o of
  Just (A.Number 1) -> do
    fat <- objectOf "fatigue" o >>= parseFatigue
    src <- objectOf "source" o >>= \s -> SourceLedger <$> nonNeg "related" s <*> nonNeg "all" s <*> nonNeg "unknown" s
    mats <- objectOf "materials" o >>= \ms -> traverse (\(k, v) -> asObject ("material " ++ K.toString k) v >>= parseMaterial >>= \m -> Right (MaterialId (K.toText k), m)) (KM.toList ms)
    act <- objectOrNull "activity" o >>= traverse parseActivity
    adv <- array "advance" o >>= traverse (\v -> asObject "advance" v >>= \a -> Pending <$> (MaterialId <$> textOf "material" a) <*> nonNeg "minutes" a)
    clock <- nonNeg "clock" o
    busy <- array "busy" o >>= traverse (\v -> asObject "busy" v >>= \iv -> Interval <$> nonNeg "start" iv <*> nonNeg "end" iv)
    rechosen <- array "rechosen" o >>= traverse (\v -> case v of { A.String t -> Right (MaterialId t); _ -> Left "rechosen: expected a string" })
    Right (Engine fat src (Map.fromList mats) act adv clock busy (Set.fromList rechosen))
  _ -> Left "v: unsupported version"
  where
    parseFatigue f = do
      sn <- FatigueSnapshot <$> number "r" f <*> number "u" f <*> number "debt" f <*> number "windowUsed" f <*> number "windowCap" f
              <*> (field "dayReport" f >>= \v -> case v of { A.Null -> Right Nothing; A.Number n -> Right (Just (realToFrac n)); _ -> Left "dayReport: invalid value" })
              <*> (objectOrNull "dayFrozen" f >>= traverse (\z -> (,,) <$> number "um" z <*> number "bDay" z <*> number "dStart" z))
              <*> number "dayEffort" f <*> number "dayUsed" f
      restoreFatigue p sn
    parseMaterial m = do
      f <- unitField "f" m
      s <- unitField "s" m
      upd <- nonNeg "updated" m
      origin <- textOf "origin" m >>= \t -> maybe (Left "origin: invalid value") Right (originOf t)
      interest <- unitField "interest" m
      burden <- unitField "burden" m
      Right (MaterialState (Familiarity f s) upd origin interest burden)
    parseActivity a = do
      mat <- MaterialId <$> textOf "material" a
      x <- number "driftX" a
      stage <- textOf "stage" a >>= \t -> case t of { "first" -> Right FirstRun; "rechosen" -> Right Rechosen; _ -> Left "stage: invalid value" }
      un <- nonNeg "unintegrated" a
      line <- textOf "line" a >>= \t -> case t of { "notYet" -> Right NotYet; "first" -> Right FirstCrossing; "second" -> Right SecondCrossing; _ -> Left "line: invalid value" }
      if x < -1 || x > 1 then Left "driftX: expected a number in [-1, 1]" else Right (Activity mat (Drift x stage) un line)

-- 帳簿（engine.json の外側） -------------------------------------------------------------

stampField :: K.Key -> A.Object -> Parse UTCTime
stampField = stampWith (iso8601ParseM . T.unpack)

ledgerValue :: Ledger -> A.Value
ledgerValue lg = A.object
  [ "v" A..= (5 :: Int)
  , "segment" A..= lgSegment lg, "settled" A..= lgSettled lg
  , "obs" A..= A.object [ K.fromText (materialText k) A..= [ A.object [ "at" A..= obAt o, "interest" A..= obInterest o, "burden" A..= obBurden o ] | o <- os ] | (k, os) <- Map.toList (lgObs lg) ]
  , "alias" A..= A.object [ K.fromText (materialText k) A..= v | (k, v) <- Map.toList (lgAlias lg) ]
  , "epoch" A..= stampValue (Just (lgEpoch lg)), "syncedAt" A..= stampValue (Just (lgSyncedAt lg))
  , "dayStart" A..= stampValue (Just (lgDayStart lg)), "windowStart" A..= stampValue (Just (lgWindowStart lg))
  , "inSegment" A..= lgInSegment lg, "restUntil" A..= stampValue (lgRestUntil lg)
  , "engine" A..= engineValue (lgEngine lg) ]

parseLedger :: Params -> A.Object -> Parse Ledger
parseLedger p o = case KM.lookup "v" o of
  Just (A.Number n) | n >= 1 && n <= 5 -> do
    en <- objectOf "engine" o >>= parseEngine p
    epoch <- stampField "epoch" o
    synced <- stampField "syncedAt" o
    day <- stampField "dayStart" o
    win <- stampField "windowStart" o
    inSeg <- bool "inSegment" o
    rest <- nullableField stampField "restUntil" o
    -- v1（2026-10-28 05:45〜11:50）には、段の鍵、入れた運行、入りきらなかった分がない
    seg <- case KM.lookup "segment" o of
      Nothing -> Right Nothing
      Just A.Null -> Right Nothing
      Just (A.String t) -> Right (Just (SegmentId t))
      Just _ -> Left "segment: invalid value"
    settled <- case KM.lookup "settled" o of
      Nothing -> Right []
      Just (A.Array xs) -> traverse (\v -> case v of { A.Number x | x >= 0, fromIntegral (truncate x :: Int) == x -> Right (truncate x); _ -> Left "settled: invalid value" }) (foldr (:) [] xs)
      Just _ -> Left "settled: invalid value"
    -- v2 の uncounted（数だけ）は、どの材料のものか分からないので戻せない。0 でなければ止める
    case KM.lookup "uncounted" o of
      Just (A.Number x) | x > 0 -> Left "v2 uncounted must be zero; material identity is unavailable"
      _ -> Right ()
    -- v3 の owed（入れきれなかった分の列）は、v4 では持たない（読んだ分は Record で A に入る）。空でなければ止める
    case KM.lookup "owed" o of
      Just (A.Array xs) | not (null xs) -> Left "v3 owed must be empty; manual migration is required"
      _ -> Right ()
    obs <- case KM.lookup "obs" o of
      Nothing -> Right Map.empty
      Just (A.Object m) -> Map.fromList <$> traverse (\(k, v) -> case v of
          A.Array xs -> (\os -> (MaterialId (K.toText k), os)) <$> traverse obsOf (foldr (:) [] xs)
          _ -> Left "obs: invalid value") (KM.toList m)
      Just _ -> Left "obs: invalid value"
    alias <- case KM.lookup "alias" o of
      Nothing -> Right Map.empty
      Just (A.Object m) -> Map.fromList <$> traverse (\(k, v) -> case v of { A.String t -> Right (MaterialId (K.toText k), MaterialId t); _ -> Left "alias: invalid value" }) (KM.toList m)
      Just _ -> Left "alias: invalid value"
    Right (Ledger en epoch synced day win inSeg rest seg settled obs alias)
  _ -> Left "v: unsupported version"


obsOf :: A.Value -> Parse Obs
obsOf v = do
  o <- asObject "obs" v
  at <- nonNeg "at" o
  let unitOrNull k = case KM.lookup k o of
        Just A.Null -> Right Nothing
        Nothing -> Right Nothing
        Just (A.Number x) | x >= 0, x <= 1 -> Right (Just (realToFrac x))
        _ -> Left ("obs." ++ K.toString k ++ ": invalid value")
  i <- unitOrNull "interest"
  b <- unitOrNull "burden"
  Right (Obs at i b)


placeOf :: A.Object -> Parse Place
placeOf o = field "place" o >>= \v -> case v of
  A.String "home" -> Right Home
  A.String "out" -> Out . fromMaybe "" <$> textOrNull "where" o
  _ -> Left "place: invalid value"

placeFields :: Place -> [(K.Key, A.Value)]
placeFields Home = ["place" A..= ("home" :: T.Text), "where" A..= A.Null]
placeFields (Out w) = ["place" A..= ("out" :: T.Text), "where" A..= w]

doingText :: Doing -> T.Text
doingText WithYou = "with"
doingText OnMyOwn = "own"
doingText Asleep = "asleep"

doingOf :: A.Object -> Parse Doing
doingOf o = field "doing" o >>= \d -> case d of
  A.String "with" -> Right WithYou
  A.String "own" -> Right OnMyOwn
  A.String "asleep" -> Right Asleep
  _ -> Left "doing: invalid value"

situationValue :: Situation -> A.Value
situationValue (Situation pl d) = A.object (placeFields pl ++ ["doing" A..= doingText d])

situationOf :: A.Object -> Parse Situation
situationOf o = Situation <$> placeOf o <*> doingOf o

heardValue :: Heard -> A.Value
heardValue (Heard s t) = A.object ["session" A..= s, "at" A..= stampValue (Just t)]

heardOf :: A.Object -> Parse Heard
heardOf o = Heard <$> textOrNull "session" o <*> stamp "at" o

describeMark :: Situation -> String
describeMark (Situation place doing) = where_ ++ "，" ++ what
  where
    where_ = case place of
      Home -> "在家"
      Out w -> if T.null w then "在外面" else "在外面（" ++ T.unpack w ++ "）"
    what = case doing of
      WithYou -> "跟你一起"
      OnMyOwn -> "各做各的"
      Asleep -> "睡了"

describeChange :: Change -> String
describeChange (Set s) = describeMark s
describeChange Clear = "清掉"

-- talk.json：見た最後の記録される一言 ------------------------------------------------------

data TalkFile = TalkFile
  { tfLast   :: Maybe Heard
  , tfTied   :: Bool         -- ^ 別の会話に、同じ時刻の一言があった
  , tfRecent :: [UTCTime]    -- ^ 直近一日の記録される一言の時刻（昇順）
  }

emptyTalk :: TalkFile
emptyTalk = TalkFile Nothing False []

talkValue :: TalkFile -> A.Value
talkValue tf = A.object
  [ "v" A..= (2 :: Int)
  , "last" A..= stampValue (heardAt <$> tfLast tf)
  , "session" A..= (tfLast tf >>= heardSession)
  , "tied" A..= tfTied tf
  , "recent" A..= map (stampValue . Just) (tfRecent tf) ]

parseTalk :: A.Object -> Parse TalkFile
parseTalk o = case KM.lookup "v" o of
  Nothing -> do  -- 2026-10-22 の形：最後の時刻だけ。どの会話かは分からない
    t <- stamp "last" o
    Right (TalkFile (Just (Heard Nothing t)) False [t])
  Just (A.Number 2) -> do
    lastAt <- stampOrNull "last" o
    sess <- textOrNull "session" o
    tied <- bool "tied" o
    recent <- field "recent" o >>= \v -> case v of
      A.Array xs -> mapM (\x -> case x of
        A.String s | Just u <- parseStamp s -> Right u
        _ -> Left "recent: expected a timestamp") (foldr (:) [] xs)
      _ -> Left "recent: invalid value"
    Right (TalkFile (Heard sess <$> lastAt) tied (sort (nub recent)))
  Just _ -> Left "v: unsupported version"

observedOf :: TalkFile -> Maybe Observed
observedOf tf = (\h -> Observed h (tfTied tf)) <$> tfLast tf

-- presence.json：一筆 --------------------------------------------------------------------

data Marks = Marks { mkCurrent :: Maybe InEffect, mkWaiting :: Maybe Waiting, mkTalk :: Maybe Talk }
  deriving (Eq, Show)

emptyMarks :: Marks
emptyMarks = Marks Nothing Nothing Nothing

marksValue :: Marks -> A.Value
marksValue m = A.object
  [ "v" A..= (3 :: Int)
  , "current" A..= maybe A.Null inEffectValue (mkCurrent m)
  , "waiting" A..= maybe A.Null waitingValue (mkWaiting m)
  , "talk" A..= maybe A.Null talkLayerValue (mkTalk m) ]
  where
    inEffectValue e = A.object
      [ "since" A..= stampValue (Just (eSince e)), "basis" A..= heardValue (recHeard (eBasis e))
      , "committed" A..= stampValue (Just (eCommitted e)), "set" A..= situationValue (eSituation e) ]
    waitingValue w = A.object
      [ "since" A..= stampValue (Just (wSince w)), "basis" A..= heardValue (recHeard (wBasis w))
      , "set" A..= (case wChange w of Set s -> situationValue s; Clear -> A.Null) ]
    talkLayerValue t = A.object
      [ "first" A..= stampValue (Just (talkFirst t)), "since" A..= stampValue (Just (talkSince t))
      , "cancelled" A..= stampValue (talkCancelled t), "prevEnd" A..= stampValue (talkPrevEnd t) ]

-- | 読む。前の形（2026-10-22 の、一回の返事が終わるのを待つ形）からは、効いていた一筆だけを移し、待っていた一筆は外す
-- （その条件は「一回の返事が終わったら」で、いまの「一分たったら」とは別のもの。外したことは、書き直すときに記録へ）。
-- | 読む。返すのは、一筆、記録に残す知らせ、前の形だったか（前の形なら、次に書くときに新しい形で書き直す）。
parseMarks :: A.Object -> Parse (Marks, [String], Bool)
parseMarks o = do
  v <- count "v" o
  case v of
    3 -> do
      current <- objectOrNull "current" o >>= traverse inEffectOf
      waiting <- objectOrNull "waiting" o >>= traverse waitingOf
      talk <- objectOrNull "talk" o >>= traverse talkOf
      Right (Marks current waiting talk, [], False)
    2 -> do
      current <- objectOrNull "current" o >>= traverse slotOf
      pending <- objectOrNull "pending" o >>= traverse slotOf
      Right ( Marks (current >>= fst) Nothing Nothing
            , [ "旧格式里等着的一笔没有迁过来（" ++ snd p ++ "）" | Just p <- [pending] ], True )
    1 -> do
      committed <- if KM.member "committed" o then stampOrNull "committed" o else Right Nothing
      set <- objectOrNull "mark" o >>= traverse situationOf
      since <- stamp "since" o
      basis <- stamp "basis" o
      let sess = textField "session" o
      Right $ case (committed, set) of
        (Just c, Just s) -> (Marks (Just (InEffect since (Received (Heard sess basis)) c s)) Nothing Nothing, [], True)
        (Nothing, Just s) -> (emptyMarks, ["旧格式里等着的一笔没有迁过来（" ++ describeMark s ++ "）"], True)
        _ -> (emptyMarks, [], True)
    _ -> Left "v: unsupported version"
  where
    inEffectOf e = InEffect <$> stamp "since" e <*> (Received <$> (objectOf "basis" e >>= heardOf))
                            <*> stamp "committed" e <*> (objectOf "set" e >>= situationOf)
    waitingOf w = Waiting <$> stamp "since" w <*> (Received <$> (objectOf "basis" w >>= heardOf))
                          <*> (objectOrNull "set" w >>= \s -> maybe (Right Clear) (fmap Set . situationOf) s)
    talkOf t = Talk <$> stamp "first" t <*> stamp "since" t <*> stampOrNull "cancelled" t
                    <*> (if KM.member "prevEnd" t then stampOrNull "prevEnd" t else Right Nothing)
    -- 2026-10-22 の夜の形の格
    slotOf sl = do
      sess <- textOrNull "session" sl
      since <- stamp "since" sl
      basis <- stamp "basis" sl
      committed <- stampOrNull "committed" sl
      set <- objectOrNull "set" sl >>= traverse situationOf
      let described = maybe "清掉" describeMark set
      Right (case (committed, set) of
               (Just c, Just s) -> Just (InEffect since (Received (Heard sess basis)) c s)
               _ -> Nothing
            , described)

-- | 待っている一筆を確かめて、一筆に反映する。効いたとき、その一筆より前に立った「話に向き合う」層は取り消す。
settleMarks :: Params -> UTCTime -> Maybe Observed -> Marks -> (Marks, [String])
settleMarks p now lastTalk m = case mkWaiting m of
  Nothing -> (m, [])
  Just w -> case settleWaiting p now lastTalk w of
    TooSoon -> (m, [])
    Dropped -> (m { mkWaiting = Nothing }, ["有新话，等着的一笔不算了：" ++ describeChange (wChange w)])
    Applied e ->
      let talk' = case mkTalk m of
            Just t | isNothing (talkCancelled t), wSince w > talkSince t -> Just t { talkCancelled = Just now }
            other -> other
      in (Marks e Nothing talk', ["一笔生效了：" ++ describeChange (wChange w)])


-- check ------------------------------------------------------------------------

parseState :: A.Object -> Parse TriggerState
parseState o = do
  later <- case KM.lookup "v" o of
    Nothing -> Right False  -- 2026-10-22 の最初の試運転の形（v がない）
    Just (A.Number 2) -> Right True
    Just (A.Number 3) -> Right True
    Just _ -> Left "v: unsupported version"
  ds <- stampOrNull "dayStart" o
  n <- count "starts" o
  mu <- bool "morningUsed" o
  oo <- stampOrNull "outingOf" o
  lt <- bool "leftToday" o
  aw <- stampOrNull "away" o
  se <- objectOrNull "session" o >>= traverse session
  (ba, lc, mk) <- if later
    then (,,) <$> stampOrNull "backAt" o <*> stampOrNull "lastCheck" o <*> stampOrNull "markUsed" o
    else Right (Nothing, Nothing, Nothing)
  Right (TriggerState ds n mu oo lt aw ba lc mk se)
  where
    session s = do
      kind <- field "kind" s >>= \v -> case v of
        A.String "outing" -> Right OutingSession
        A.String "morning" -> Right MorningSession
        A.String "marked" -> MarkedSession <$> placeOf s
        _ -> Left "session.kind: invalid value"
      since <- stamp "since" s
      paused <- stampOrNull "paused" s
      wasAway <- bool "wasAway" s
      Right (Session kind since paused wasAway)

stateValue :: TriggerState -> A.Value
stateValue ts = A.object
  [ "v" A..= (3 :: Int)
  , "dayStart" A..= stampValue (tsDayStart ts), "starts" A..= tsStarts ts
  , "morningUsed" A..= tsMorningUsed ts, "outingOf" A..= stampValue (tsOutingOf ts)
  , "leftToday" A..= tsLeftToday ts, "away" A..= stampValue (tsAway ts)
  , "backAt" A..= stampValue (tsBackAt ts), "lastCheck" A..= stampValue (tsLastCheck ts)
  , "markUsed" A..= stampValue (tsMarkUsed ts)
  , "session" A..= maybe A.Null sessionValue (tsSession ts) ]
  where
    sessionValue s = A.object $
      [ "since" A..= stampValue (Just (sessSince s)), "paused" A..= stampValue (sessPaused s)
      , "wasAway" A..= sessWasAway s ]
      ++ kindFields (sessKind s)

describe :: Doko -> Decision -> String
describe seen d = case d of
  DidStart OutingSession -> "开始：出门"
  DidStart MorningSession -> "开始：早上比ユーザー先醒"
  DidStart (MarkedSession Home) -> case seen of
    Doko False _ _ -> "开始：先回家了，自己待着"
    _ -> "开始：在家，各做各的"
  DidStart (MarkedSession (Out w)) -> "开始：在外面等你" ++ (if T.null w then "" else "（" ++ T.unpack w ++ "）")
  DidPause -> "暂停：专心说话"
  DidResume why -> "继续：" ++ why
  DidEnd why -> "结束：" ++ why

data RunnerFile = RunnerFile
  { rfState   :: RunnerState
  , rfPid     :: Maybe ProcessID   -- ^ 起こした子（走っている・収まるのを待っているあいだ）
  }

emptyRunnerFile :: RunnerFile
emptyRunnerFile = RunnerFile initialRunner Nothing

runnerValue :: RunnerFile -> A.Value
runnerValue rf = A.object
  [ "v" A..= (3 :: Int)
  , "phase" A..= phaseValue (rsPhase (rfState rf))
  , "pid" A..= maybe A.Null (A.toJSON . (fromIntegral :: ProcessID -> Int)) (rfPid rf)
  , "segment" A..= maybe A.Null segmentValue (rsSegment (rfState rf))
  , "nextRunId" A..= rsNextRunId (rfState rf) ]
  where
    phaseValue Vacant = A.object ["state" A..= ("vacant" :: T.Text)]
    phaseValue (Running r) = A.object ["state" A..= ("running" :: T.Text), "run" A..= runValue r]
    phaseValue (Settling r why asked) = A.object
      [ "state" A..= ("settling" :: T.Text), "run" A..= runValue r
      , "why" A..= (case why of { TimedOut -> "timedOut" :: T.Text; Asked -> "asked" }), "askedAt" A..= stampValue (Just asked) ]
    runValue r = let RunId n = runId r in A.object
      [ "id" A..= n, "slot" A..= runSlot r, "startedAt" A..= stampValue (Just (runStartedAt r)), "seg" A..= segKeyValue (runSeg r) ]
    segmentValue seg = A.object
      [ "key" A..= segKeyValue (segKey seg), "slots" A..= segSlots seg, "failures" A..= segFailures seg
      , "noDelivery" A..= segNoDelivery seg, "nextEarliest" A..= stampValue (segNextEarliest seg)
      , "ended" A..= maybe A.Null (A.String . endedText) (segEnded seg) ]

segKeyValue :: SegKey -> A.Value
segKeyValue k = A.object (("since" A..= stampValue (Just (skSince k))) : kindFields (skKind k))

kindFields :: SessionKind -> [(K.Key, A.Value)]
kindFields OutingSession = ["kind" A..= ("outing" :: T.Text)]
kindFields MorningSession = ["kind" A..= ("morning" :: T.Text)]
kindFields (MarkedSession place) = ("kind" A..= ("marked" :: T.Text)) : placeFields place

-- | 帳簿にしまう段の鍵（始まりの時刻と種類）。
segKeyText :: SegKey -> SegmentId
segKeyText k = SegmentId $ T.pack (iso8601Show (skSince k)) <> " " <> case skKind k of
  OutingSession -> "outing"
  MorningSession -> "morning"
  MarkedSession Home -> "marked home"
  MarkedSession (Out w) -> "marked out " <> w

endedText :: EndReason -> T.Text
endedText SaidEnough = "enough"
endedText HardCap = "hardCap"
endedText NoDelivery = "noDelivery"
endedText StartFailures = "startFailures"
endedText PeriodEnded = "periodEnded"
endedText HeldForReview = "heldForReview"

segKeyOf :: A.Object -> Parse SegKey
segKeyOf s = do
  since <- stamp "since" s
  kind <- textOf "kind" s >>= \t -> case t of
    "outing" -> Right OutingSession
    "morning" -> Right MorningSession
    "marked" -> MarkedSession <$> placeOf s
    _ -> Left "kind: invalid value"
  Right (SegKey since kind)

parseRunner :: A.Object -> Parse RunnerFile
parseRunner o = case KM.lookup "v" o of
  Just (A.Number n) | n == 1 || n == 2 || n == 3 -> do
    ph <- objectOf "phase" o
    st <- textOf "state" ph
    phase <- case st of
      "vacant" -> Right Vacant
      "running" -> Running <$> (objectOf "run" ph >>= runOf)
      "settling" -> do
        r <- objectOf "run" ph >>= runOf
        -- v2 までは、わけと時刻を持っていなかった：伝えた時刻は分からないので起こした時刻、わけは「頼んだ」とする
        why <- case KM.lookup "why" ph of
          Just (A.String "timedOut") -> Right TimedOut
          Just (A.String "asked") -> Right Asked
          Nothing -> Right Asked
          _ -> Left "phase.why: invalid value"
        asked <- case KM.lookup "askedAt" ph of
          Nothing -> Right (runStartedAt r)
          Just _ -> stamp "askedAt" ph
        Right (Settling r why asked)
      _ -> Left "phase.state: invalid value"
    pid <- field "pid" o >>= \v -> case v of
      A.Null -> Right Nothing
      A.Number x -> Right (Just (fromIntegral (truncate x :: Int)))
      _ -> Left "pid: invalid value"
    seg <- objectOrNull "segment" o >>= traverse segmentOf
    nextId <- intOf "nextRunId" o
    Right (RunnerFile (RunnerState phase seg nextId) pid)
  _ -> Left "v: unsupported version"
  where
    runOf r = Run <$> (RunId <$> intOf "id" r) <*> (objectOf "seg" r >>= segKeyOf) <*> intOf "slot" r <*> stamp "startedAt" r
    segmentOf sg = Segment <$> (objectOf "key" sg >>= segKeyOf) <*> intOf "slots" sg <*> intOf "failures" sg
                           <*> intOf "noDelivery" sg <*> stampOrNull "nextEarliest" sg <*> (field "ended" sg >>= endedOf)
    endedOf A.Null = Right Nothing
    endedOf (A.String t) = case t of
      "enough" -> Right (Just SaidEnough)
      "hardCap" -> Right (Just HardCap)
      "noDelivery" -> Right (Just NoDelivery)
      "startFailures" -> Right (Just StartFailures)
      "periodEnded" -> Right (Just PeriodEnded)
      "heldForReview" -> Right (Just HeldForReview)
      _ -> Left "ended: invalid value"
    endedOf _ = Left "ended: invalid value"

-- 時刻と欄 ---------------------------------------------------------------------

parseStamp :: T.Text -> Maybe UTCTime
parseStamp t = case iso8601ParseM (T.unpack t) :: Maybe ZonedTime of
  Just z -> Just (zonedTimeToUTC z)
  Nothing -> iso8601ParseM (T.unpack t)

textField :: K.Key -> A.Object -> Maybe T.Text
textField k o = case KM.lookup k o of
  Just (A.String t) -> Just t
  _ -> Nothing

stampValue :: Maybe UTCTime -> A.Value
stampValue = maybe A.Null (A.String . T.pack . iso8601Show)

-- | 欄は全部そろっていて、形が合っていること。足りない欄を 0 や False で埋めない。
type Parse a = Either String a

field :: K.Key -> A.Object -> Parse A.Value
field k o = maybe (Left ("missing field: " ++ K.toString k)) Right (KM.lookup k o)

stamp :: K.Key -> A.Object -> Parse UTCTime
stamp = stampWith parseStamp

stampWith :: (T.Text -> Maybe UTCTime) -> K.Key -> A.Object -> Parse UTCTime
stampWith parse k o = field k o >>= \v -> case v of
  A.String t | Just u <- parse t -> Right u
  _ -> Left (K.toString k ++ ": expected a timestamp")

stampOrNull :: K.Key -> A.Object -> Parse (Maybe UTCTime)
stampOrNull = nullableField stamp

nullableField :: (K.Key -> A.Object -> Parse a) -> K.Key -> A.Object -> Parse (Maybe a)
nullableField parse k o = field k o >>= \v -> case v of
  A.Null -> Right Nothing
  _ -> Just <$> parse k o

bool :: K.Key -> A.Object -> Parse Bool
bool k o = field k o >>= \v -> case v of
  A.Bool b -> Right b
  _ -> Left (K.toString k ++ ": expected a boolean")

count :: K.Key -> A.Object -> Parse Int
count k o = field k o >>= \v -> case v of
  A.Number n | n >= 0, n <= 1000, fromIntegral (truncate n :: Int) == n -> Right (truncate n)
  _ -> Left (K.toString k ++ ": expected an integer in [0, 1000]")

textOrNull :: K.Key -> A.Object -> Parse (Maybe T.Text)
textOrNull = nullableField textOf

objectOf :: K.Key -> A.Object -> Parse A.Object
objectOf k o = field k o >>= asObject (K.toString k)

objectOrNull :: K.Key -> A.Object -> Parse (Maybe A.Object)
objectOrNull = nullableField objectOf

textOf :: K.Key -> A.Object -> Parse T.Text
textOf k o = field k o >>= \v -> case v of
  A.String t -> Right t
  _ -> Left (K.toString k ++ ": expected a string")

intOf :: K.Key -> A.Object -> Parse Int
intOf k o = field k o >>= \v -> case v of
  A.Number n | fromIntegral (truncate n :: Int) == n -> Right (truncate n)
  _ -> Left (K.toString k ++ ": expected an integer")


