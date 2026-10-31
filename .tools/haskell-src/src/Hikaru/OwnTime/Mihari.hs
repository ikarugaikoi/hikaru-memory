{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hikaru.OwnTime.Mihari
  ( check, Phrase(..), Kind(..), Input(..), readInput, Privacy(..), Cache(..), Scan(..), ScanResult(..),
    badSession, scanSession, scanRows, freshCache, cacheValue, parseCache, store, syncTalk, talkMark,
    presence, RowFailure(..)
  ) where

import Control.Exception (Exception, IOException, bracket, catch, throwIO)
import Control.Monad (foldM, forM_, unless, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isSpace)
import Data.List (findIndex, isSuffixOf, sort)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, diffUTCTime, localTimeToUTC, zonedTimeToUTC)
import qualified Data.Vector as V
import GHC.IO.FD (fdFD)
import qualified GHC.IO.Handle.FD as HFD
import qualified Hikaru.Compat.Json as J
import Hikaru.Compat.Diagnostic (DiagnosticFailure(..), ioDiagnostic)
import Hikaru.Choubo.JsonLine
import Hikaru.Config (Config(..), configuredNow, loadConfig)
import Hikaru.Conversation.DateTime (dateTimeToJST)
import Hikaru.Conversation.Hidden (HiddenClass(hiddenEnds, hiddenStarts), classifyHidden, classifyText)
import Hikaru.Conversation.Parse (parseLine)
import Hikaru.Conversation.Types (Message(..))
import Hikaru.OwnTime.Fatigue
import Hikaru.OwnTime.Errors (diagnosticText, fileDiagnostic, stateFailure)
import qualified Hikaru.OwnTime.Health as H
import Hikaru.OwnTime.Recovery
import Hikaru.OwnTime.Runtime
import Hikaru.OwnTime.Store
import Hikaru.OwnTime.Trigger
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.FilePath ((</>))
import System.IO (IOMode(ReadMode), SeekMode(AbsoluteSeek), hClose, hPutStrLn, hSeek, openBinaryFile, stderr)
import System.Posix.Files (fileID, fileSize, getFdStatus, getFileStatus)
import System.Posix.Types (Fd(Fd))

check :: [String] -> IO ()
check args = do
  unless ("--dry-run" `elem` args) $ do
    hPutStrLn stderr "hikaru-owntime: いまは試運転だけ（check --dry-run）"
    exitWith (ExitFailure 2)
  H.watchHealth H.Check checkTick

checkTick :: IO ()
checkTick = do
  dir <- stateDir
  createDirectoryIfMissing True (dir </> "sessions")
  -- 見張りは一度に一つ（重なったら、あとのほうが前のほうの書いた状態を読んでから決める）。
  -- 時刻もロックの中で取る（先に取ると、待たされているあいだに日替わりした状態を、古い時刻で見てしまう）
  withLock (dir </> "check.lock") $ do
   nowZ <- configuredNow
   let now = zonedTimeToUTC nowZ
   dayStart <- dayStartOf now
   (ts0, firstRun) <- loadHealing H.Check dir now (dir </> "state.json")
     (fmap (\st -> (st, False)) . parseState) (initialTrigger, True) LastGood
   -- Resolve inputs under their existing locks before publishing any decisions.
   withLock (dir </> "presence.lock") $ withLock (dir </> "talk.lock") $ do
     _ <- healMarks dir now
     _ <- healTalk dir now
     pure ()
   syncTalk dir now
   home <- homeWifi
   (seen, dokoNote) <- readDoko home now
   forM_ dokoNote (hPutStrLn stderr)
   do
      -- 待っている一筆を確かめる：一筆と最後の一言の両方のロックの中で、読み直して、書く（最後の一言が、読んだあと
      -- 確かめるまでのあいだに進んでしまうと、効かせてはいけない一筆を効かせてしまう）
      (marks, notes, talk) <- withLock (dir </> "presence.lock") $ withLock (dir </> "talk.lock") $ do
        (m0, migrated, oldForm) <- healMarks dir now
        talk <- healTalk dir now
        let (m1, settled) = settleMarks defaultParams now (observedOf talk) m0
        when (oldForm || not (null migrated) || m1 /= m0) $
          writeAtomic (dir </> "presence.json") (A.encode (marksValue m1))
        pure (m1, migrated ++ settled, talk)
      forM_ notes (appendLog dir now)
      let obs = observedOf talk
          inputs = Inputs now dayStart seen obs (tfRecent talk) (mkCurrent marks) (mkWaiting marks) (mkTalk marks)
          (ts1, decisions) = decide defaultParams inputs ts0
          place = case seen of
            Doko True _ _ -> "在家"
            Doko False _ _ -> maybe "不在家" (\t -> clockJST t ++ " 出门") (tsAway ts1)
            DokoUnknown -> "没有 Wi-Fi 的消息"
          marked = case effectivePresence defaultParams inputs ts1 of
            Just e -> "；记着：" ++ describeMark (eSituation e)
            Nothing -> ""
          waiting = case effectiveWaiting defaultParams inputs ts1 of
            Just w -> "；等着生效：" ++ describeChange (wChange w)
            Nothing -> ""
          talking = if isJust (activeTalk defaultParams inputs ts1) then "；专心说话中" else ""
          context = "（" ++ place ++ "；最后一句 " ++ maybe "没有" (clockJST . heardAt . obsHeard) obs
                    ++ marked ++ waiting ++ talking ++ "）"
      when firstRun $ appendLog dir now ("空跑开始：只记判断，不叫醒" ++ context)
      forM_ decisions $ \d -> appendLog dir now (describe seen d ++ context)
      H.writeGood (dir </> "state.json") (A.encode (stateValue ts1))


-- The legacy JSON/Message contract ends here; the queue receives only Input.
-- | 列の項目が合言葉か。
data Phrase = StartPhrase | EndPhrase | NoPhrase
  deriving (Eq, Show)

-- | 列の項目の種類：ユーザーの一言（合言葉かどうか）か、道具の知らせ（日記、通知、別の会話からの知らせ、シェル）か。
data Kind = HumanItem Phrase | SystemItem
  deriving (Eq, Show)

data Input
  = Enqueue Kind (Maybe UTCTime) | Dequeue | Remove (Maybe Kind) | UnknownQueue
  | Delivered Kind Bool (Maybe UTCTime) | Ignored
  deriving (Eq, Show)

readInput :: T.Text -> B.ByteString -> IO (Either String Input)
readInput identity row = case TE.decodeUtf8' row of
  Left _ -> pure (Left "invalid UTF-8")
  Right text -> checked text $ case J.decodeJson (conversationText text) of
    Left _ -> pure (Left "invalid JSON")
    Right doc@(J.Object _)
      | str (get "type" doc) == Just "queue-operation" -> pure (Right (queueOp doc))
      | otherwise -> do
          r <- parseLine dateTimeToJST (J.pyStringFromText identity) doc
          case r of
            Left why -> pure (Left ("invalid conversation row: " ++ T.unpack why))
            -- ユーザーが自分で打った一言だけが届いた一言（わたしの返事、貼り付けだけのものは違う）
            Right (Just message) | messageTyped message -> do
              at <- messageUTC message
              let cls = classifyHidden message
                  -- 定時の日記、道具の知らせ、スラッシュの命令：ユーザーの一言ではない（列には道具の知らせとして当てる）
                  kind | either (const False) (systemPrompt . T.strip) (J.pyStringToText (messageText message)) = SystemItem
                       | hiddenStarts cls = HumanItem StartPhrase
                       | hiddenEnds cls = HumanItem EndPhrase
                       | otherwise = HumanItem NoPhrase
              pure (Delivered kind (str (get "type" doc) == Just "attachment") <$> at)
            Right _ -> pure (Right (other doc))
    Right _ -> pure (Left "expected a JSON object")
  where
    -- Retain the existing detailed failures (e.g. a non-scalar surrogate).
    -- The legacy queue decoder can accept non-JSON constants; successful
    -- admission must also satisfy the shared conversation syntax rule.
    checked text action = do
      parsed <- action
      pure $ case parsed of
        Right _ | classifyRecord True Outside text /= CompleteJson -> Left "invalid JSON"
        _ -> parsed
    queueOp doc = case str (get "operation" doc) of
      Just "enqueue" -> Enqueue (kindOf (get "content" doc)) (str (get "timestamp" doc) >>= parseStamp)
      Just "dequeue" -> Dequeue
      Just "remove" -> Remove (if hasKey "content" doc then Just (kindOf (get "content" doc)) else Nothing)
      _ -> UnknownQueue
    -- 届いたものの、ユーザーの一言ではないもの（道具の知らせなど）：列の模型だけ進める。
    other doc
      | str (get "type" doc) == Just "attachment", str (get "type" (get "attachment" doc)) == Just "queued_command" = Delivered SystemItem True Nothing
      | str (get "type" doc) == Just "user", Just k <- str (get "kind" (get "origin" doc)), k /= "human" = Delivered SystemItem False Nothing
      | otherwise = Ignored
    kindOf v = case v of
      J.String x -> case J.pyStringToText x of
        Right t -> classify t
        _ -> HumanItem NoPhrase
      -- 画像などがついた一言は、文字の塊（text）の並び。核の textOfContent と同じく、文字の塊をそのままつないで
      -- （間に何も入れずに）見る。文字がなければ（画像だけ）、ふつうの一言
      J.Array blocks ->
        let texts = [t | b <- V.toList blocks, str (get "type" b) == Just "text", Just t <- [str (get "text" b)]]
        in if null texts then HumanItem NoPhrase else classify (T.concat texts)
      _ -> HumanItem NoPhrase
    classify t
      | systemPrompt (T.strip t) || slashCommand (T.strip t) = SystemItem
      | hiddenStarts cls = HumanItem StartPhrase
      | hiddenEnds cls = HumanItem EndPhrase
      | otherwise = HumanItem NoPhrase
      where cls = classifyText True t
    -- スラッシュの命令（/compact、/model …）：列には入るが、届く行がない（か、<command-name> の形で届く）。
-- Project-local implementation note.
    slashCommand t = case T.uncons t of
      Just ('/', rest) -> let w = T.takeWhile (not . isSpace) rest
                          in not (T.null w) && T.all (\ch -> isAlphaNum ch || ch == '-' || ch == '_') w
      _ -> False

    messageUTC message = do
      r <- dateTimeToJST (messageTime message)
      pure $ case r of
        Right local -> Right (Just (localTimeToUTC jstZone local))
        Left why -> Left ("timestamp: " ++ T.unpack why)
    get k v = either (const J.Null) id (J.field k J.Null v)
    str (J.String x) = either (const Nothing) Just (J.pyStringToText x)
    str _ = Nothing
    hasKey k (J.Object o) = any ((== Right k) . J.pyStringToText . fst) (J.objectPairs o)
    hasKey _ _ = False

systemPrompt :: T.Text -> Bool
systemPrompt p = T.null p || any (`T.isPrefixOf` p)
  [ "あなたは Claude です", "<scheduled-task", "<command-name>", "<local-command-"
  , "<task-notification", "<side-sessions-event", "<ci-monitor-event", "<agent-message", "<cross-session-message"
  , "<bash-input>", "<bash-stdout>", "<bash-stderr>" ]


-- 会話ごとの控え ----------------------------------------------------------------

-- | この会話は、いま記録される話の中か。
data Privacy = Recorded | NotRecorded | Unknown
  deriving (Eq, Show)

-- | 列の項目の段階：列の中／取り出されて、届くのを待っている（空いているときの dequeue）／届いて、列から外れる（remove）のを
-- 待っている（返事を書いているあいだに届いた queued_command）。
data Stage = Queued | AwaitingDelivery | Landed
  deriving (Eq, Show)

data Item = Item { iKind :: Kind, iStage :: Stage }
  deriving (Eq, Show)

data Cache = Cache
  { cOffset    :: Integer
  , cInode     :: Maybe Integer
  , cPrivacy   :: Privacy
  , cQueue     :: [Item]
  , cObserved  :: Maybe UTCTime   -- ^ この会話で見た最後の記録される一言（enqueue の行も含む）
  , cDelivered :: Maybe UTCTime   -- ^ この会話で届いた最後の記録される一言
  }
  deriving (Eq, Show)

freshCache :: Maybe Integer -> Cache
freshCache inode = Cache 0 inode Recorded [] Nothing Nothing

isStartItem :: Item -> Bool
isStartItem (Item (HumanItem StartPhrase) _) = True
isStartItem _ = False

-- | まだ届いていない合言葉が列にある（その後ろに並んだ一言は守る）。届いたあと列から外れるのを待っているだけのものは、
-- もうとばりの中として守られているので、ここでは数えない。
startAhead :: Cache -> Bool
startAhead c = any (\it -> isStartItem it && iStage it /= Landed) (cQueue c)

isPhraseItem :: Item -> Bool
isPhraseItem (Item (HumanItem NoPhrase) _) = False
isPhraseItem (Item (HumanItem _) _) = True
isPhraseItem _ = False

-- | この会話で起きたことを trouble.log に書いてよいか：記録される話の中で、合言葉が列にないとき。
loggable :: Cache -> Bool
loggable c = cPrivacy c == Recorded && not (startAhead c)

-- Only caches built by the strict completed-row reader can be resumed.
cacheVersion :: Int
cacheVersion = 6

cacheValue :: Cache -> A.Value
cacheValue c = A.object
  [ "v" A..= cacheVersion, "offset" A..= cOffset c, "inode" A..= cInode c
  , "privacy" A..= privacyText (cPrivacy c)
  , "queue" A..= map itemValue (cQueue c)
  , "observed" A..= stampValue (cObserved c), "delivered" A..= stampValue (cDelivered c) ]
  where
    privacyText Recorded = "recorded" :: T.Text
    privacyText NotRecorded = "notRecorded"
    privacyText Unknown = "unknown"
    itemValue (Item kind stage) = A.object
      [ "kind" A..= (case kind of HumanItem _ -> "human" :: T.Text; SystemItem -> "system")
      , "phrase" A..= (case kind of
          HumanItem StartPhrase -> "start" :: T.Text
          HumanItem EndPhrase -> "end"
          _ -> "none")
      , "stage" A..= (case stage of Queued -> "queued" :: T.Text; AwaitingDelivery -> "awaiting"; Landed -> "landed") ]

parseCache :: A.Object -> Parse Cache
parseCache o = do
  v <- count "v" o
  unless (v == cacheVersion) (Left "v: unsupported version")
  offset <- field "offset" o >>= \x -> case x of
    A.Number n | n >= 0, fromIntegral (truncate n :: Integer) == n -> Right (truncate n)
    _ -> Left "offset"
  inode <- field "inode" o >>= \x -> case x of
    A.Null -> Right Nothing
    A.Number n | n >= 0, fromIntegral (truncate n :: Integer) == n -> Right (Just (truncate n))
    _ -> Left "inode"
  privacy <- field "privacy" o >>= \x -> case x of
    A.String "recorded" -> Right Recorded
    A.String "notRecorded" -> Right NotRecorded
    A.String "unknown" -> Right Unknown
    _ -> Left "privacy"
  queue <- field "queue" o >>= \x -> case x of
    A.Array xs -> mapM itemOf (V.toList xs)
    _ -> Left "queue"
  observed <- stampOrNull "observed" o
  delivered <- stampOrNull "delivered" o
  Right (Cache offset inode privacy queue observed delivered)
  where
    itemOf (A.Object it) = do
      kind <- field "kind" it
      phrase <- field "phrase" it
      stage <- field "stage" it >>= \x -> case x of
        A.String "queued" -> Right Queued
        A.String "awaiting" -> Right AwaitingDelivery
        A.String "landed" -> Right Landed
        _ -> Left "stage"
      k <- case (kind, phrase) of
        (A.String "system", _) -> Right SystemItem
        (A.String "human", A.String "start") -> Right (HumanItem StartPhrase)
        (A.String "human", A.String "end") -> Right (HumanItem EndPhrase)
        (A.String "human", A.String "none") -> Right (HumanItem NoPhrase)
        _ -> Left "kind"
      Right (Item k stage)
    itemOf _ = Left "queue: expected an object item"

-- Old checkpoints carry only the boundary of already-published events. Their
-- cached state is rebuilt under the new reader before it can be used again.
data CacheFile = CurrentCache Cache | PreviousCache Cache

parseCacheFile :: A.Object -> Parse CacheFile
parseCacheFile object = case KM.lookup "v" object of
  Just (A.Number 2) -> PreviousCache <$> parseCache (KM.insert "v" (A.toJSON cacheVersion) object)
  Just (A.Number 3) -> PreviousCache <$> parseCache (KM.insert "v" (A.toJSON cacheVersion) object)
  Just (A.Number 4) -> PreviousCache <$> parseCache (KM.insert "v" (A.toJSON cacheVersion) object)
  Just (A.Number 5) -> PreviousCache <$> parseCache (KM.insert "v" (A.toJSON cacheVersion) object)
  _ -> CurrentCache <$> parseCache object

badSession :: T.Text -> Bool
badSession sid = T.null sid || T.any (`elem` ("/\\" :: String)) sid

cachePath :: FilePath -> String -> FilePath
cachePath dir sid = dir </> "sessions" </> (sid ++ ".json")

-- 会話を読む ---------------------------------------------------------------------

-- | 読み足した結果：新しい控え、数えた記録される一言の時刻、記してよい困りごと。
data Scan = Scan { scCache :: Cache, scCounted :: [UTCTime], scTroubles :: [String] }

data ScanResult = NoTranscript | Scanned Scan

data RowFailure = RowFailure Int String deriving (Eq, Show)
instance Exception RowFailure

parseRow :: T.Text -> Int -> B.ByteString -> IO Input
parseRow identity number row =
  (readInput identity row >>= either (throwIO . RowFailure number) pure)
    `catch` (\(DiagnosticFailure detail) -> throwIO (RowFailure number (diagnosticText detail)))
    `catch` (\(err :: IOException) -> ioDiagnostic Nothing Nothing err >>= throwIO . RowFailure number . diagnosticText)

-- | 会話の記録を、前に読んだところから続けて読む。控えは返すだけで、書くのは呼ぶ側。
scanSession :: FilePath -> String -> IO ScanResult
scanSession dir sid = withLock (cachePath dir sid ++ ".lock") (scanSessionUsing True dir sid)

scanSessionUsing :: Bool -> FilePath -> String -> IO ScanResult
scanSessionUsing useCache dir sid = do
  config <- loadConfig
  let transcript = logRoot config </> (sid ++ ".jsonl")
      identity = projectIdentity config
  bracket (missingAsNothing (openBinaryFile transcript ReadMode)) (mapM_ hClose) $ \handle -> case handle of
   Nothing -> pure NoTranscript
   Just h -> do
    when useCache (H.resumeRepair dir (cachePath dir sid))
    cached <- if useCache then loadParsed parseCacheFile (cachePath dir sid) else pure Absent
    -- 開いてから、その同じファイルの大きさと inode を見る（見てから開くと、そのあいだに換えられたファイルを、
    -- 古い位置と古い状態で読んでしまう）
    fd <- HFD.handleToFd h
    st <- getFdStatus (Fd (fdFD fd))
    let size = fromIntegral (fileSize st) :: Integer
        inode = fromIntegral (fileID st) :: Integer
        previous = case cached of
          Got (CurrentCache c) -> Just c
          _ -> Nothing  -- Missing, damaged and old-version caches rebuild once.
        (cachedBase, troubles0) = case previous of
          Just c | cOffset c <= size, maybe True (== inode) (cInode c) -> (c { cInode = Just inode }, [])
          Just c -> ((freshCache (Just inode)) { cObserved = cObserved c, cDelivered = cDelivered c }, ["记录被截短或换掉了，从头重读"])
          Nothing -> (freshCache (Just inode), [])
    atBoundary <- if cOffset cachedBase == 0 then pure True else do
      hSeek h AbsoluteSeek (cOffset cachedBase - 1)
      (== "\n") <$> B.hGet h 1
    let base = if atBoundary then cachedBase else freshCache (Just inode)
        offset = cOffset base
        damaged = case cached of
          Bad why -> Just why
          Got (CurrentCache _) | not atBoundary -> Just "offset: not a complete row boundary"
          _ -> Nothing
    hSeek h AbsoluteSeek offset
    bytes <- B.hGet h (fromIntegral (size - offset))
    let (records, tailRecords) = span recordTerminated (splitRecords bytes)
        completeEnd = case reverse records of [] -> 0; record:_ -> recordEnd record
        legacyOffset = case cached of
          Got (PreviousCache c) | maybe True (== inode) (cInode c), cOffset c <= size ->
            -- An old physical-line offset may lie inside a new record. Only
            -- wholly published records belong to the migration prefix.
            foldl max 0 [recordEnd r | r <- records, recordEnd r <= cOffset c]
          _ -> 0
        (alreadyPublished, appended) = span ((<= legacyOffset) . recordEnd) records
        -- Successful current-version resumes never read/parse the prefix.
        located :: Integer -> IO a -> IO a
        located start action = action `catch` \(RowFailure number why) -> do
          hSeek h AbsoluteSeek 0
          prefix <- B.hGet h (fromIntegral start)
          stateFailure transcript ("line " ++ show (BC.count '\n' prefix + number) ++ ": " ++ why)
        rows = map (B.init . recordBytes)
    -- Validate the unfinished suffix too: only a genuinely incomplete record
    -- waits. As before, OwnTime publishes/checkpoints only LF-terminated input.
    forM_ tailRecords $ \record -> located offset $
      if recordQuote record /= Outside then pure ()
      else case TE.decodeUtf8' (recordBytes record) of
        Left _ -> throwIO (RowFailure (fromInteger (recordLine record)) "invalid UTF-8")
        Right text -> when (classifyRecord False Outside text == InvalidJson) $
          throwIO (RowFailure (fromInteger (recordLine record)) "invalid JSON")
    -- Rebuild old state once, without republishing its talk/trouble events.
    (rebuilt, _, _) <- located 0 $ scanRows identity base [] (rows alreadyPublished)
    (cache, counted, troubles) <- located (offset + legacyOffset) $
      scanRows identity rebuilt troubles0 (rows appended)
    let scanned = Scan cache { cOffset = offset + completeEnd } (sort counted) troubles
    case damaged of
      Just why -> do
        now <- zonedTimeToUTC <$> configuredNow
        -- Publishing a completed checkpoint here would lose counted times if
        -- the caller cannot publish talk/presence. Preserve the bad bytes now,
        -- install a zero checkpoint, and let store publish talk BEFORE progress.
        _ <- H.recoverFile H.Check dir now (cachePath dir sid) why "reset damaged checkpoint; rebuild from original transcript before publication"
          (pure (BL.toStrict (A.encode (cacheValue (freshCache (Just inode))))))
        pure ()
      _ -> pure ()
    pure (Scanned scanned)

scanRows :: T.Text -> Cache -> [String] -> [B.ByteString] -> IO (Cache, [UTCTime], [String])
scanRows identity base troubles rows = foldM step (base, [], troubles) (zip (scanl (\n row -> n + BC.count '\n' row + 1) 1 rows) rows)
  where
    step (c, counted, tr) (number, row) = do
      event <- parseRow identity number row
      let (c', at, more) = advance c event
      pure (c', maybe counted (: counted) at, tr ++ [m | m <- more, loggable c])

    -- 列の操作の行。
    advance c event = case event of
      Enqueue kind time ->
        let countIt = kind == HumanItem NoPhrase && cPrivacy c == Recorded && not (startAhead c)
            at = if countIt then time else Nothing
        in (c { cQueue = cQueue c ++ [Item kind Queued], cObserved = max (cObserved c) at }, at, [])
      Dequeue -> case cQueue c of
        (m : rest) -> (c { cQueue = m { iStage = AwaitingDelivery } : rest }, Nothing, [])
        [] -> (c, Nothing, ["队列空着，却有 dequeue"])
      Remove kind -> let (c', more) = leave c kind in (c', Nothing, more)
      UnknownQueue
        | any isStartItem (cQueue c) -> (c { cPrivacy = Unknown, cQueue = [] }, Nothing, [])
        | otherwise -> (c { cQueue = [] }, Nothing, ["不认识的队列操作"])
      Delivered SystemItem viaQueue _ -> let (c', more) = arrive c SystemItem viaQueue in (c', Nothing, more)
      Delivered (HumanItem phrase) viaQueue at -> deliverHuman c phrase viaQueue at
      Ignored -> (c, Nothing, [])

    -- 列を去った（remove）。中身がなければ先頭。同じ種類の、届いて列から外れるのを待っているものを先に当てる（ひとまとまりでいくつも届いて
    -- から順に列から外れるので）。届いたあとに列から外れるのならふつう。届かないまま去った合言葉は「分からない」へ。
    leave c kind = case kind of
      Nothing -> case cQueue c of
        [] -> (c, ["remove 时队列空着"])
        (m : rest) -> (departed m c { cQueue = rest }, [])
      Just k ->
        let landedAt = findIndex (\it -> iKind it == k && iStage it == Landed) (cQueue c)
            anyAt = findIndex ((== k) . iKind) (cQueue c)
        in case maybe anyAt Just landedAt of
          Nothing -> (c, ["remove 对不上队列"])
          Just i ->
            let (before, rest) = splitAt i (cQueue c)
                (c1, more) = clearLeft before c
            in case rest of
              m : after -> (departed m c1 { cQueue = filter ((== Landed) . iStage) before ++ after }, more)
              [] -> (c, [])
    departed m c
      | iStage m == Landed = c
      | isPhraseItem m = c { cPrivacy = Unknown }
      | otherwise = c

    -- 届いた一言（人のもの）。同じ種類の先頭の項目に当てる。段階がまだ「届いて列から外れるのを待つ」でないものから。
    deliverHuman c phrase viaQueue at =
      let (c1, more) = arrive c (HumanItem phrase) viaQueue
          privacy' = case phrase of
            StartPhrase -> NotRecorded
            EndPhrase -> Recorded
            NoPhrase -> cPrivacy c1
          counted = if phrase == NoPhrase && privacy' == Recorded then at else Nothing
          c2 = c1 { cPrivacy = privacy', cObserved = max (cObserved c1) counted, cDelivered = max (cDelivered c1) counted }
      in (c2, counted, more)

    -- 同じ種類の、まだ届いていない先頭の項目に当てる。合言葉なら、並んでいる同じ合言葉を全部当てる（二度送られたことが
    -- あった。2026-09-29 の記録。届いたのは一度で、もう一つが残ると、あとで「分からない」になってしまう）。
    arrive c kind viaQueue =
      let matches it = iKind it == kind && iStage it /= Landed
          duplicate = case kind of
            HumanItem StartPhrase -> True
            HumanItem EndPhrase -> True
            _ -> False
          -- 二度送られた同じ合言葉を当てるのは、反対の合言葉が並んでいるところまで（終わりの合言葉の向こうの合言葉は、
          -- 次にとばりを下ろす一言なので残す）
          opposite it = case (kind, iKind it) of
            (HumanItem StartPhrase, HumanItem EndPhrase) -> True
            (HumanItem EndPhrase, HumanItem StartPhrase) -> True
            _ -> False
          dropDups [] = []
          dropDups (x : xs)
            | matches x = dropDups xs
            | opposite x = x : xs
            | otherwise = x : dropDups xs
      in case break matches (cQueue c) of
        (before, m : after) ->
          let (c1, more) = clearLeft before c
              after' = if duplicate then dropDups after else after
              rest = [m { iStage = Landed } | viaQueue] ++ after'
          in (c1 { cQueue = filter ((== Landed) . iStage) before ++ rest }, more)
        -- 列に記録のない一言。ユーザーの一言なら、先入れ先出しで、まだ届いていない項目は全部それより前に並んでいた
        -- はずのもの（記録が抜けた）として片づける（合言葉が混じっていれば「分からない」へ）。道具の知らせは列を通らずに
        -- 書かれることがある（<local-command-stdout> など）ので、列はそのまま
        (before, []) -> case kind of
          HumanItem _ -> let (c1, more) = clearLeft before c in (c1 { cQueue = filter ((== Landed) . iStage) before }, more)
          SystemItem -> (c, [])

    -- 届いた一言より前に残っていた項目。届いて列から外れるのを待っているものはそのまま残す。それ以外は記録が抜けたもの：
    -- 合言葉が混じっていれば「分からない」へ（黙って）。ユーザーの一言なら困りごとに記す。道具の知らせや
    -- スラッシュの命令（届く行がない）は黙って外す。
    clearLeft before c =
      let gone = filter ((/= Landed) . iStage) before
      in if any isPhraseItem gone then (c { cPrivacy = Unknown }, [])
         else (c, ["队列里有没见到送到的项目" | any ((== HumanItem NoPhrase) . iKind) gone])

-- | ユーザーの一言ではないもの（定時の日記、道具の知らせ、スラッシュの命令）。
-- 定時の日記の見分け方は 'Hikaru.Choubo.Now' と同じ。
trouble :: FilePath -> UTCTime -> String -> IO ()
trouble dir = appendTimedLog "%Y-%m-%d %H:%M" (dir </> "trouble.log")

-- | 読み足した分を片づける：最後の一言を見張りから見える talk.json に出し、控えを書き、困りごとを記す。
-- 先に出してから控えを書く（あいだで止まっても、次にもう一度読んで出すので、なくならない）。
store :: FilePath -> String -> UTCTime -> Scan -> IO ()
store dir sid now scanned = do
  noteTalk dir (T.pack sid) (scCounted scanned)
  storeCache dir sid now scanned

storeCache :: FilePath -> String -> UTCTime -> Scan -> IO ()
storeCache dir sid now (Scan c _ troubles) = do
  withLock (cachePath dir sid ++ ".lock") $ writeAtomic (cachePath dir sid) (A.encode (cacheValue c))
  forM_ troubles $ \why -> trouble dir now (sid ++ "：" ++ why)

-- talk-mark --------------------------------------------------------------------

talkMark :: IO ()
talkMark = do
  runner <- lookupEnv "HIKARU_OWNTIME_RUN"
  input <- BL.getContents
  case (runner, A.decode input) of
    (Nothing, Just (A.Object o)) -> do
      let sid = fromMaybe "" (textField "session_id" o)
      unless (badSession sid) $ H.watchHealth H.Talk $ do
        now <- zonedTimeToUTC <$> configuredNow
        dir <- stateDir
        createDirectoryIfMissing True (dir </> "sessions")
        let name = T.unpack sid
        r <- scanSession dir name
        case r of
          NoTranscript -> trouble dir now ("talk-mark " ++ name ++ "：找不到这个会话的记录，这一句不算")
          Scanned s -> store dir name now s
    _ -> pure ()

-- | 決める前に、前に読んだところより先が書き足された会話を読み足して、最後の一言を進める。
-- 記録が消えた会話は、もう見ない（控えを捨てる）。
syncTalk :: FilePath -> UTCTime -> IO ()
syncTalk dir now = do
  config <- loadConfig
  names <- listDirectory (dir </> "sessions")
  -- A staged repair receipt is metadata beside its original cache, not a
  -- registered conversation; scanning it as one would delete it on restart.
  forM_ [take (length n - 5) n | n <- names, ".json" `isSuffixOf` n, not (".json.repair.json" `isSuffixOf` n)] $ \sid -> do
    let transcript = logRoot config </> (sid ++ ".jsonl")
    status <- missingAsNothing (getFileStatus transcript)
    case status of
      Nothing -> do
        removeFile (cachePath dir sid)
        trouble dir now (sid ++ "：会话的记录不见了，不再看这个会话")
      Just st -> do
        withLock (cachePath dir sid ++ ".lock") $ H.resumeRepair dir (cachePath dir sid)
        cached <- loadParsed parseCache (cachePath dir sid)
        let current = case cached of
              Got c -> cOffset c == fromIntegral (fileSize st)
                    && cInode c == Just (fromIntegral (fileID st))
              _ -> False
        unless current $ do
          scanned <- scanSession dir sid
          case scanned of
            Scanned sc -> store dir sid now sc
            NoTranscript -> pure ()




-- These are called while holding presence.lock / talk.lock respectively.
healMarks :: FilePath -> UTCTime -> IO (Marks, [String], Bool)
healMarks dir now = loadHealing H.Check dir now (dir </> "presence.json") parseMarks (emptyMarks, [], False)
  (Rebuild "reset daily presence to no entry" (pure (BL.toStrict (A.encode (marksValue emptyMarks)))))

healTalk :: FilePath -> UTCTime -> IO TalkFile
healTalk dir now = loadHealing H.Check dir now (dir </> "talk.json") parseTalk emptyTalk
  (Rebuild "rebuild talk state from all original transcripts" (rebuildTalk dir))

rebuildTalk :: FilePath -> IO B.ByteString
rebuildTalk dir = do
  config <- loadConfig
  names <- sort . filter (".jsonl" `isSuffixOf`) <$> listDirectory (logRoot config)
  talk <- foldM collect emptyTalk names
  pure (BL.toStrict (A.encode (talkValue talk)))
  where
    collect old name = do
      let sid = take (length name - 6) name
      scanned <- scanSessionUsing False dir sid
      case scanned of
        NoTranscript -> stateFailure name "transcript disappeared during talk reconstruction"
        Scanned sc -> pure (mergeTalk (T.pack sid) (scCounted sc) old)

-- presence ---------------------------------------------------------------------

data PresenceCmd = PTalk | PChange Change

presence :: [String] -> IO ()
presence args = do
  cmd <- case args of
    ["talk"] -> pure PTalk
    ["clear"] -> pure (PChange Clear)
    [place, doing] -> PChange . Set <$> situationFrom place doing Nothing
    [place, doing, w] -> PChange . Set <$> situationFrom place doing (Just w)
    _ -> usage
  sid <- lookupEnv "CLAUDE_CODE_SESSION_ID" >>= maybe (refuse "この会話がどれか分からない（CLAUDE_CODE_SESSION_ID がない）ので、書かない") pure
  when (badSession (T.pack sid)) (refuse "会話の名前がおかしいので、書かない")
  dir <- stateDir
  createDirectoryIfMissing True (dir </> "sessions")
  now <- zonedTimeToUTC <$> configuredNow
  r <- scanSession dir sid
  scanned <- case r of
    NoTranscript -> refuse "書かない：この会話の記録が見つからない"
    Scanned sc -> pure sc
  let cache = scCache scanned
      save = store dir sid now scanned
      saveWithTalk = do
        talkf <- noteTalkLocked dir (T.pack sid) (scCounted scanned)
        storeCache dir sid now scanned
        pure talkf
  case cPrivacy cache of
    NotRecorded -> save >> putStrLn "とばりの中なので、書かない"
    Unknown -> save >> refuse "書かない：この会話の記録が読めなくなっている（次の始まりか終わりの合言葉まで）"
    Recorded -> case cmd of
      PTalk -> do
        -- いまの層がまだ効いているか（終わっていない、取り消されていない、日替わりや帰宅で効かなくなっていない）は、
        -- 見張りと同じ 'activeTalk' で見る。最後の一言は、ロックの中で読む
        dayStart <- dayStartOf now
        stateL <- loadParsed parseState (dir </> "state.json")
        home <- homeWifi
        (seen, _) <- readDoko home now
        st0 <- loadOrFail (dir </> "state.json") initialTrigger stateL
        renewed <- withLock (dir </> "presence.lock") $ do
          (marks, notes) <- marksOrFresh dir
          withLock (dir </> "talk.lock") $ do
            talkf <- saveWithTalk
            let inputs = Inputs now dayStart seen (observedOf talkf) (tfRecent talkf) (mkCurrent marks) (mkWaiting marks) (mkTalk marks)
                -- 居場所の知らせにある、見張りがまだ見ていない帰宅も数える：見張りと同じ decide を、書かずに通す
                (st1, _) = decide defaultParams inputs st0
                recent = tfRecent talkf
                active = isJust (activeTalk defaultParams inputs st1)
                layer = case mkTalk marks of
                  Just t | active -> t { talkSince = now }
                  -- 新しい層。前の層がいつ終わったかを添える（見張りがまだ見ていなかった分を、あとから正しく判断するため）
                  Just t -> Talk now now Nothing (Just (min now (talkEnd defaultParams t recent)))
                  Nothing -> Talk now now Nothing Nothing
            writeAtomic (dir </> "presence.json") (A.encode (marksValue marks { mkTalk = Just layer }))
            forM_ notes (appendLog dir now)
            pure active
        appendLog dir now (if renewed then "续了一笔：专心说话" else "记了一笔：专心说话")
        putStrLn (if renewed then "続けた：話に向き合う" else "記した：話に向き合う（いまから効く）")
      PChange change -> do
        basisAt <- maybe (save >> refuse "この会話には、まだ届いた記録される一言がないので、書かない") pure (cDelivered cache)
        let basis = Received (Heard (Just (T.pack sid)) basisAt)
        (accepted, newer) <- withLock (dir </> "presence.lock") $ do
          (marks, notes) <- marksOrFresh dir
          withLock (dir </> "talk.lock") $ do
            talkf <- saveWithTalk
            let older = [x | Just x <- [eBasis <$> mkCurrent marks, wBasis <$> mkWaiting marks]
                           , heardAt (recHeard x) > basisAt]
                newer = maybe False (`laterThan` basis) (observedOf talkf)
            if not (null older) then pure (False, newer) else do
              writeAtomic (dir </> "presence.json")
                (A.encode (marksValue marks { mkWaiting = Just (Waiting now basis change) }))
              forM_ notes (appendLog dir now)
              pure (True, newer)
        unless accepted (refuse "もっと新しい一言を受けた一筆があるので、書かない")
        appendLog dir now ("记了一笔：" ++ describeChange change ++ "（回的是 " ++ clockJST basisAt ++ " 那一句，一分钟后核）")
        when newer $ putStrLn "（まだ届いていない一言があるので、この一筆はたぶん効かない。読んでから、まだそうなら書き直す）"
        putStrLn ("書き留めた（一分のあと、見張りが確かめてから効く）：" ++ describeChange change)
  where
    usage = do
      hPutStrLn stderr "使い方: hikaru-owntime presence home|out with|own|asleep [どこ]（どこは out のときだけ） | presence clear | presence talk"
      exitWith (ExitFailure 2)
    refuse text = hPutStrLn stderr text >> exitWith (ExitFailure 1)
    situationFrom place doing w = do
      pl <- case (place, w) of
        ("home", Nothing) -> pure Home
        ("out", _) -> pure (Out (maybe "" T.pack w))
        _ -> usage
      d <- case doing of
        "with" -> pure WithYou
        "own" -> pure OnMyOwn
        "asleep" -> pure Asleep
        _ -> usage
      pure (Situation pl d)
    -- ロックの中で読む。初めての一筆だけ、空から始める
    marksOrFresh dir = do
      l <- loadMarks dir
      (marks, notes, _) <- loadOrFail (dir </> "presence.json") (emptyMarks, [], False) l
      pure (marks, notes)

-- | 家の Wi-Fi の知らせ（doko.json）を 'Doko' に。家の Wi-Fi につながっていれば、知らせが古くても家にいる
-- （出るときは知らせが来る）。出かけていて知らせが一日以上古ければ、分からない。
readDoko :: T.Text -> UTCTime -> IO (Doko, Maybe String)
readDoko home now = do
  path <- dokoFile
  l <- loadParsed parseDoko path
  pure $ case l of
    Absent -> (DokoUnknown, Nothing)
    Bad why -> (DokoUnknown, Just (diagnosticText (fileDiagnostic path why) ++ "; location is unknown"))
    Got seen -> (seen, Nothing)
  where
    parseDoko o = do
      updated <- stamp "updated" o
      here <- objectOf "now" o
      wifi <- textOrNull "wifi" here
      since <- stamp "since" here
      before <- case KM.lookup "before" o of
        Nothing -> Right Nothing
        Just A.Null -> Right Nothing
        Just (A.Object b) -> do
          bw <- textOrNull "wifi" b
          bf <- if KM.member "since" b then stampOrNull "since" b else Right Nothing
          bu <- stamp "until" b
          Right (Just (Previous (bw == Just home) bf bu))
        Just _ -> Left "before: invalid value"
      let atHome = wifi == Just home
      Right (if not atHome && diffUTCTime now updated > 24 * 3600 then DokoUnknown else Doko atHome since before)

