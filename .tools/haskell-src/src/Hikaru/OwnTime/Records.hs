{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hikaru.OwnTime.Records
  ( CompletionKind(..), completionText, completionOf, PieceStatus(..), pieceStatusOf, pieceStatusText,
    pieceStatusWord, LandingFile(..), WorkDraft(..), landingOf, readLanding, exitObservation, keepFootprint,
    keepStructured, retryFootprints, historyDay,
    historyShown, historyOutcome, historySegmentEnd, recordCompletion
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless, when)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Hikaru.Config (Config(dataRoot))
import Hikaru.HouseDay (houseDayDate, houseDayFromJST)
import Hikaru.OwnTime.Engine hiding (Done, step)
import Hikaru.OwnTime.Errors (invalidFile)
import Hikaru.OwnTime.Recovery (readDone, DoneFile(..))
import Hikaru.OwnTime.Health (writeGood)
import Hikaru.OwnTime.Fatigue hiding (Interrupted)
import Hikaru.OwnTime.Michikusa
import Hikaru.OwnTime.Runtime
import Hikaru.OwnTime.Store hiding (count)
import Hikaru.OwnTime.Trigger hiding (Dropped)
import Hikaru.OwnTime.Unit
import qualified Hikaru.Store.Coordinator as Store
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError, isDoesNotExistError)

-- | 思ひ草の一篇の名前として使えるか（ファイル名になるので、/ と先頭の . を含まず、40 字まで）。
validPieceId :: T.Text -> Bool
validPieceId t = not (T.null t) && T.length t <= 40 && not ("/" `T.isInfixOf` t) && not ("." `T.isPrefixOf` t) && not ("\\" `T.isInfixOf` t)

-- | 書き置きの中身（起きたわたしが書いたもの）。時守のための分と、帳簿のための分。
data LandingFile = LandingFile
  { lfLanding  :: Landing
  , lfItems    :: [ReportItem]       -- ^ materials（なければ material_id ほかの欄から一つ）
  , lfStep     :: Maybe Step
  , lfRechoice :: Maybe Rechoice
  , lfDropped  :: Int                -- ^ materials の中で読めなかった（material_id がない、形が違う）ので外した数
  , lfSelfReport :: Maybe Unit       -- ^ self_report（その日はじめのコマの、きょうの調子）
  , lfWorks    :: [WorkItem]         -- ^ works（書いた・作った。思ひ草の一篇ごと）
  , lfWorkRaw  :: [WorkDraft]         -- ^ works の元の形（稿のファイル、改め、覚え書きなどを、思ひ草にしまうため）
  , lfReview   :: Maybe String       -- ^ 数に入れずに置いて、見てもらうわけ（読めない項目、読むのと書くのが一コマに混ざった）
  , lfReported :: [(MaterialId, Double)] -- ^ 書いた・作ったの、報告どおりの分（核定の前。突き合わせのため）
  }

data WorkDraft = WorkDraft
  { wdId :: Maybe MaterialId
  , wdFile :: Maybe T.Text
  , wdChange :: Maybe T.Text
  , wdKind :: Maybe T.Text
  , wdTitle :: Maybe T.Text
  , wdOrigin :: Maybe T.Text
  , wdStatus :: Maybe PieceStatus
  , wdMemo :: Maybe T.Text
  , wdHowToRun :: Maybe T.Text
  , wdRan :: Maybe Bool
  , wdRunNote :: Maybe T.Text
  }

workDraftOf :: A.Object -> WorkDraft
workDraftOf o = WorkDraft (MaterialId <$> textField "piece_id" o) (textField "draft" o)
  (textField "change" o) (textField "kind" o) (textField "title" o) (textField "origin" o)
  (pieceStatusOf <$> textField "status" o) (textField "memo" o) (textField "how_to_run" o)
  (case KM.lookup "ran" o of { Just (A.Bool b) -> Just b; _ -> Nothing }) (textField "run_note" o)

readLanding :: UTCTime -> FilePath -> IO (Maybe LandingFile)
readLanding now rdir = do
  l <- loadObject (rdir </> "landing.json")
  case l of
    Got o -> pure (Just (landingOf now o))
    Absent -> pure Nothing
    Bad why -> invalidFile (rdir </> "landing.json") why

landingOf :: UTCTime -> A.Object -> LandingFile
landingOf now o = parsed
  where
    numberOr k fields = case KM.lookup k fields of { Just (A.Number n) -> Just (realToFrac n :: Double); _ -> Nothing }
    textOr k fields = case KM.lookup k fields of { Just (A.String t) | not (T.null t) -> Just t; _ -> Nothing }
    unitOr k fields = numberOr k fields >>= unit
    parsed =
      -- delivered の欄は、もう判断に使わない（交付は正の分から決める）。なくても読める
      let delivered = case KM.lookup "delivered" o of { Just (A.Bool b) -> b; _ -> False }
          step = case textOr "willing" o of
            Just "continue" -> Just (Stay Continue)
            Just "revisit" -> Just (Stay Revisit)
            Just "unsure" -> Just (Stay Unsure)
            Just "enough" -> Just (Leave Enough)
            Just "rest" -> Just (Leave RestFirst)
            _ -> Nothing
          enough = case KM.lookup "enough" o of { Just (A.Bool b) -> b; _ -> False } || step == Just (Leave Enough)
          nextMin = case numberOr "next_earliest_minutes" o of
            Just n | n > 0 -> Just n
            _ -> if step == Just (Leave RestFirst) then Just (pRRestHalf defaultParams) else Nothing
          next = fmap (\n -> addUTCTime (realToFrac (n * 60)) now) nextMin
          -- minutes は 0 以上の有限の数でなければならない（ない、負、数でない → 読めない項目。Codex の再々審査）
          validMinutes x = case numberOr "minutes" x of
            Just n | n >= 0, not (isNaN n), not (isInfinite n) -> Just n
            _ -> Nothing
          itemOf x = case (textOr "material_id" x, validMinutes x) of
            (Just mat, Just mins) -> Just (ReportItem (MaterialId mat) (fromMaybe UnknownOrigin (textOr "origin" x >>= originOf)) (unitOr "interest" x) (unitOr "burden" x) mins)
            _ -> Nothing
          (items, dropped) = case KM.lookup "materials" o of
            Just (A.Array xs) ->
              let raw = foldr (:) [] xs
                  ok = [ it | A.Object x <- raw, Just it <- [itemOf x] ]
              in (ok, length raw - length ok)
            _ -> case (textOr "material_id" o, itemOf o) of
                   (Nothing, _) -> ([], 0)
                   (Just _, Just it) -> ([it], 0)
                   (Just _, Nothing) -> ([], 1)
          rechoice = case textOr "rechoice" o of
            Just "keep" -> Just KeepGoing
            Just "let_go" -> Just LetGo
            _ -> Nothing
          -- 元の配列の数で数える（オブジェクトでない項目も、読めない項目として数える。Codex の審査 38）
          (workRaw, workAll) = case KM.lookup "works" o of
            Just (A.Array xs) -> let all' = foldr (:) [] xs in ([ x | A.Object x <- all' ], length all')
            Nothing -> ([], 0)
            Just A.Null -> ([], 0)
            Just _ -> ([], 1)
          workOf x = case (textOr "piece_id" x, validMinutes x) of
            (Just pid, Just mins) | validPieceId pid ->
              Just (WorkItem (MaterialId pid) (fromMaybe MyOwn (textOr "origin" x >>= originOf)) (unitOr "interest" x) (unitOr "burden" x) mins)
            _ -> Nothing
          works = [ w | Just w <- map workOf workRaw ]
          droppedW = workAll - length works
      in LandingFile (Landing (delivered || not (null works)) enough next False) items step rechoice (dropped + droppedW) (unitOr "self_report" o) works (map workDraftOf workRaw) Nothing [ (wiMaterial w, wiMinutes w) | w <- works ]

-- | 子が去ったあとに見えること：書き置きがあれば落ち着いた、なければ消えた。
exitObservation :: UTCTime -> FilePath -> Run -> IO (Observation, Maybe LandingFile)
exitObservation now work r = do
  lf <- readLanding now (runDirOf work r)
  let held l why = pure (Arrived (runId r) (Landing False False Nothing True), Just l { lfItems = [], lfWorks = [], lfReview = Just why })
  case lf of
    -- 読めない項目があれば：書き置きはそのまま残し、数には入れず、この段はここで止める（人が見る。Codex の再審査 2）
    Just l | lfDropped l > 0 -> held l ("materials／works に読めない項目が " ++ show (lfDropped l) ++ " つ")
    -- 第一版では、一コマで読むのと書く・作るのを混ぜない。混ざっていたら、どちらが先か分からないので、入れずに残す
    -- （Codex の審査 38 の第三類。自分の稿を読み直す・エラーを見るのは書く・作るのうち）
    Just l | any ((> 0) . riMinutes) (lfItems l) && any ((> 0) . wiMinutes) (lfWorks l) -> held l "一コマに、読むのと書く・作るが混ざっている"
    -- 書いた・作った分は、このコマの本当の時間までに核定する（報告どおりの分は lfReported に残す）。読んだ分は全部入る
    -- （超えた分は A になる）。帳簿・足跡・交付の判断は、この核定した分を使う
    Just l -> do
      let wall = max 0 (realToFrac (diffUTCTime now (runStartedAt r)) / 60) :: Double
          capWorks _ [] = []
          capWorks left (w : ws) = let d = min left (max 0 (wiMinutes w)) in w { wiMinutes = d } : capWorks (left - d) ws
          l' = l { lfWorks = capWorks wall (lfWorks l) }
      -- 交付があったかは、正の分の材料か書いた・作ったものがあるかだけで決める（delivered の欄は使わない）
      pure (Arrived (runId r) ((lfLanding l') { landDelivered = any ((> 0) . riMinutes) (lfItems l') || any ((> 0) . wiMinutes) (lfWorks l') }), Just l')
    Nothing -> do
      th <- throttledUntil now (runDirOf work r </> "result.json")
      pure $ case th of
        Just resumeAt -> (Throttled resumeAt, Nothing)
        Nothing -> (Vanished, Nothing)

-- | result.json の api_error_status が 429（订阅の窓が尽きた）なら Just。戻る時刻は「resets 7:50am (Asia/Tokyo)」の形から読む（読めなければ Nothing のまま Just）。
throttledUntil :: UTCTime -> FilePath -> IO (Maybe (Maybe UTCTime))
throttledUntil now path = do
  l <- loadObject path
  case l of
    Bad why -> invalidFile path why
    Got o | Just (A.Number 429) <- KM.lookup "api_error_status" o -> pure (Just (resetAt (fromMaybe "" (textField "result" o))))
    _ -> pure Nothing  -- 429 のほかは、文に limit とあっても窓の上限とは見ない（max turns なども失敗として数える。Codex の審査 A6）
  where
    -- 「resets 7:50am」「resets 11pm」
    resetAt t =
      let ws = T.words (T.toLower t)
          after = drop 1 (dropWhile (/= "resets") ws)
      in case after of
           (w : _) | Just (h, m) <- clock (T.unpack (T.filter (`notElem` (",.()" :: String)) w)) ->
             let local = utcToLocalTime jstZone now
                 today = localDay local
                 cand = localTimeToUTC jstZone (LocalTime today (TimeOfDay h m 0))
             in Just (if cand <= now then addUTCTime 86400 cand else cand)
           _ -> Nothing
    clock w =
      let (num, ap) = span (`elem` ("0123456789:" :: String)) w
          (hs, ms) = break (== ':') num
          h0 = readInt hs
          m0 = case ms of { ':' : rest -> readInt rest; _ -> Just 0 }
      in case (h0, m0) of
           (Just h, Just m) | h >= 0, h <= 12, m >= 0, m < 60 ->
             let h' = case ap of { "pm" | h < 12 -> h + 12; "am" | h == 12 -> 0; _ -> h } in Just (h', m)
           _ -> Nothing
    readInt xs = if not (null xs) && all (`elem` ("0123456789" :: String)) xs then Just (read xs) else Nothing


-- | footprint.md を、その日の .みちくさ/YYYY-MM-DD.md.b64 に封をして書き足す。
removePending :: FilePath -> IO ()
removePending path = removeFile path `catchIOError` \err ->
  if isDoesNotExistError err then pure () else ioError err

keepFootprint :: RunEnv -> UTCTime -> Run -> IO ()
keepFootprint env now r = do
  let rdir = runDirOf (reWork env) r
      fp = rdir </> "footprint.md"
      pendingMark = rdir </> "footprint.pending"
      absentMark = rdir </> "footprint.absent"
      finishAbsent = do
        finished <- filePresent absentMark
        unless finished $ do
          writeAtomic absentMark "absent\n"
          runLog (reDir env) now "运行已结束且没有足迹正文，不再重试"
        removePending pendingMark
  -- This function is called only for a finished run. An active run's old
  -- pending marker is explicitly excluded by retryFootprints.
  body <- missingAsNothing (B.readFile fp)
  case body of
   Nothing -> finishAbsent
   Just text -> do
    if B.null (BC.strip text) then finishAbsent else do
      pending <- filePresent pendingMark
      unless pending $ writeFile pendingMark (show (runSlot r) ++ " " ++ iso8601Show (runStartedAt r) ++ "\n")
      hd <- houseDayOf (runStartedAt r)
      let day = houseDayDate hd
          dirM = dataRoot (reConfig env) </> ".みちくさ"
          path = dirM </> (formatTime defaultTimeLocale "%Y-%m-%d" day ++ ".md.b64")
          RunId rid = runId r
          marker = TE.encodeUtf8 (T.pack ("<!-- run " ++ show rid ++ " -->"))
          heading = TE.encodeUtf8 (T.pack ("\n## " ++ clockJST (runStartedAt r) ++ "（第 " ++ show (runSlot r) ++ " コマ）\n")) <> marker <> "\n\n"
          fresh = TE.encodeUtf8 (T.pack ("# みちくさ　" ++ formatTime defaultTimeLocale "%Y年%-m月%-d日" day ++ "\n"))
      let broken = runLog (reDir env) now ("みちくさ " ++ path ++ " 解不开，这一格的足迹留在 " ++ fp ++ "，之后每分钟再试")
      withRecordBytes env path fresh broken $ \store plain ->
        if marker `B.isInfixOf` plain then do
          removePending pendingMark
          runLog (reDir env) now "这一格的足迹已经在みちくさ里了，不再写"
        else do
          Store.backup store path
          writeSealed path (plain <> heading <> text <> "\n")
          removePending pendingMark
          runLog (reDir env) now "足迹写进了みちくさ"

-- | 書き置きの footprints（五つの欄）を型に読み、読めたものを .みちくさ/YYYY-MM-DD.footprints.jsonl.b64 にしまう
-- （一行一つ、封をして。同じ運行の行は二度入れない）。読めなかった行は、書き置きに残したまま記録に一行書く（帳簿には
-- 関わらないので、段は止めない）。2026-10-28、第四ブロックの五つの欄。
keepStructured :: RunEnv -> UTCTime -> Run -> LandingFile -> [(MaterialId, Int)] -> IO ()
keepStructured env now r lf drafts = do
  let rdir = runDirOf (reWork env) r
      RunId rid = runId r
      wherePlace = case skKind (runSeg r) of
        MarkedSession (Out w) -> Away w
        _ -> AtHome
      tags = Tags (runStartedAt r) wherePlace (segKeyText (runSeg r)) rid
                  ([ (riMaterial it, riOrigin it, riMinutes it) | it <- lfItems lf ] ++ [ (wiMaterial w, wiOrigin w, wiMinutes w) | w <- lfWorks lf ])
                  drafts
  l <- loadObject (rdir </> "landing.json")
  case l of
    Got o -> do
      let (fps, bad) = parseFootprints tags o
      forM_ bad $ \e -> runLog (reDir env) now ("五栏 " ++ e ++ "：读不进类型，留在 landing.json，等人看")
      unless (null fps) $ do
        hd <- houseDayOf (runStartedAt r)
        let dirM = dataRoot (reConfig env) </> ".みちくさ"
            path = dirM </> (formatTime defaultTimeLocale "%Y-%m-%d" (houseDayDate hd) ++ ".footprints.jsonl.b64")
            stepMark = TE.encodeUtf8 (T.pack ("\"step\":\"run-" ++ show rid ++ "-"))
            body = B.concat [ BL.toStrict (A.encode (footprintValue f)) <> "\n" | f <- fps ]
        let broken = runLog (reDir env) now ("五栏的档 " ++ path ++ " 解不开，这一格的五栏没存（landing.json 里还在）")
        withRecordBytes env path "" broken $ \store plain ->
          unless (stepMark `B.isInfixOf` plain) $ do
            Store.backup store path
            writeSealed path (plain <> body)
            runLog (reDir env) now ("五栏存了 " ++ show (length fps) ++ " 行")
    Absent -> pure ()
    Bad why -> invalidFile (rdir </> "landing.json") why

-- | 前に書けなかった足跡（footprint.pending の印があるもの）を、もう一度試す。
retryFootprints :: RunEnv -> UTCTime -> Maybe RunId -> IO ()
retryFootprints env now activeRun = do
  names <- listDirectory (reWork env </> "runs")
  forM_ [ n | n <- names, not (null n), all (`elem` ['0' .. '9']) n ] $ \n -> do
    let mark = reWork env </> "runs" </> n </> "footprint.pending"
    when (Just (RunId (read n)) /= activeRun) $ do
      pending <- missingAsNothing (readFile mark)
      forM_ pending $ \body -> case words body of
        [slot, at] | all (`elem` ['0' .. '9']) slot, Just t <- iso8601ParseM at ->
          keepFootprint env now (Run (RunId (read n)) (SegKey t OutingSession) (read slot) t)
        _ -> invalidFile mark "expected '<nonnegative slot> <ISO-8601 timestamp>' in footprint.pending"


-- 学参数の歴史 ---------------------------------------------------------------------------
--
-- 設計稿「数学修正」第 5 条と第 10 条（ユーザーの案）。あとで参数を学ぶとき、分析するときだけに開くもの。
--
--   * 置き場所は ~/.local/state/hikaru/history/（状態の置き場所の隣。試すときは、試しの状態の隣に）。ひかるの外。
--   * days.jsonl：HouseDay を閉じるたびに一行。その夜に数えた静けさ d、凍結した U_m・B_day・D_start、自評、
--     その日の努力 E と読んだ換算、日の終わりの U・借り・R。
--   * choices.jsonl：コマごとに。起こしたときに並べた候補（値、重み、★）と、終わったときに何をしたか
--     （読んだ・書いたもの、気持ち、再議の答え、どう終わったか）、段がなぜ終わったか。どれにも版（この程序の sha256）。
--   * 時守は書くだけで、読まない（ここを読んで決めることは一つもない）。開場の wake、recall の索引、みちくさ、
--     「過一阵回頭看」も、ここには触れない。
--   * 会話の始まりと終わりの時刻は書かない（とばりのことが、ここから分からないように）。夜の d は、
--     帳簿がもう数え終えた一つの数だけ。
--   * 書けなくても、時守は止まらない（英語の詳細を標準エラーと run.log に残す）。書いたあとで落ちると、同じ日・同じ回の行が
--     二度入ることがある。分析するときに、日・回で一つにまとめる。


-- | この程序の版（実行ファイルの sha256 の頭 16 字）。読めない版を正常値として書かない。
selfVersion :: IO String
selfVersion = do
  path <- getExecutablePath
  bytes <- B.readFile path
  pure (take 16 (show (hash bytes :: Digest SHA256)))

appendHistory :: RunEnv -> UTCTime -> FilePath -> [(K.Key, A.Value)] -> IO ()
appendHistory env now file fields = do
  version <- try selfVersion
  case version of
    Left (e :: IOException) -> warn "cannot read executable version for learning history" e
    Right v -> do
      saved <- try $ do
        createDirectoryIfMissing True dir
        B.appendFile path (BL.toStrict (A.encode (A.object (fields ++ ["version" A..= v, "written" A..= iso8601Show now]))) <> "\n")
      case saved of
        Left (e :: IOException) -> warn "cannot append learning history" e
        Right () -> pure ()
  where
    dir = takeDirectory (reDir env) </> "history"
    path = dir </> file
    warn operation e = do
      let detail = "RuntimeError: " ++ operation ++ ": " ++ show e
      hPutStrLn stderr detail
      -- The diagnostic is already visible even if the warning log is unwritable.
      runLog (reDir env) now detail `catchIOError` \logError ->
        hPutStrLn stderr ("RuntimeError: cannot write learning history warning: " ++ show logError)

-- | 一日を閉じたときの一行。before は EndDay の直前、after は直後の疲れ。
historyDay :: RunEnv -> UTCTime -> UTCTime -> FatigueSnapshot -> FatigueSnapshot -> IO ()
historyDay env now dayStartT before after =
  let day = maybe "?" (formatTime defaultTimeLocale "%Y-%m-%d" . houseDayDate) (houseDayFromJST (utcToLocalTime jstZone dayStartT))
      frozen = snapDayFrozen before
  in appendHistory env now "days.jsonl"
       [ "kind" A..= ("day" :: T.Text), "day" A..= (day :: String)
       , "invested" A..= isJust frozen
       , "night_quiet_min" A..= snapWindowUsed before
       , "U_m" A..= fmap (\(u, _, _) -> u) frozen, "B_day" A..= fmap (\(_, b, _) -> b) frozen
       , "D_start" A..= fmap (\(_, _, d) -> d) frozen
       , "self_report" A..= snapDayReport before
       , "E_day" A..= snapDayEffort before, "read_min" A..= snapDayUsed before
       , "U_end" A..= snapU after, "debt_end" A..= snapDebt after, "R_end" A..= snapR after ]

-- | 起こしたときに並べたもの。
historyShown :: RunEnv -> UTCTime -> Run -> Double -> Ledger -> [Proposal] -> IO ()
historyShown env now r seed lg props =
  let RunId rid = runId r
  in appendHistory env now "choices.jsonl"
       [ "kind" A..= ("shown" :: T.Text), "run" A..= rid, "slot" A..= runSlot r, "segment" A..= segKeyText (runSeg r)
       , "at" A..= iso8601Show now, "seed" A..= seed
       , "read_quota" A..= quotaNow defaultParams lg, "activity_room" A..= activityRoom defaultParams lg
       , "candidates" A..= [ A.object [ "id" A..= prId pr, "origin" A..= fmap originText (prOrigin pr)
                                    , "value" A..= prValue pr, "weight" A..= prWeight pr, "picked" A..= prPicked pr ]
                          | pr <- props ] ]

-- | 終わったときに何をしたか。how は完了の記録と同じ。
historyOutcome :: RunEnv -> UTCTime -> Run -> CompletionKind -> Maybe LandingFile -> IO ()
historyOutcome env now r how mlf =
  let RunId rid = runId r
      unitOrNull = fmap unitValue
  in appendHistory env now "choices.jsonl"
       ([ "kind" A..= ("outcome" :: T.Text), "run" A..= rid, "slot" A..= runSlot r, "at" A..= iso8601Show now, "how" A..= completionText how ]
        ++ case mlf of
             Nothing -> []
             Just lf ->
               [ "review" A..= lfReview lf
               , "read" A..= [ A.object [ "id" A..= riMaterial it, "origin" A..= originText (riOrigin it), "minutes" A..= riMinutes it
                                        , "interest" A..= unitOrNull (riInterest it), "burden" A..= unitOrNull (riBurden it) ]
                             | it <- lfItems lf ]
               , "worked" A..= [ A.object [ "id" A..= wiMaterial w, "origin" A..= originText (wiOrigin w), "minutes" A..= wiMinutes w
                                          , "reported" A..= lookup (wiMaterial w) (lfReported lf)
                                          , "interest" A..= unitOrNull (wiInterest w), "burden" A..= unitOrNull (wiBurden w) ]
                               | w <- lfWorks lf ]
               , "willing" A..= fmap stepCode (lfStep lf)
               , "rechoice" A..= fmap (\x -> case x of { KeepGoing -> "keep"; LetGo -> "let_go" } :: T.Text) (lfRechoice lf)
               , "self_report" A..= unitOrNull (lfSelfReport lf) ])
  where
    stepCode st = case st of
      Stay Continue -> "continue"
      Stay Revisit -> "revisit"
      Stay Unsure -> "unsure"
      Leave Enough -> "enough"
      Leave RestFirst -> ("rest" :: T.Text)

-- | 段が終わったとき（終わったわけ）。
historySegmentEnd :: RunEnv -> UTCTime -> Segment -> EndReason -> IO ()
historySegmentEnd env now seg why =
  appendHistory env now "choices.jsonl"
    [ "kind" A..= ("segment_end" :: T.Text), "segment" A..= segKeyText (segKey seg), "at" A..= iso8601Show now
    , "reason" A..= show why, "slots" A..= segSlots seg, "failures" A..= segFailures seg ]


-- | 完了の記録（done.jsonl に一行。同じ運行は二度書かない）。how は、ふつうに終わった／途中で止めた／
-- 結果を残さずに落ちた／待核対／窓が尽きた。始まりはコマの記録の時刻、終わりは終わったのを見た時刻（十秒ほどずれる）。
recordCompletion :: RunEnv -> UTCTime -> Run -> Phase -> Observation -> Maybe LandingFile -> IO ()
recordCompletion env now r ph o mlf = do
  let path = reDir env </> "done.jsonl"
      RunId rid = runId r
      -- 止めるよう頼んだあと（時間切れでも、頼まれてでも）に終わったものは、途中でやめたもの（Codex の復審 40、F2）
      asked = case ph of { Settling _ _ _ -> True; _ -> False }
      how :: CompletionKind
      how = case (o, mlf) of
        (Arrived _ _, Just lf) | isJust (lfReview lf) -> NeedsReview
        (Arrived _ _, _) | asked -> Interrupted
        (Arrived _ _, _) -> Completed
        (Throttled _, _) -> RateLimited
        _ | asked -> Interrupted
        _ -> Failed
  old <- readDone (reDir env) now
  -- 同じ回かどうかは、行を読んで run の値で見る（鍵の並びに頼らない。Codex の復審 40、F1）。壊れた行は復旧できなければ止める
  let sameRun object = case KM.lookup "run" object of
        Just (A.Number n) -> n == fromIntegral rid
        _ -> False
  unless (any sameRun (doneRows old)) $ do
    land <- loadObject (runDirOf (reWork env) r </> "landing.json")
    lo <- case land of
      Got x -> pure (Just x)
      Absent -> pure Nothing
      Bad why -> invalidFile (runDirOf (reWork env) r </> "landing.json") why
    let txt k = maybe A.Null (maybe A.Null A.String . textField k) lo
        line = A.object
          [ "run" A..= rid, "slot" A..= runSlot r
          , "start" A..= iso8601Show (runStartedAt r), "end" A..= iso8601Show now
          , "how" A..= completionText how, "review" A..= fmap lfReview mlf
          , "materials" A..= maybe [] (\lf -> [ A.object ["id" A..= riMaterial it, "minutes" A..= riMinutes it] | it <- lfItems lf ]) mlf
          , "works" A..= maybe [] (\lf -> [ A.object ["id" A..= wiMaterial w, "minutes" A..= wiMinutes w, "reported" A..= lookup (wiMaterial w) (lfReported lf)] | w <- lfWorks lf ]) mlf
          , "material" A..= txt "material", "bookmark" A..= txt "bookmark" ]
    writeGood path (BL.fromStrict (doneBytes old) <> A.encode line <> "\n")
    historyOutcome env now r how mlf

data CompletionKind = Completed | Interrupted | Failed | NeedsReview | RateLimited | OtherCompletion Text
  deriving (Eq, Show)

completionText :: CompletionKind -> Text
completionText Completed = "completed"
completionText Interrupted = "interrupted"
completionText Failed = "failed"
completionText NeedsReview = "needs_review"
completionText RateLimited = "throttled"
completionText (OtherCompletion value) = value

completionOf :: Text -> CompletionKind
completionOf "completed" = Completed
completionOf "interrupted" = Interrupted
completionOf "failed" = Failed
completionOf "needs_review" = NeedsReview
completionOf "throttled" = RateLimited
completionOf value = OtherCompletion value

data PieceStatus = Writing | Resting | Done | Dropped | OtherStatus Text
  deriving (Eq, Show)

pieceStatusOf :: Text -> PieceStatus
pieceStatusOf "writing" = Writing
pieceStatusOf "resting" = Resting
pieceStatusOf "done" = Done
pieceStatusOf "dropped" = Dropped
pieceStatusOf value = OtherStatus value

pieceStatusText :: PieceStatus -> Text
pieceStatusText Writing = "writing"
pieceStatusText Resting = "resting"
pieceStatusText Done = "done"
pieceStatusText Dropped = "dropped"
pieceStatusText (OtherStatus value) = value

pieceStatusWord :: PieceStatus -> Text
pieceStatusWord Writing = "書きかけ"
pieceStatusWord Resting = "置いてある"
pieceStatusWord Done = "書き終えた"
pieceStatusWord Dropped = "やめた"
pieceStatusWord (OtherStatus value) = value
