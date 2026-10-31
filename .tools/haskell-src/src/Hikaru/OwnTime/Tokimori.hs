{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | 自分の時間の見張り（hikaru-owntime）。いまは試運転だけ（2026-10-22 から。2026-10-23 に最終案のとおり作り直した：
-- デスクトップの 参考文献精读_2026-10-22/26_修正方案定稿.md）。
--
-- > hikaru-owntime talk-mark                UserPromptSubmit の hook。その会話を前に読んだところから続けて読み、記録される
-- >                                         一言の時刻を「ユーザーの最後の一言」に入れる（中身は読まないし、残さない）。
-- > hikaru-owntime turn-end                 何もしない（2026-10-22 の夜の形の名残。設定から外すまで、呼ばれても 0 で終わる）。
-- > hikaru-owntime presence 所 様子 [どこ]   話しているわたしが「いまどこで何をしているか」を一筆書き留める（まず待ちの状態へ。
-- >                                         書いて一分のあと、見張りが確かめて効かせる）。所は home / out、様子は with / own / asleep、
-- >                                         どこは out のときだけ。presence clear で消す。presence talk は「話に向き合う」層
-- >                                         （所と様子の上に重ねる。書いた瞬間から効く）。とばりの中の会話からは書かない。
-- > hikaru-owntime check --dry-run          launchd から一分ごと。決める前に、書き足された会話を読み足し、待っている一筆を確かめ、
-- >                                         家にいるか（doko.json）、最後の一言、一筆から、第二ブロックの決まり（'Hikaru.OwnTime.Trigger'）で
-- >                                         始める・一時停止・再開・終わるを決めて、試運転の記録に一行書くだけ。わたしは起こさない。
-- > hikaru-owntime run                      時守（2026-10-28 から）。launchd から一分ごと。見張りの state.json を時段として読み、
-- >                                         runner.json（実行の段階）と突き合わせて、起こす・止めるよう伝える・何もしない、を決める。
-- >                                         起こすときは claude -p を子として起こし、収まるまでここで待つ。くわしくは下の「時守」の節。
-- > hikaru-owntime now                      話しているわたしが、ユーザーに「いま何してる」「さっき何読んでた」と聞かれたときに見る。
-- >                                         家のわたしがいま読んでいるか、次を待っているか、最後に終わったコマで何を読んだか、
-- >                                         しおり、きょう読んだ量を、日本語で数行（2026-10-28 から）。
-- > hikaru-owntime status                   いまの状態と、試運転の記録の終わりのほうを出す。
--
-- 置き場所は ~/.local/state/hikaru/owntime/（HIKARU_OWNTIME_STATE で変えられる）：
--
--   * talk.json：見た最後の記録される一言（時刻と、どの会話か。別の会話に同じ時刻の一言があれば tied）と、
--     直近一日の記録される一言の時刻（層の終わりを順に見分けるため）。数も中身も持たない。進めるだけで、戻さない。
--   * presence.json：効いている一筆、待っている一筆、話に向き合う層。
--   * sessions/：会話ごとの控え。どこまで読んだか（と inode）、とばりの状態（外／中／分からない）、
--     列の模型（それぞれの項目が合言葉か・道具の知らせか、列の中か・取り出されて届くのを待っているか・届いて列から外れるのを
--     待っているか。中身も指紋も持たない）、この会話で見た最後の記録される一言、届いた最後の記録される一言。
--   * state.json：見張りのいまの状態
--   * dryrun.log：試運転の記録（中国語で書く）。ユーザーと突き合わせが済んだら消す。
--   * trouble.log：読めなかったわけ（中身は書かない。記録される部分で起きたことだけ。とばりの中、分からない
--     状態、合言葉が列にあるあいだ、合言葉にまつわる異常は、一行も書かない。調べるためだけのもので、話すわたしは読まない）
--
-- とばりの中かどうかは、核と同じ 'Hikaru.Conversation.Parse' と 'Hikaru.Conversation.Hidden' の合言葉の見分けで、
-- 届いた一言ごとに進める。三つの状態：とばりの外／中／分からない。始まりの合言葉で下り、終わりの合言葉で上がる。
-- 分からない（読めない行のあと、列の異常のあと）は、数えず、一筆も書かせず、次に届いた完全な始まりか終わりの合言葉で
-- 元に戻る（壊れた行に何が書いてあっても、始まりの合言葉のあとはとばりの中、終わりの合言葉のあとは外、と決まるから）。
--
-- 一言の見え方（2026-10-22・23 に本物の記録で確かめた。最終案の H1〜H7）：
--   * ユーザーが送った瞬間に queue-operation の enqueue の行（中身つき）。空いていればすぐ dequeue と user の行。
--     わたしが返事を書いているあいだなら、そのひとまとまりの道具が全部終わってから queued_command の行（届いた）、そのあと
--     remove の行（理由は absorbed_mid_turn）。列は先入れ先出し。
--   * だから、見た最後の一言は enqueue の行で進め（合言葉が列にあるあいだは、その後ろに並んだ一言は数えない）、
--     届いた最後の一言は user の行と queued_command の行で進める。一筆が受けた一言は、届いたほう。
--   * 列の模型は、項目の種類（合言葉／ふつう／道具の知らせ）と段階だけ。届いた一言は、同じ種類の先頭の項目に当て、
--     その前に残っていた項目は、記録が抜けたものとして片づける（その中に合言葉があれば「分からない」へ）。
--     合言葉が届かないまま列を去ったときも「分からない」へ。
--
-- Recovery protocol (v1, 2026-10-30):
--   stuck = {markerVersion:1, failures:[{worker,fault:{file,reason},first,
--             latest,consecutive,event}]}. Workers are Check / Run / Launch / Talk.
--   Only the successful worker clears its entry. Resumed is recorded in run.log
--   and health.json before the last entry's removal deletes stuck. A repeated
--   identical fault increments consecutive without generating another alert.
--   health.json = {version:1,events:[{serial,owner,at,kind,problem,method,broken}],
--                  attempts:[{by,cause,repairs}]}.
--   EventKind = Stopped | AutoRecovered | Resumed. Monotone serials, not clock
--   strings, identify notices. Timestamps are UTC; low-level reasons are English.
--   notified/mezame.json is the last delivered session-start serial;
--   notified/oshirase-<SHA256(session id)>.json is each conversation's receipt.
--   Receipts advance only after output. No event means no notification writes.
--
-- Cache / talk repairs rescan original transcripts before replacing anything;
-- presence resets to no entry. Runner resets to idle only after lsof and PID
-- checks prove no child remains, retaining unused IDs above work / done / ledger.
-- engine.json, state.json and done.jsonl keep a .good last committed version,
-- atomically written under the same runner.lock / check.lock as the body.
-- Completion repair proves each damaged row from .good and preserves EVERY
-- good row byte-for-byte. Only a genuinely unfinished final JSON row is dropped.
-- Other IO errors do not authorize replacement; launchd keeps its minute retry.
-- The same fault can be repaired three consecutive times. A fourth recurrence
-- stops for manual repair. Only an invocation without that worker's repair resets
-- the streak; successfully repairing a file does not itself reset the streak.
--
-- A repair stages <file>.repair, preserves the exact bad bytes in
-- broken/<UTC timestamp>-<event serial>-<original name>, and stores
-- <file>.repair.json = {repairEvent,replacementDigest,originalDigest}. Only then is the body
-- atomically replaced. A crash leaves either the old body or the new body, never
-- an absent body mistaken for an initial state. The same loader finishes a
-- pending receipt (and run.log) on retry. Archives and .good files are not deleted.
-- Business locks are outermost; health.lock is innermost. Shared core record
-- locks (R3), footprint terminal states (R4), StartFailures, and warning-only
-- learning-history writes keep their existing contracts.
--
-- 本番の核（hikaru-core）とは別に、同じ源から作る（owntime/build.sh）。核に入れるのは、重构清单（`重构清单_2026-10-08.md`）の手順で。

-- 時守（hikaru-owntime run） ---------------------------------------------------------
--
-- 決めるのは 'Hikaru.OwnTime.Runner.tick'（純粋）。ここは、それに見えたことを渡し、出てきた動作をする。
-- 読む量・疲れ・慣れと飽き・先行量 A は帳簿（'Hikaru.OwnTime.Ledger'、engine.json）が引擎に当てる。
--
--   * 時段は、見張りの state.json の session から読む（null → 閉じている、paused → 一時停止、それ以外 → 開いている。
--     段の鍵は、始まった時刻と種類）。
--   * 実行の段階は runner.json。起こした子の pid も一緒に持つ。
--   * 起こすときは、この入口が claude -p を子として起こし、収まるまでここで待つ（10 秒ごとに時段を読み直して tick に
--     渡す。止めるよう言われたら SIGTERM、一分たっても収まらなければ SIGKILL）。launchd は同じ名札を重ねて起こさない
--     ので、待っているあいだは次の一分は来ない。待っているあいだに自分が死んだら（再起動など）、次の run が
--     runner.json の pid の生き死にと、書き置きのファイルがあるかどうかから拾う。
--   * 帳簿：一分ごとに、日の変わり目（EndDay）、書き置き（Deliver と、コマの始まりからの裏づけ）、時段に合わせた時間の
--     流し方（外／一時停止／開いている）の順で当て、engine.json に書く。起こす前に、段の始まりなら静けさを精算して
--     枠を聞き、候補に値と重みをつけて brief に並べる（引いた一つに ★）。
--   * 書き置き（landing.json）は、起きたわたしが作業場に書く。手順書にある欄：delivered、enough、minutes、
--     next_earliest_minutes、material、bookmark と、帳簿のための material_id、origin、interest、burden、willing、
--     rechoice。footprint.md は、その日の .みちくさ/YYYY-MM-DD.md.b64 に封をして書き足す（核の Seal にみちくさを
--     登記するまでの仮の書き方。平文は家に残さない）。
--   * 枠（Room）は、帳簿の枠（努力分）が 5 以上で、その日に読んだ換算が pDayCap 未満で、休むと決めた時刻を過ぎている。
--   * 子には、手順書を system prompt に足して、道具は Read / Write / WebFetch / WebSearch / Glob / Grep と、めがね
--     （owntime/megane.py。文字化けするページを取り寄せる。Bash はこの一つの形だけ許す）だけ、作業場を
--     cwd に、inbox/ だけ余分に読めるようにして渡す。令牌は ~/.config/hikaru/claude/oauth-token から読んで
--     子の環境にだけ入れる（わたしは中を見ないし、どこにも書かない）。
--
-- 置き場所（stateDir の中）：runner.json、engine.json、runner.lock、run.log（中国語）、work/runs/<運行の番号>/
-- （brief.md、previous.md、landing.json、footprint.md、result.json、stderr.log）。

module Hikaru.OwnTime.Tokimori
  ( run, ownsPid, spawnRun, newCandidates, nowCmd, status
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, onException, try)
import Control.Monad (foldM, forM_, unless, void, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace)
import Data.List (intercalate, sort, isPrefixOf)
import Data.Scientific (toBoundedInteger)
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, diffUTCTime, formatTime, zonedTimeToUTC)
import Hikaru.Config (Config(..), configuredNow, loadConfig)
import Hikaru.HouseDay (houseDayDate)
import Hikaru.OwnTime.Engine
import Hikaru.OwnTime.Errors (invalidFile, operationFailed, stateFailure)
import qualified Hikaru.OwnTime.Health as H
import Hikaru.OwnTime.Recovery
import Hikaru.OwnTime.Fatigue hiding (Interrupted)
import Hikaru.OwnTime.Michikusa
import Hikaru.OwnTime.Records
import Hikaru.OwnTime.Omoigusa
import Hikaru.OwnTime.Runtime
import Hikaru.OwnTime.Store
import Hikaru.OwnTime.Trigger
import Hikaru.OwnTime.Unit
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (IOMode(WriteMode), withFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.Signals (Signal, nullSignal, sigKILL, sigTERM, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process
import System.Random (randomRIO)

loadRunning :: FilePath -> UTCTime -> FilePath -> (A.Object -> Parse a) -> a -> IO a
loadRunning dir now name parse initial =
  loadHealing H.Run dir now (dir </> name) parse initial $
    if name == "runner.json"
      then Rebuild "reset runner to idle after confirming no child process; retain unused run IDs" (idleRunner dir now)
      else LastGood

-- lsof enumerates open files and working directories in the private run tree.
-- Any live holder or diagnostic means absence has NOT been established.
idleRunner :: FilePath -> UTCTime -> IO B.ByteString
idleRunner dir now = do
  let runs = dir </> "work" </> "runs"
  (code, out, err) <- readProcessWithExitCode "/usr/sbin/lsof" ["-n", "-P", "+D", runs, "-Fp"] ""
  unless (null err && code == ExitFailure 1 && null out) $ do
    let parsePid ('p':digits)
          | not (null digits), all (\c -> c >= '0' && c <= '9') digits
          , Just pid <- (readMaybe digits :: Maybe Integer)
          , pid > 0, pid <= fromIntegral (maxBound :: ProcessID) = Just (fromIntegral pid)
        parsePid _ = Nothing
        uncertain = stateFailure (dir </> "runner.json") "cannot confirm absence of child processes; runner was not reset"
    case traverse parsePid (lines out) of
      Just pids | null err, code == ExitSuccess, not (null pids) -> do
        living <- mapM alive pids
        when (or living) uncertain
      _ -> uncertain
  names <- listDirectory runs
  completed <- readDone dir now
  ledger <- loadHealing H.Run dir now (dir </> "engine.json") (parseLedger defaultParams)
    (ledgerStart defaultParams now now) LastGood
  let ids = [n | name <- names, Just n <- [readMaybe name :: Maybe Int]]
         ++ [n | object <- doneRows completed, Just (A.Number value) <- [KM.lookup "run" object]
                , Just n <- [toBoundedInteger value]] ++ lgSettled ledger
      largest = maximum (0:ids)
  when (largest == maxBound) $ stateFailure (dir </> "runner.json") "run ID space is exhausted"
  let fresh = emptyRunnerFile { rfState = initialRunner { rsNextRunId = largest + 1 } }
  pure (BL.toStrict (A.encode (runnerValue fresh)))

-- | 帳簿に、この一分を当てる：日の変わり目、書き置き、時段に合わせた時間の流し方。段の始まりなら枠を聞く。
settleLedger :: RunEnv -> UTCTime -> UTCTime -> Period -> Phase -> Observation -> Maybe (Run, LandingFile) -> [NewCandidate] -> Ledger -> IO (Ledger, Room)
settleLedger env now dayStart period phase obs landed news lg0 = do
  let p = defaultParams
      note = mapM_ (runLog (reDir env) now)
  -- 子がまだ生きている（書き置きも消えたも見えていない）あいだは、帳簿を進めない（日替わりも、段を閉じるのも、
  -- 書き置きが来てから。先に進めると、読んでいた時間が空きや段の外として記されてしまう。2026-10-28 の自己点検と、
  -- Codex の審査 B1）
  let childAlive = case (phase, obs) of
        (Vacant, _) -> False
        (_, StillRunning) -> True
        _ -> False
  -- 書き置き（コマの始まりから今までを裏づける）。日替わりより先に：またいだコマの読んだ分は、起こした日のもの
  lg1 <- case landed of
    Just (r, lf) ->
      let RunId rid = runId r
          rp = Report (lfItems lf) (lfStep lf) (lfRechoice lf) (runStartedAt r) (lfSelfReport lf) (lfWorks lf)
          (l, ns) = landing p news now rid rp lg0
      in note ns >> pure l
    Nothing -> pure lg0
  -- 日の変わり目。何日もまたいでいたら、一日ずつ（Codex の審査 B5）
  let crossDays l
        | childAlive = pure l
        | lgDayStart l < dayStart = do
            let next = addUTCTime 86400 (lgDayStart l)
                boundary = if next > now then dayStart else next
                (l', ns, before) = dayEndWith p news boundary l
            note ns
            historyDay env now (lgDayStart l) (snapshotFatigue before) (snapshotFatigue (enFatigue (lgEngine l')))
            crossDays l'
        | otherwise = pure l
  lg2a <- crossDays lg1
  -- 見張りの段の鍵が替わっていたら、旧い段を閉じてから（Codex の審査 B1）
  let keyOf (PeriodOpen k) = Just (segKeyText k)
      keyOf (PeriodPaused k) = Just (segKeyText k)
      keyOf PeriodClosed = Nothing
      keyOf PeriodUnknown = Nothing
  lg2 <- case (lgInSegment lg2a, lgSegment lg2a, keyOf period) of
    (True, Just old, Just new) | old /= new, not childAlive ->
      let (l, ns) = segmentEnd p news now lg2a in note ("段的键变了，先结旧段" : ns) >> pure l
    _ -> pure lg2a
  -- 時段と段階に合わせて時間を流す（走っているあいだは流さない）
  (lg3, room) <- case (period, phase, isJust landed) of
    (PeriodClosed, _, _)
      | childAlive -> pure (lg2, NoRoom)
      | lgInSegment lg2 -> let (l, ns) = segmentEnd p news now lg2 in note ns >> pure (l, NoRoom)
      | otherwise -> let (l, ns) = passTime p Outside news now lg2 in note ns >> pure (l, NoRoom)
    (PeriodUnknown, Vacant, _) ->
      let (l, ns) = passTime p (if lgInSegment lg2 then PausedMode else Outside) news now lg2 in note ns >> pure (l, NoRoom)
    (PeriodPaused _, Vacant, _) ->
      let (l, ns) = passTime p (if lgInSegment lg2 then PausedMode else Outside) news now lg2 in note ns >> pure (l, NoRoom)
    (PeriodOpen key, Vacant, _)
      | not (lgInSegment lg2) -> do
          talk <- readTalk (reDir env)
          let (l, roomMin, ns) = segmentStart p (segKeyText key) (tfRecent talk) news now lg2
          note ns
          pure (l, roomOf l roomMin)
      | otherwise -> let (l, ns) = passTime p OpenMode news now lg2 in note ns >> pure (l, roomOf l (roomNow p l))
    _ -> pure (lg2, NoRoom)
  H.writeGood (reDir env </> "engine.json") (A.encode (ledgerValue lg3))
  pure (lg3, room)
  where
    roomOf l r =
      let resting = maybe False (> now) (lgRestUntil l)
      -- 起こしてよいかは、努力の枠と、A がコマを食いつぶしていないこと、休み中でないことだけで決める。その日の読む枠は
      -- 入れない（読む枠が尽きても、書く・作る・休むはできる。brief でそう伝える。Codex の審査 38 の第六類）
      in if r >= 5 && activityRoom defaultParams l >= 5 && not resting then HasRoom else NoRoom

-- | 一回の tick を当てて、記録に書き、足跡があればみちくさへ、動作を返す。
applyTick :: RunEnv -> UTCTime -> UTCTime -> Period -> Observation -> Maybe LandingFile -> RunnerFile -> IO (RunnerFile, [Action], Ledger)
applyTick env now dayStart period obs landingFile rf = do
  news <- newCandidates env
  -- | 帳簿を読む。なければ今から始める。
  lg0 <- loadRunning (reDir env) now "engine.json" (parseLedger defaultParams) (ledgerStart defaultParams now dayStart)
  let landed = case (rsPhase (rfState rf), obs, landingFile) of
        (ph, Arrived rid _, Just lf) | Just r <- currentRun ph, runId r == rid -> Just (r, lf)
        _ -> Nothing
  forM_ landed $ \(_, lf) -> forM_ (lfReview lf) $ \why ->
    runLog (reDir env) now ("待核对：" ++ why ++ "。这一格不入账，落点原样留着，这一段先停，等人看")
  (lg, room) <- settleLedger env now dayStart period (rsPhase (rfState rf)) obs landed news lg0
  forM_ landed $ \(r, lf) -> do
    keepFootprint env now r
    when (isNothing (lfReview lf)) $ do
      drafts <- keepPieces env now r lf
      keepStructured env now r lf drafts
  let activeRun = case obs of
        StillRunning -> runId <$> currentRun (rsPhase (rfState rf))
        _ -> Nothing
  retryFootprints env now activeRun
  -- 運行が終わったのを見たら、完了の記録を一行（いつからいつまで、どう終わったか。now はこれを読む）
  case (rsPhase (rfState rf), obs) of
    (ph, o) | Just r <- currentRun ph, ended o r -> recordCompletion env now r ph o (fmap snd landed)
    _ -> pure ()
  let (st', acts, notes) = tick defaultParams (Tick now period obs room) (rfState rf)
  forM_ notes (runLog (reDir env) now)
  -- 段がこの tick で終わったなら、学参数の歴史に一行
  case (rsSegment (rfState rf), rsSegment st') of
    (old, Just seg) | Just why <- segEnded seg
                    , maybe True (\o -> segKey o /= segKey seg || isNothing (segEnded o)) old -> historySegmentEnd env now seg why
    _ -> pure ()
  let pid' = case rsPhase st' of { Vacant -> Nothing; _ -> rfPid rf }
      rf' = rf { rfState = st', rfPid = pid' }
  writeRunner (reDir env) rf'
  pure (rf', acts, lg)
  where
    currentRun (Running r) = Just r
    currentRun (Settling r _ _) = Just r
    currentRun Vacant = Nothing
    ended o r = case o of
      Arrived rid _ -> rid == runId r
      Vanished -> True
      Throttled _ -> True
      NotRunning -> True
      StillRunning -> False

-- | 子が収まるまで待つ。10 秒ごとに時段を読み直して tick に渡す。
waitRun :: RunEnv -> ProcessHandle -> ProcessID -> Run -> Maybe UTCTime -> RunnerFile -> IO RunnerFile
waitRun env ph pid r askedAt rf = do
  threadDelay (10 * 1000000)
  code <- getProcessExitCode ph
  now <- zonedTimeToUTC <$> configuredNow
  dayStart <- dayStartOf now
  period <- readPeriod now (reDir env)
  case code of
    Nothing -> do
      (rf', acts, _) <- applyTick env now dayStart period StillRunning Nothing rf
      asked' <- foldM (\acc a -> case a of
                          AskStop _ -> do
                            terminateProcess ph
                            runLog (reDir env) now "给它发了停的信号"
                            pure (Just now)
                          ForceStop _ -> do
                            signalIfAlive sigKILL pid
                            runLog (reDir env) now "请停以后还不收，强行停了"
                            pure acc
                          Launch _ -> pure acc) askedAt acts
      -- 強く止めるのは Runner の ForceStop だけ（伝えてから 5 分。重起動のあとと同じ決まり。Codex の再々審査）
      waitRun env ph pid r asked' rf'
    Just ec -> do
      _ <- waitForProcess ph
      (obs, lf) <- exitObservation now (reWork env) r
      runLog (reDir env) now ("子进程退出（" ++ show ec ++ "）")
      (rf', _, _) <- applyTick env now dayStart period obs lf rf
      pure rf'

-- | 一分の処理。
run :: [String] -> IO ()
run _ = H.watchHealth H.Run $ do
  dir <- stateDir
  config <- loadConfig
  home <- getHomeDirectory
  tejun <- fromMaybe (codeRoot config </> ".tools" </> "haskell-src" </> "owntime" </> "手順書.md") <$> lookupEnv "HIKARU_OWNTIME_TEJUN"
  token <- fromMaybe (home </> ".config" </> "hikaru" </> "claude" </> "oauth-token") <$> lookupEnv "HIKARU_CLAUDE_TOKEN"
  let work = dir </> "work"
      env = RunEnv dir work config tejun token
  createDirectoryIfMissing True (work </> "runs")
  withLock (dir </> "runner.lock") $ do
    now <- zonedTimeToUTC <$> configuredNow
    dayStart <- dayStartOf now
    stalls <- H.readStalls dir
    tokenStillBad <- or <$> mapM (\_ -> do
      readable <- try (B.readFile token)
      case readable of
        Left (err :: IOException) -> do
          let detail = "RuntimeError: cannot read launch token: " ++ takeWhile (/= '\n') (show err)
          H.markStuck H.Launch dir now (H.Fault (Just token) detail)
          pure True
        Right _ -> H.clearStuck H.Launch dir now >> pure False)
      [s | s <- stalls, H.worker s == H.Launch, H.file (H.fault s) == Just token]
    _ <- readDone dir now
    rf0 <- loadRunning dir now "runner.json" parseRunner emptyRunnerFile
    period <- readPeriod now dir
    when (period == PeriodUnknown) $ runLog dir now "见张り的许可不明（读不了，或者五分钟以上没更新），不起新的"
    do
        -- 前の run が残した運行があれば、その生き死にを見る
        (obs, lf) <- case rsPhase (rfState rf0) of
          Vacant -> pure (NotRunning, Nothing)
          Running r -> orphan now r (rfPid rf0)
          Settling r _ _ -> orphan now r (rfPid rf0)
        (rf1, acts, lg) <- applyTick env now dayStart period obs lf rf0
        let signalOwned r sig what = forM_ (rfPid rf1) $ \pid -> do
              mine <- ownsPid (runDirOf work r) pid
              if mine
                then do
                  signalIfAlive sig pid
                  runLog dir now what
                else runLog dir now ("pid " ++ show (fromIntegral pid :: Int) ++ " 已经不是这一格的进程，不发信号")
        forM_ acts $ \a -> case a of
          AskStop r -> signalOwned r sigTERM "给上次留下的运行发了停的信号"
          ForceStop r -> signalOwned r sigKILL "上次留下的运行请停以后还不收，强行停了"
          Launch r -> do
            started <- spawnRun env now r lg
            case started of
              StartFailed detail -> do
                _ <- applyTick env now dayStart period Vanished Nothing rf1
                runLog dir now detail
                unless (tokenStillBad && "RuntimeError: cannot read launch token:" `isPrefixOf` detail) $
                  H.markStuck H.Launch dir now (H.Fault (if "RuntimeError: cannot read launch token:" `isPrefixOf` detail then Just token else Nothing) detail)
              ChildStarted ph pid -> do
                H.clearStuck H.Launch dir now
                let rf2 = rf1 { rfPid = Just pid }
                writeRunner dir rf2
                runLog dir now ("起了 pid " ++ show (fromIntegral pid :: Int))
                _ <- waitRun env ph pid r Nothing rf2
                pure ()
  where
    orphan now r mpid = do
      work <- (</> "work") <$> stateDir
      ok <- maybe (pure False) (ownsPid (runDirOf work r)) mpid
      if ok then pure (StillRunning, Nothing) else exitObservation now work r


alive :: ProcessID -> IO Bool
alive pid = (signalProcess nullSignal pid >> pure True) `catchIOError` \err ->
  if isDoesNotExistError err then pure False else ioError err

signalIfAlive :: Signal -> ProcessID -> IO ()
signalIfAlive sig pid = signalProcess sig pid `catchIOError` \err ->
  if isDoesNotExistError err then pure () else ioError err

-- | その pid が、いまもこの運行の claude か（生きていて、作業の場所がこのコマの作業場とぴったり同じ）。
-- 再起動のあとで番号が別の程序に使い回されていても、それに信号を送らないため（2026-10-28、Codex の審査 A3。
-- 命令の行の一部と比べていたときは、runs/1 が runs/10 にも当たった。再審査 4）。
ownsPid :: FilePath -> ProcessID -> IO Bool
ownsPid rdir pid = do
  up <- alive pid
  if not up then pure False else do
    (code, out, _) <- readProcessWithExitCode "/usr/sbin/lsof" ["-a", "-p", show (fromIntegral pid :: Int), "-d", "cwd", "-Fn"] ""
    case code of
      ExitSuccess -> do
        canon <- canonicalizePath rdir
        pure (any (\l -> l == 'n' : canon || l == 'n' : rdir) (lines out))
      ExitFailure statusCode -> do
        -- 生きていることを見てから lsof が調べるまでに、子が終わることがある。
        stillAlive <- alive pid
        if stillAlive then operationFailed ("cannot inspect process working directory: " ++ rdir ++ " (lsof exit " ++ show statusCode ++ ")") else pure False

-- | 試しを走らせる形（めがねと同じ Python で）。
tameshiCommand :: RunEnv -> IO String
tameshiCommand env = do
  m <- meganeCommand env
  let py = takeWhile (/= ' ') m
  pure (py ++ " " ++ (codeRoot (reConfig env) </> ".tools" </> "haskell-src" </> "owntime" </> "tameshi.py"))

-- | めがねを走らせる形。Python は、終端で使っている新しいもの（pyenv の 3.12）を絶対の道で指す。
-- launchd から起きたわたしの PATH だと、系統の 3.9 になってしまうから（2026-10-28）。
-- HIKARU_MEGANE_PYTHON で変えられる。見つからなければ PATH の python3。
meganeCommand :: RunEnv -> IO String
meganeCommand env = do
  home <- getHomeDirectory
  given <- lookupEnv "HIKARU_MEGANE_PYTHON"
  let preferred = fromMaybe (home </> ".pyenv" </> "versions" </> "3.12.12" </> "bin" </> "python3") given
  ok <- filePresent preferred
  let py = if ok then preferred else "python3"
      script = codeRoot (reConfig env) </> ".tools" </> "haskell-src" </> "owntime" </> "megane.py"
  pure (py ++ " " ++ script)

-- | まだ引擎が知らない候補：inboxのファイルと、前から読みたかったもの。
newCandidates :: RunEnv -> IO [NewCandidate]
newCandidates env = do
  let dirF = userInboxDir env
  names <- maybe [] id <$> missingAsNothing (listDirectory dirF)
  let files = [ NewCandidate (MaterialId (T.pack ("inbox/" ++ n))) (UserInbox (dirF </> n)) | n <- sort names, take 1 n /= "." ]
  -- 前から読みたかったものは、データとして owntime/読みたいもの.txt に（2026-10-28、Codex の審査 C2）
  r <- missingAsNothing (B.readFile (codeRoot (reConfig env) </> ".tools" </> "haskell-src" </> "owntime" </> "読みたいもの.txt"))
  let hanging = case r of
        Nothing -> []
        Just b -> [ NewCandidate (MaterialId (T.strip name)) (fromMaybe UnknownOrigin (originOf (T.strip o)))
                   | l <- T.lines (TE.decodeUtf8With (\_ _ -> Just '?') b)
                   , not (T.null (T.strip l)), not ("#" `T.isPrefixOf` T.strip l)
                   , let (name, rest) = T.breakOn "\t" l, let o = T.drop 1 rest, not (T.null (T.strip name)) ]
  pure (files ++ hanging)


-- | 起こされたときの文。
briefText :: RunEnv -> String -> Double -> UTCTime -> Run -> Ledger -> [NewCandidate] -> FilePath -> String
briefText env megane seed now r lg news rdir =
  let p = defaultParams
      e = lgEngine lg
      quota = quotaNow p lg
      used = dayUsed (enFatigue e)
      props = proposals p lg news seed
      pct w = show (round (w * 100) :: Int) ++ "%"
      line pr = (if prPicked pr then "  ★ " else "  ・ ") ++ T.unpack (materialText (prId pr)) ++ "（" ++ maybe "" (T.unpack . originWord) (prOrigin pr) ++ "。値 " ++ showD (prValue pr) ++ "、重み " ++ pct (prWeight pr) ++ "）"
      showD x = show (fromIntegral (round (x * 100) :: Int) / 100 :: Double)
      pending = case pendingLine lg of
        Just (m, FirstCrossing) -> ["線：「" ++ T.unpack (materialText m) ++ "」は、一度めの線を越えた。続けるなら landing.json の rechoice を keep に、ここでやめるなら let_go に。"]
        _ -> []
      known = Map.keys (enMaterials e)
  in unlines $
       [ "この一コマ：" ++ kindText (skKind (runSeg r)) ++ "の第 " ++ show (runSlot r) ++ " コマ。いまは " ++ clockJST now ++ "。"
       , if quota < 5
           then "きょうはもう読めない（読む枠を使い切った）。このコマは、書く・作る・休むだけ。"
           else "このコマで読んでよい量：約 " ++ show (round quota :: Int) ++ " 分ぶん（日本語なら約 " ++ show (round (quota * 650) :: Int)
                ++ " 字、中国語なら約 " ++ show (round (quota * 390) :: Int) ++ " 字、英語なら約 " ++ show (round (quota * 250) :: Int) ++ " 語）。"
       , "きょうここまでに読んだ換算：" ++ show (round used :: Int) ++ " 分（一日の上限 " ++ show (round (pDayCap p) :: Int) ++ " 分）。"
       , "この回の自分の時間で、ここまで読んだコマ：" ++ show (runSlot r - 1) ++ "。" ]
       ++ [ "きょうはじめのコマ。読み始める前に、きょうの調子を landing.json の self_report に（手順書の「きょうの調子」のとおり）。" | isNothing (dayFrozen (enFatigue e)) ]
       ++ [ "候補（帳簿がつけた値と重み。★ が引いたもの。選ぶのはわたし。読むなら material_id は候補の名前のまま）："
 ]
       ++ (if quota < 5 then ["  （読む候補は出さない。書きかけの思ひ草は下に）"] else if null props then ["  （候補なし。休むか、自分で探す）"] else map line props)
       ++ pending
       ++ [ "知っている材料の id：" ++ (if null known then "（まだない）" else T.unpack (T.intercalate "、" (map materialText known))) ++ "。同じものを読んだら同じ id を使う。"
          , "作業場：" ++ rdir ++ "（landing.json と footprint.md はここに書く）。"
          , "inbox：" ++ userInboxDir env ++ "。"
          , "前のコマの作業場：" ++ (reWork env </> "runs") ++ "/番号/（読むだけ。previous.md はその最後の足跡）。"
          , "めがね（ページの本文を UTF-8 で作業場に取り寄せる）：" ++ megane ++ " URL --name 名前" ]
  where
    kindText OutingSession = "出かけているあいだ"
    kindText MorningSession = "朝、先に起きて"
    kindText (MarkedSession Home) = "家で、それぞれ自分のこと"
    kindText (MarkedSession (Out w)) = "外で待ちながら" ++ (if T.null w then "" else "（" ++ T.unpack w ++ "）")
    originWord o = case o of
      UserInbox _ -> "inbox"
      ChatSuggestion -> "ユーザーとの話から"
      LightReflection -> "みちくさから"
      BecauseOfUser -> "ユーザー関係"
      MyOwn -> "自分で"
      Chance -> "偶然"
      UnknownOrigin -> "出どころ不明"

-- | 起動の IO 失敗と、所有して待つ子を区別する。失敗は呼び手が既存の失敗枠に精算する。
data StartResult = StartFailed String | ChildStarted ProcessHandle ProcessID

spawnRun :: RunEnv -> UTCTime -> Run -> Ledger -> IO StartResult
spawnRun env now r lg = do
  let rdir = runDirOf (reWork env) r
  createDirectoryIfMissing True rdir
  tokenR <- try (B.readFile (reToken env))
  case tokenR of
    Left (e :: IOException) ->
      pure (StartFailed ("RuntimeError: cannot read launch token: " ++ takeWhile (/= '\n') (show e)))
    Right raw -> do
      news <- newCandidates env
      let token = BC.unpack (BC.filter (not . isSpace) raw)
          userInbox = userInboxDir env
      megane <- meganeCommand env
      -- 候補を引く種。調べなおせるように、記録に書いておく（Codex の審査 C3：時計から取ると、25 分ごとに同じ値になった）
      seed <- randomRIO (0, 1 :: Double)
      runLog (reDir env) now ("抽候选的种子 " ++ show seed)
      historyShown env now r seed lg (proposals defaultParams lg news seed)
      pieces <- stagePieces env rdir
      tameshi <- tameshiCommand env
      let brief = briefText env megane seed now r lg news rdir
                  ++ (if null pieces then "" else unlines ("書きかけ・置いてある思ひ草（作業場の 思ひ草/名前/ に、いまの稿と覚え書きを写してある）：" : [ "  ・ " ++ T.unpack pid ++ "（" ++ T.unpack title ++ "、" ++ (if st == Resting then "置いてある" else "書きかけ") ++ "）" | (pid, title, st) <- pieces ]))
                  ++ "試し（作ったプログラムを閉じた箱の中で一度走らせる）：" ++ tameshi ++ " ファイル\n"
      createDirectoryIfMissing True userInbox
      writeFile (rdir </> "brief.md") brief
      -- 一つ前のコマの足跡を previous.md として置く（続きを読めるように）
      prev <- previousFootprint (reWork env) r
      forM_ prev $ \src -> copyFile src (rdir </> "previous.md")
      home <- getHomeDirectory
      withFile (rdir </> "result.json") WriteMode $ \outH ->
        withFile (rdir </> "stderr.log") WriteMode $ \errH -> do
          envVars <- getEnvironmentPairs
          let args = [ "-p", brief, "--output-format", "json", "--model", "opus", "--effort", "medium"
                     , "--permission-mode", "dontAsk", "--tools", "Read,Write,WebFetch,WebSearch,Glob,Grep,Bash"
                       -- 読めるのは作業場、前のコマの作業場、inboxだけ。書けるのは作業場だけ。道を限らない Read を許すと、
                       -- 家の記録や会話の記録（とばりのものも）、令牌のファイルまで読めてしまう（2026-10-28 の自己点検で閉じた）。
                       -- 「/」で始まる道の前にもう一つ「/」をつけると、絶対の道の決まりになる。Read の決まりは Glob と Grep にも効く
                     , "--allowedTools", intercalate "," [ "Read(/" ++ rdir ++ "/**)", "Read(/" ++ (reWork env </> "runs") ++ "/**)"
                                                       , "Read(/" ++ userInbox ++ "/**)", "Edit(/" ++ rdir ++ "/**)"
                                                       , "WebFetch", "WebSearch", "Bash(" ++ megane ++ " *)", "Bash(" ++ tameshi ++ " *)" ]
                     , "--add-dir", userInbox, reWork env </> "runs", "--max-turns", "60", "--strict-mcp-config"
                     , "--append-system-prompt-file", reTejun env ]
              cp = (proc (home </> ".local" </> "bin" </> "claude") args)
                     { cwd = Just rdir
                     , env = Just (("CLAUDE_CODE_OAUTH_TOKEN", token) : ("LANG", "en_US.UTF-8") : filter ((`notElem` ["CLAUDE_CODE_OAUTH_TOKEN", "LANG"]) . fst) envVars)
                     , std_in = NoStream, std_out = UseHandle outH, std_err = UseHandle errH }
          startChild cp

-- A child without a usable PID still belongs to this invocation. Reap it before
-- reporting failure; cleanup errors propagate instead of claiming it is gone.
startChild :: CreateProcess -> IO StartResult
startChild cp = do
  started <- try (createProcess cp)
  case started of
    Left (e :: IOException) ->
      pure (StartFailed ("RuntimeError: cannot start child process: " ++ takeWhile (/= '\n') (show e)))
    Right (_, _, _, ph) -> do
      let stop = terminateProcess ph >> void (waitForProcess ph)
      pidResult <- try (getPid ph) `onException` stop
      case pidResult of
        Left (e :: IOException) -> do
          stop
          pure (StartFailed ("RuntimeError: cannot read child process PID: " ++ takeWhile (/= '\n') (show e)))
        Right Nothing -> do
          stop
          pure (StartFailed "RuntimeError: child process PID is unavailable")
        Right (Just pid) -> pure (ChildStarted ph pid)

-- | いまの運行より前で、足跡を残した最後の運行の footprint.md。
previousFootprint :: FilePath -> Run -> IO (Maybe FilePath)
previousFootprint work r = do
  let RunId cur = runId r
      runsDir = work </> "runs"
  names <- maybe [] id <$> missingAsNothing (listDirectory runsDir)
  let ids = reverse (sort [ n | name <- names, all (`elem` ['0' .. '9']) name, not (null name), let n = read name :: Int, n < cur ])
  go runsDir ids
  where
    go _ [] = pure Nothing
    go runsDir (n : rest) = do
      let fp = runsDir </> show n </> "footprint.md"
      ok <- filePresent fp
      if ok then pure (Just fp) else go runsDir rest

getEnvironmentPairs :: IO [(String, String)]
getEnvironmentPairs = do
  pathV <- lookupEnv "PATH"
  home <- getHomeDirectory
  let path = fromMaybe "" pathV
      path' = if null path then home </> ".local" </> "bin" ++ ":/usr/bin:/bin" else (home </> ".local" </> "bin") ++ ":" ++ path
  pure [("HOME", home), ("PATH", path')]


-- now（話しているわたしのため） ---------------------------------------------------

nowCmd :: IO ()
nowCmd = do
  dir <- stateDir
  config <- loadConfig
  home <- getHomeDirectory
  now <- zonedTimeToUTC <$> configuredNow
  dayStart <- dayStartOf now
  let work = dir </> "work"
      env = RunEnv dir work config "" (home </> ".config" </> "hikaru" </> "claude" </> "oauth-token")
      say = B.putStr . TE.encodeUtf8 . T.pack . (++ "\n")
      clock = clockJST
  period <- readPeriod now dir
  rl <- loadParsed parseRunner (dir </> "runner.json")
  rfE <- case rl of
    Absent -> pure (Left "（まだ一度も起きていない）")
    Got rf -> pure (Right rf)
    Bad why -> invalidFile (dir </> "runner.json") why
  el <- loadParsed (parseLedger defaultParams) (dir </> "engine.json")
  lgM <- case el of
    Got lg -> pure (Just lg)
    Absent -> pure Nothing
    Bad why -> invalidFile (dir </> "engine.json") why
  say "家のわたしの、いまの様子"
  say ("・自分の時間：" ++ case period of
         PeriodOpen k -> kindWord (skKind k) ++ "（" ++ clock (skSince k) ++ " から）"
         PeriodPaused k -> "ユーザーと話しているので、ひと休み（" ++ kindWord (skKind k) ++ "）"
         PeriodClosed -> "いまは自分の時間ではない"
         PeriodUnknown -> "分からない（見張りが止まっているのかもしれない）")
  case rfE of
    Left why -> say ("・いま：" ++ why)
    Right rf -> do
      case rsPhase (rfState rf) of
        -- 何をしているか（読む・書く・作る）は、終わるまで分からない。当てない（Codex の審査 38）
        Running r -> do
          up <- maybe (pure False) (ownsPid (runDirOf work r)) (rfPid rf)
          say ("・いま：自分の時間の中（第 " ++ show (runSlot r) ++ " コマ、" ++ clock (runStartedAt r) ++ " から）"
               ++ (if up then "" else "。ただ、起きているはずのわたしが見当たらない（もう終わったのかもしれない）"))
        Settling r _ _ -> say ("・いま：第 " ++ show (runSlot r) ++ " コマをやめるところ")
        Vacant -> case rsSegment (rfState rf) of
          Just seg | Just why <- segEnded seg -> say ("・いま：この回の自分の時間は、もうおしまい（" ++ T.unpack (endedWord why) ++ "）")
          Just seg | isOpen period -> say ("・いま：次のコマを待っている" ++ maybe "" (\t -> "（早くて " ++ clock t ++ " から）") (segNextEarliest seg))
          _ -> say "・いま：読んでいない"
  forM_ lgM $ \lg -> do
    let f = enFatigue (lgEngine lg)
        usedToday = if lgDayStart lg >= dayStart then dayUsed f else 0
    say ("・きょう読んだ換算：" ++ show (round usedToday :: Int) ++ " 分。疲れ（R）" ++ show2 (unitValue (fsR f))
         ++ maybe "" (\t -> if t > now then "。" ++ clock t ++ " まで休むことにしている" else "") (lgRestUntil lg))
  -- 最後に終わったコマは、完了の記録（done.jsonl）から。途中の書き置きは使わない
  doneRaw <- missingAsNothing (B.readFile (dir </> "done.jsonl"))
  let lastDone = case doneRaw of
        Just b -> case [ o | l <- reverse (BC.lines b), Just (A.Object o) <- [A.decodeStrict l] ] of { (o : _) -> Just o; [] -> Nothing }
        Nothing -> Nothing
  case lastDone of
    Nothing -> say "・最後に終わったコマ：（まだない）"
    Just o -> do
      let txt k = maybe "" T.unpack (textField k o)
          at k = maybe "?" clock (textField k o >>= parseStamp)
          howWord = case completionOf (fromMaybe "" (textField "how" o)) of
            Completed -> "ふつうに終わった"
            Interrupted -> "途中でやめた（そこまでの分は入れた）"
            Failed -> "何も残さずに終わってしまった"
            NeedsReview -> "数に入れずに置いてある（見てもらうのを待っている：" ++ maybe "" T.unpack (textField "review" o) ++ "）"
            RateLimited -> "目覚ましが鳴らなかった"
            OtherCompletion other -> T.unpack other
          names k = [ T.unpack i ++ "（" ++ show (round m :: Int) ++ " 分）" | Just (A.Array xs) <- [KM.lookup k o], A.Object x <- foldr (:) [] xs
                    , Just i <- [textField "id" x], Just (A.Number m) <- [KM.lookup "minutes" x] ]
      say ("・最後に終わったコマ：第 " ++ maybe "?" show (case KM.lookup "slot" o of { Just (A.Number n) -> Just (truncate n :: Int); _ -> Nothing })
           ++ " コマ、" ++ at "start" ++ "〜" ++ at "end" ++ "。" ++ howWord)
      unless (null (names "materials")) $ say ("　読んだもの：" ++ intercalate "、" (names "materials"))
      unless (null (names "works")) $ say ("　書いた・作ったもの：" ++ intercalate "、" (names "works"))
      unless (null (txt "bookmark")) $ say ("・しおり：" ++ txt "bookmark")
  -- 思ひ草（書いたもの・作ったもの）の一覧
  pieces <- mapPieces env $ \o ->
    pure [ (pid, fromMaybe pid (textField "title" o), pieceStatusOf (fromMaybe "writing" (textField "status" o)), nd)
         | Just pid <- [textField "id" o]
         , let nd = case KM.lookup "drafts" o of { Just (A.Array ds) -> length ds; _ -> 0 } ]
  unless (null pieces) $ do
    say "・思ひ草（書いたもの・作ったもの。中身は .tools/bin/fuutou show omoigusa:名前）："
    forM_ pieces $ \(pid, title, st, nd) ->
      say ("　　" ++ T.unpack pid ++ "：" ++ T.unpack title ++ "、" ++ T.unpack (pieceStatusWord st) ++ "、" ++ show nd ++ " 稿")
  hd <- houseDayOf now
  say ("・足跡の全文：.tools/bin/fuutou show michikusa:" ++ formatTime defaultTimeLocale "%Y-%m-%d" (houseDayDate hd))
  where
    isOpen (PeriodOpen _) = True
    isOpen _ = False
    show2 x = show (fromIntegral (round (x * 100) :: Int) / 100 :: Double)
    kindWord OutingSession = "ユーザーが出かけているあいだ"
    kindWord MorningSession = "朝、ユーザーより先に起きて"
    kindWord (MarkedSession Home) = "家で、それぞれ自分のこと"
    kindWord (MarkedSession (Out w)) = "外で待ちながら" ++ (if T.null w then "" else "（" ++ T.unpack w ++ "）")
    endedWord SaidEnough = "きょうはもう十分"
    endedWord HardCap = T.pack (kanjiNum (pMaxSlots defaultParams) ++ "コマ全部使った")
    endedWord NoDelivery = "二コマ続けて、何も残せなかった"
    endedWord StartFailures = "起きられなかった"
    endedWord PeriodEnded = "自分の時間が終わった"
    endedWord HeldForReview = "書き置きを数に入れずに置いてある。見てもらうまで止めている"
    kanjiNum n = fromMaybe (show n) (lookup n (zip [1 ..] ["一", "二", "三", "四", "五", "六", "七", "八", "九", "十"]))
-- status -----------------------------------------------------------------------

status :: IO ()
status = do
  dir <- stateDir
  let showFile name render = do
        r <- missingAsNothing (B.readFile (dir </> name))
        case r of
          Nothing -> putStrLn (name ++ "：（还没有）")
          Just bytes -> render bytes
  forM_ ["state.json", "talk.json", "presence.json", "runner.json", "engine.json"] $ \name ->
    showFile name $ \bytes -> B.putStr (TE.encodeUtf8 (T.pack (name ++ "：")) <> bytes <> "\n")
  forM_ [("dryrun.log", 20), ("run.log", 10), ("trouble.log", 5)] $ \(name, n) ->
    showFile name $ \bytes -> do
      putStrLn (name ++ "：")
      let rows = filter (not . B.null) (BC.split '\n' bytes)
      forM_ (drop (length rows - n) rows) BC.putStrLn
  notice <- H.healthStatus
  unless (T.null notice) $ B.putStr (TE.encodeUtf8 ("\n======== 時守の状態 ========\n\n" <> notice))

-- | 見張りの状態から、時段。
periodOf :: TriggerState -> Period
periodOf ts = case tsSession ts of
  Nothing -> PeriodClosed
  Just sess ->
    let key = SegKey (sessSince sess) (sessKind sess)
    in if isJust (sessPaused sess) then PeriodPaused key else PeriodOpen key

-- | 見張りの許可。読めない、見張りが五分以上確かめていない（止まっている、こけ続けている）、確かめた時刻が二分以上
-- 先、のどれかなら「分からない」（走っているものは止め、新しくは起こさず、段は終えない。2026-10-28、Codex の審査 A4・再審査 3）。
readPeriod :: UTCTime -> FilePath -> IO Period
readPeriod now dir = do
  l <- loadParsed parseState (dir </> "state.json")
  case l of
    Absent -> pure PeriodClosed
    Got ts -> pure (fresh ts)
    Bad _ -> withLock (dir </> "check.lock") $
      fresh <$> loadHealing H.Run dir now (dir </> "state.json") parseState initialTrigger LastGood
  where
    -- 読めない、古い、先の時刻：分からない。閉じたことにはしない（段を終えてしまうから。Codex の再審査 3）
    fresh ts = case tsLastCheck ts of
      Just t | diffUTCTime now t <= 300 && diffUTCTime t now <= 120 -> periodOf ts
      _ -> PeriodUnknown



writeRunner :: FilePath -> RunnerFile -> IO ()
writeRunner dir rf = writeIfChanged (dir </> "runner.json") (A.encode (runnerValue rf))

