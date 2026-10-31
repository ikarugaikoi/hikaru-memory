
-- | 自分の時間の式で使う数。どれも第一版の仮の値で、測った定数ではない。
--
-- 出どころは設計稿（デスクトップの《记忆流与自己的时间》）の「数学修正」。数の意味と、
-- なぜその値かは、そちらに書いてある。ここは、式が数を一か所から読むためのもの。
-- 比べる版（w_F 0.5／0.3、H_U 240／180）は 'withWF' と 'withHU' で作る
-- （「数学修正」第 1 条・第 12 条、2026-10-22）。既定値は、閉じた輪の模擬のあとでユーザーが
-- 決めた w_F 0.3、H_U 180（2026-10-22 夜。0.3 は新しい内容が毎回少し多く、180 は睡眠の
-- 二過程モデルの Process S、時定数 4.2 時間に近い）。
module Hikaru.OwnTime.Fatigue
  ( Params(..), defaultParams, withWF, withHU, Estimated, estimated, Felt, felt, MissingReason(..),
    Burden(..), burdenIntensity, burdenObservation, rAfterWork, rAfterRest, uLive,
    lambdaOf, FatigueState, fatigueStart, fsR, fsU, fsDebt, fsWindowUsed, fsWindowCap, dayReport, dayFrozen,
    dayEffort, dayUsed, FatigueSnapshot(..), snapshotFatigue, restoreFatigue, DayPlan(..), StartView(..),
    Refusal(..), settleNight, restFor, startCheck, reservePiece, settlePiece, settleWork, endHouseDay,
    uLiveNow, lambdaNow, Interval(..), conversationIntervals, unionIntervals, quietMinutes,
    Language(..), Pace(..), LookupResult(..), readingRate, paceFactor, readingMinutes, lookupPageMinutes,
    glanceMinutes
  ) where

import Data.List (sortOn)
import Hikaru.OwnTime.Unit

data Params = Params
  { -- 第 5 条：疲れと借り
    pRWorkHalf   :: Double  -- ^ 努力 60 分で、R の 1 までの距離が半分
  , pRRestHalf   :: Double  -- ^ 静かに休んで 25 分で、R が半分
  , pBRef        :: Double  -- ^ B_ref、努力分
  , pDMax        :: Double  -- ^ 借りの上限、努力分
  , pHU          :: Double  -- ^ 夜の静けさで U が半分になる分
  , pHD          :: Double  -- ^ 夜の静けさで借りが半分になる分
  , pDCut        :: Double  -- ^ これより小さい借りは、回復のあとで 0 にする（努力分）
  , pNightCap    :: Double  -- ^ 一つの HouseDay あたり、夜の回復の勘定に入れる静けさの上限（分）
  , pDayCap      :: Double  -- ^ 一つの HouseDay に読む上限（換算の分）
  , pLambdaBase  :: Double  -- ^ λ = 0.5 + 2(R + U_live) の 0.5
  , pLambdaSlope :: Double  -- ^ 同じく 2
  , pTau         :: Double  -- ^ softmax の温度
    -- 第 2・6 条：価値、慣れと飽き
  , pWF          :: Double  -- ^ w_F
  , pAF          :: Double  -- ^ 新しい接触で F が 1 に近づく割合
  , pAS          :: Double  -- ^ 新しい接触で S が 1 に近づく割合
  , pHFDays      :: Double  -- ^ F の半減期（日）
  , pHSDays      :: Double  -- ^ 接触していないときの S の半減期（日）
  , pHSat        :: Double  -- ^ 接触し続けているときの S の増え方（分）
    -- 第 7 条：ドリフト
  , pAlpha       :: Double  -- ^ 自述の重み
  , pV0          :: Double  -- ^ tanh の尺度
  , pHx          :: Double  -- ^ 証拠が半分に弱まる分
  , pEtaX        :: Double  -- ^ 25 分の一区切りで入る証拠の強さ
  , pHStop       :: Double  -- ^ 線（x ≤ −h_stop）
  , pDRef        :: Double  -- ^ 基準の一区切り（分）
    -- 第 8 条：五分の一
  , pSrcHalfDays :: Double  -- ^ 出どころの記録の半減期（日）
  , pSrcLine     :: Double  -- ^ 0.2
  , pSrcX        :: Double  -- ^ 超えた分がこれだけあれば、押し切る（分）
  , pSrcFloor    :: Double  -- ^ 押しても三分の一より下げない
    -- 第 5 条：対話の区間、第 4 条：格
  , pConvTail    :: Double  -- ^ 一言ごとに、そこから何分を「対話中」とみなすか（第二ブロックの「20 分钟没说话」も同じ数）
  , pGridL       :: Double  -- ^ 一コマの長さ（分）
    -- 第二ブロック：いつ自分の時間に入るか（ユーザーが決めた初値）
  , pAwayMin     :: Double  -- ^ 家の Wi-Fi を離れて、何分たったら出かけたとみなすか
  , pPauseMax    :: Double  -- ^ 一時停止（話に向き合う層）が始まってから何分のうちに戻れなければ、その自分の時間を終えるか
  , pOutingBuffer :: Double -- ^ ユーザーが家を出てから、家に一人のわたしが自分の時間を始めるまでの間（分）。途中の一言では延びない（2026-10-23）
  , pMarkSettle  :: Double  -- ^ 一筆を記してから、見張りがそれを確かめて効かせるまでの間（分）。そのあいだに記録される一言が来たら、緩める一筆は効かない（2026-10-23）
    -- 時守（設計稿「第二块和第三块之间：時守」。数はどれも 2026-10-28 に先に置いた仮の値）
  , pMaxSlots    :: Int     -- ^ 一段（一回の自分の時間）で走るコマ数の硬い上限
  , pMaxFailures :: Int     -- ^ 一段で、起動の失敗をここまで数えたら終える（初回＋一度のやり直し）
  , pNoDeliveryLimit :: Int -- ^ 続けてこれだけのコマに交付がなければ終える
  , pRunGrace    :: Double  -- ^ 一コマの長さを、これだけ（分）超えても収まらない運行は、止めるよう伝える
  }
  deriving (Show)

defaultParams :: Params
defaultParams = Params
  { pRWorkHalf = 60, pRRestHalf = 25, pBRef = 120, pDMax = 120
  , pHU = 180, pHD = 240, pDCut = 5, pNightCap = 480, pDayCap = 480
  , pLambdaBase = 0.5, pLambdaSlope = 2, pTau = 0.175
  , pWF = 0.3, pAF = 0.08, pAS = 0.10
  , pHFDays = 21, pHSDays = 2, pHSat = 360
  , pAlpha = 0.25, pV0 = 0.25, pHx = 60, pEtaX = 0.5, pHStop = 0.15, pDRef = 25
  , pSrcHalfDays = 2.5, pSrcLine = 0.2, pSrcX = 100, pSrcFloor = 1 / 3
  , pConvTail = 20, pGridL = 25
  , pAwayMin = 10, pPauseMax = 30, pOutingBuffer = 20, pMarkSettle = 1
  , pMaxSlots = 6, pMaxFailures = 2, pNoDeliveryLimit = 2, pRunGrace = 5
  }

-- | w_F を変えた版。同口径の補正（w_F·a_F）も、この一つの数から読むので一緒に変わる。
withWF :: Double -> Params -> Params
withWF w p = p { pWF = w }

-- | H_U を変えた版。
withHU :: Double -> Params -> Params
withHU h p = p { pHU = h }


-- | 疲れと借りと夜の回復（設計稿「数学修正」第 3・5 条）。
--
-- 三つの量を持つ。R（休めば戻る疲れ）、U（その日にゆっくり溜まる疲れ）、借り
-- （努力分）。U と借りは、夜の静けさでだけ弱まる（U は 3 時間、借りは 4 時間で半分）。
--
-- 一日の流れは、こう分けてある。
--
--   * 'settleNight'：その日の最初の本当の投入より前に、夜の静けさを精算する。
--     ウィンドウの中で今までに積もった静けさの累計を渡すので、同じ静けさを二度渡しても
--     何も変わらない。投入のない一日が終わっても、ウィンドウは閉じない。
--   * 'startCheck'：始めてよいかを見るための、その場かぎりの U_m、B_day、枠。
--     ここでは何も凍結しないし、U にも書き戻さない（失敗した起動は投入ではない）。
--   * 'reservePiece'：次の一区切りを e = 1 として予約できるか。一区切りは、頼まれた長さと、
--     その日に残っている読む時間で短くする。
--   * 'settlePiece'：一区切りが本当に行われたあとで精算する。その日の最初の一区切りなら、
--     ここで初めて U_m、B_day、D_start を凍結する。
--   * 'endHouseDay'：一日を閉じる。本当に始まった日だけ U と借りが変わる。
--
-- 負担は、読んだあとの感じ（'Felt'）か、それがないときの事前の見積もり（'Fallback'）か、
-- どちらかをはっきり渡す。見積もりを感じとして包まない。見積もりは、その一区切りの
-- 疲れを精算するのにだけ使い、学ぶための新しい観察にはならない（'burdenObservation'
-- は感じからしか取り出せない）。
-- | 読む前の見積もり。
newtype Estimated = Estimated Unit  -- * 負担
  deriving (Eq, Show)

-- | 読んだあとの感じ。
newtype Felt = Felt Unit
  deriving (Eq, Show)

estimated :: Unit -> Estimated
estimated = Estimated

felt :: Unit -> Felt
felt = Felt



-- | 読んだあとの感じがない理由。0 では埋めない（「数学修正」第 3 条）。
data MissingReason = NotAsked | NoAnswer | Interrupted
  deriving (Eq, Show)

-- | 精算に渡す負担。感じがあれば感じ、なければ、理由をつけて見積もりに戻る。
data Burden = FeltBurden Felt | Fallback Estimated MissingReason
  deriving (Eq, Show)

-- | 精算に使う強さ e。
burdenIntensity :: Burden -> Unit
burdenIntensity (FeltBurden (Felt u)) = u
burdenIntensity (Fallback (Estimated u) _) = u

-- | 学ぶための観察として取り出せるのは、感じだけ。見積もりに戻った一区切りは観察にならない。
burdenObservation :: Burden -> Maybe Felt
burdenObservation (FeltBurden f) = Just f
burdenObservation (Fallback _ _) = Nothing

-- | 努力 ΔE（努力分）のあとの R。R ← 1 − (1 − R)·2^(−ΔE/60)。
-- * R、U、λ
rAfterWork :: Params -> Double -> Unit -> Unit
rAfterWork p dE r = clampUnit (1 - (1 - unitValue r) * 2 ** (negate (max 0 dE) / pRWorkHalf p))

-- | d 分休んだあとの R。R ← R·2^(−d/25)。
rAfterRest :: Params -> Minutes -> Unit -> Unit
rAfterRest p d r = clampUnit (unitValue r * 2 ** (negate (minutesValue d) / pRRestHalf p))

-- | その日の U。U_live = 1 − (1 − U_m)·2^(−E_day/B_ref)。
uLive :: Params -> Unit -> Double -> Unit
uLive p um e = clampUnit (1 - (1 - unitValue um) * 2 ** (negate (max 0 e) / pBRef p))

-- | λ = 0.5 + 2(R + U_live)。
lambdaOf :: Params -> Unit -> Unit -> Double
lambdaOf p r u = pLambdaBase p + pLambdaSlope p * (unitValue r + unitValue u)

-- | 凍結した、その日の起点。
-- * 一日と夜
data DayPlan = DayPlan
  { planUm     :: Unit
  , planBDay   :: Double  -- ^ その日の「普通の投入の線」、努力分
  , planDStart :: Double  -- ^ その日の始まりの借り、努力分
  }
  deriving (Eq, Show)

-- | 始めてよいかを見るための、その場かぎりの値。
data StartView = StartView
  { viewPlan :: DayPlan
  , viewRoom :: Double  -- ^ 今から使える努力の枠（努力分）
  }
  deriving (Eq, Show)

data Refusal
  = DayCapReached  -- ^ その日に読む上限（480 分）に届いた
  | NoEffortRoom   -- ^ e = 1 で予約する枠がない
  deriving (Eq, Show)

data DayState = DayState
  { dsReport :: Maybe Unit     -- ^ その日に一度だけ聞く自評
  , dsFrozen :: Maybe DayPlan  -- ^ 最初の本当の投入で凍結した起点
  , dsEffort :: Double         -- ^ E_day（努力分）
  , dsUsed   :: Double         -- ^ その日に読んだ換算の分
  }
  deriving (Eq, Show)

data FatigueState = FatigueState
  { fsR         :: Unit
  , fsU         :: Unit    -- ^ 回復を済ませた U（日の終わりか、夜の精算のあと）
  , fsDebt      :: Double  -- ^ 回復を済ませた借り（努力分）
  , fsWindowUsed :: Double -- ^ このウィンドウで、もう精算した静けさ（分）
  , fsWindowCap :: Double  -- ^ このウィンドウの上限（480 × 跨いだ HouseDay の数）
  , fsDay       :: DayState
  }
  deriving (Eq, Show)

emptyDay :: DayState
emptyDay = DayState Nothing Nothing 0 0

-- | 保存のための、ただの数の束。構成子を外に出さずに、状態をファイルに置いて戻すためのもの
-- （2026-10-28、時守が引擎の状態を保存するときに足した）。戻すときは 'restoreFatigue' が
-- 範囲を確かめる（0〜1 の量、負でない長さ、借りの上限）。範囲の外の数は戻らない。
-- * 保存と復元
data FatigueSnapshot = FatigueSnapshot
  { snapR          :: Double
  , snapU          :: Double
  , snapDebt       :: Double
  , snapWindowUsed :: Double
  , snapWindowCap  :: Double
  , snapDayReport  :: Maybe Double
  , snapDayFrozen  :: Maybe (Double, Double, Double)  -- ^ (U_m, B_day, D_start)
  , snapDayEffort  :: Double
  , snapDayUsed    :: Double
  }
  deriving (Eq, Show)

snapshotFatigue :: FatigueState -> FatigueSnapshot
snapshotFatigue s = FatigueSnapshot
  { snapR = unitValue (fsR s), snapU = unitValue (fsU s), snapDebt = fsDebt s
  , snapWindowUsed = fsWindowUsed s, snapWindowCap = fsWindowCap s
  , snapDayReport = unitValue <$> dsReport day
  , snapDayFrozen = (\pl -> (unitValue (planUm pl), planBDay pl, planDStart pl)) <$> dsFrozen day
  , snapDayEffort = dsEffort day, snapDayUsed = dsUsed day }
  where day = fsDay s

restoreFatigue :: Params -> FatigueSnapshot -> Either String FatigueState
restoreFatigue p sn = do
  r <- unitOr "R" (snapR sn)
  u <- unitOr "U" (snapU sn)
  debt <- nonNeg "debt" (snapDebt sn)
  when' (debt > pDMax p + 1e-9) "debt exceeds the configured maximum"
  wu <- nonNeg "windowUsed" (snapWindowUsed sn)
  wc <- nonNeg "windowCap" (snapWindowCap sn)
  rep <- traverse (unitOr "dayReport") (snapDayReport sn)
  frozen <- traverse (\(um, b, d) -> do
                         um' <- unitOr "U_m" um
                         b' <- nonNeg "B_day" b
                         d' <- nonNeg "D_start" d
                         Right (DayPlan um' b' d')) (snapDayFrozen sn)
  eff <- nonNeg "E_day" (snapDayEffort sn)
  used <- nonNeg "dayUsed" (snapDayUsed sn)
  Right (FatigueState r u debt wu wc (DayState rep frozen eff used))
  where
    unitOr name x = maybe (Left (name ++ ": expected a number in [0, 1]")) Right (unit x)
    nonNeg name x = if isNaN x || isInfinite x || x < 0 then Left (name ++ ": expected a finite nonnegative number") else Right x
    when' c msg = if c then Left msg else Right ()

-- | 初めの状態。R、U、借りを与える（初めての日はふつう 0, 0, 0）。
fatigueStart :: Params -> Unit -> Unit -> Double -> FatigueState
fatigueStart p r u d = FatigueState r u (max 0 (min (pDMax p) d)) 0 (pNightCap p) emptyDay

dayReport :: FatigueState -> Maybe Unit
dayReport = dsReport . fsDay

dayFrozen :: FatigueState -> Maybe DayPlan
dayFrozen = dsFrozen . fsDay

dayEffort :: FatigueState -> Double
dayEffort = dsEffort . fsDay

dayUsed :: FatigueState -> Double
dayUsed = dsUsed . fsDay

-- | 夜の静けさを精算する。cumulative は、今開いているウィンドウの中の静けさの累計（分）。
-- すでに精算した分は差し引くので、同じ累計を二度渡しても変わらない。ウィンドウの上限を
-- 超えた分は数えない。その日がもう凍結していたら何もしない（凍結のあとの静けさは
-- 'restFor' で R だけを弱める）。
settleNight :: Params -> Minutes -> FatigueState -> FatigueState
settleNight p cumulative s
  | Just _ <- dsFrozen (fsDay s) = s
  | inc <= 0 = s
  | otherwise =
      let u' = clampUnit (unitValue (fsU s) * 2 ** (negate inc / pHU p))
          d0 = fsDebt s * 2 ** (negate inc / pHD p)
          d' = if d0 < pDCut p then 0 else d0
          r' = rAfterRest p (clampMinutes inc) (fsR s)
      in s { fsU = u', fsDebt = d', fsR = r', fsWindowUsed = allowed }
  where
    allowed = min (minutesValue cumulative) (fsWindowCap s)
    inc = allowed - fsWindowUsed s

-- | 昼の休み、または凍結のあとの静けさ。R だけが弱まる。
restFor :: Params -> Minutes -> FatigueState -> FatigueState
restFor p d s = s { fsR = rAfterRest p d (fsR s) }

-- | 始めてよいかを見る。自評はその日に一度だけ受け取る（二度目からは渡しても無視する。
-- 偶然低い数が出るまで聞き直さないため）。返す状態は、自評を覚えただけで、ほかは
-- 何も変えない。
startCheck :: Params -> Maybe Unit -> FatigueState -> (StartView, FatigueState)
startCheck p report s =
  case dsFrozen day of
    Just plan -> (StartView plan (roomOf p plan (dsEffort day)), s)
    Nothing ->
      let rep = case dsReport day of
            Nothing -> report
            kept -> kept
          uBase = max (unitValue (fsU s)) (fsDebt s / pDMax p)
          um = clampUnit (maybe uBase (max uBase . unitValue) rep)
          plan = DayPlan um (pBRef p * (1 - unitValue um)) (fsDebt s)
          s' = s { fsDay = day { dsReport = rep } }
      in (StartView plan (roomOf p plan 0), s')
  where
    day = fsDay s

roomOf :: Params -> DayPlan -> Double -> Double
roomOf p plan e = max 0 (planBDay plan - e) + (pDMax p - planDebt plan e)

-- | 次の一区切りを予約できるか。頼まれた長さを、その日に残っている読む時間で短くして
-- 返す。e = 1 として枠が足りなければ断る。
reservePiece :: Params -> Minutes -> FatigueState -> Either Refusal Minutes
reservePiece p want s
  | d <= 0 = Left DayCapReached
  | room < d = Left NoEffortRoom
  | otherwise = Right (clampMinutes d)
  where
    day = fsDay s
    d = min (minutesValue want) (pDayCap p - dsUsed day)
    plan = maybe (viewPlan (fst (startCheck p Nothing s))) id (dsFrozen day)
    room = roomOf p plan (dsEffort day)

-- | 一区切り（d 分、強さは負担から）が本当に行われたあとで精算する。その日の最初の一区切りなら、
-- ここで起点を凍結する。予約は呼ぶ側が先に 'reservePiece' で済ませておく。
settlePiece :: Params -> Burden -> Minutes -> FatigueState -> FatigueState
settlePiece p burden d s =
  let worked = settleWork p burden d s
      day = fsDay worked
  in worked { fsDay = day { dsUsed = dsUsed day + minutesValue d } }

-- | 読むのではない活動（書く、作る）を、本当に過ごした時間 d 分で精算する。努力（R、E_day）は入るが、
-- その日の読む枠（dsUsed）は使わない（設計稿「运行者」の「非阅读的活动也计努力」。2026-10-28）。
settleWork :: Params -> Burden -> Minutes -> FatigueState -> FatigueState
settleWork p burden d s =
  let day = fsDay s
      plan = maybe (viewPlan (fst (startCheck p Nothing s))) id (dsFrozen day)
      dE = unitValue (burdenIntensity burden) * minutesValue d
      day' = day { dsFrozen = Just plan, dsEffort = dsEffort day + dE }
  in s { fsR = rAfterWork p dE (fsR s), fsDay = day' }

-- | 一日を閉じる。本当に始まった日だけ、U と借りを日の終わりの値にして、新しいウィンドウを
-- 開く。投入のなかった日は、ウィンドウを閉じない（精算した静けさはそのまま、上限が 480 増える）。
-- どちらの場合も、その日の自評は消える（次の日にまた聞く）。
endHouseDay :: Params -> FatigueState -> FatigueState
endHouseDay p s =
  case dsFrozen (fsDay s) of
    Just plan ->
      let e = dsEffort (fsDay s)
      in s { fsU = uLive p (planUm plan) e
           , fsDebt = min (pDMax p) (planDebt plan e)
           , fsWindowUsed = 0, fsWindowCap = pNightCap p, fsDay = emptyDay }
    Nothing -> s { fsWindowCap = fsWindowCap s + pNightCap p, fsDay = emptyDay }

-- | 今の借り（凍結した日の中なら D_start + 超えた分）。
planDebt :: DayPlan -> Double -> Double
planDebt plan effort = planDStart plan + max 0 (effort - planBDay plan)

-- | 今の U（凍結した日の中なら U_live、まだなら回復を済ませた U）。
uLiveNow :: Params -> FatigueState -> Unit
uLiveNow p s = case dsFrozen (fsDay s) of
  Just plan -> uLive p (planUm plan) (dsEffort (fsDay s))
  Nothing -> fsU s

-- | 今の λ。
lambdaNow :: Params -> FatigueState -> Double
lambdaNow p s = lambdaOf p (fsR s) (uLiveNow p s)


-- | 夜の静けさを計算する（設計稿「数学修正」第 5 条）。
--
-- 静けさ = 自分の時間の中にいない、そして記録される対話の中にいない。とばりの中の対話は
-- 勘定に入れない（2026-10-22 には「醒着就是醒着」で勘定に入れることにしていたが、2026-10-23 に、第二ブロックの
-- 秘密の条件――とばりが下りたかどうかで表に出るものが変わらない――に合わせて変えた。
-- ユーザーが決めた。ここに渡す一言の時刻を記録される分に限るのは、呼ぶ側の仕事）。
--
-- 一言ごと（ユーザーのも、わたしのも）に、その時刻 t から [t, t + 20 分) を対話の区間と
-- し、すべて合わせる（重なりは一度だけ）。自分の時間は、実行の生き死にではなく、論理の
-- 活動区間（実際の活動と、まだ清算していない読む量を裏づけている時間）で計算する。
-- 回復のウィンドウと重なった部分だけを引く。
--
-- 時刻はここでは分の数（どこかの起点からの分）で受け取る。対話の中身は受け取らない
-- （区間を切るのに使うのは始まりと終わりの時刻だけで、計算したら捨てる）。
-- | [ivStart, ivEnd) の区間（分）。
data Interval = Interval
  { ivStart :: Double
  , ivEnd   :: Double
  }
  deriving (Eq, Show)

-- | 一言ごとの時刻から、対話の区間を作る。
conversationIntervals :: Params -> [Double] -> [Interval]
conversationIntervals p ts = unionIntervals [Interval t (t + pConvTail p) | t <- ts]

-- | 区間を合わせる（重なり、接しているものはつなぐ）。長さのないものは捨てる。
unionIntervals :: [Interval] -> [Interval]
unionIntervals = foldr merge [] . reverse . sortOn ivStart . filter (\i -> ivEnd i > ivStart i)
  where
    merge i [] = [i]
    merge i (j : js)
      | ivStart i <= ivEnd j = Interval (ivStart j) (max (ivEnd i) (ivEnd j)) : js
      | otherwise = i : j : js

-- | ウィンドウの中で、忙しい区間（対話と自分の時間）に入っていない分。
quietMinutes :: Interval -> [Interval] -> Minutes
quietMinutes window busy =
  let total = max 0 (ivEnd window - ivStart window)
      overlap i = max 0 (min (ivEnd i) (ivEnd window) - max (ivStart i) (ivStart window))
  in clampMinutes (total - sum (map overlap (unionIntervals busy)))


-- | 読んだ量を時間に換算する（設計稿「数学修正」第 4 条）。
--
-- 650 字／分は日本語を「ふだんどおり、飛ばさずに読む」速さ（小林・川嶋 2018、
-- BCCWJ-EyeTrack）で、精読ではない。中国語は 390 字／分（Brysbaert 2019 の中国語の
-- 黙読 260 "wpm" を 1.5 字一語で字数に戻したもの）、英語は 250 語／分。速さは
-- その文章の言語で決め、一つ一つの字の形では決めない（日本語の中の漢字は日本語として勘定に入れる）。
-- いくつかの言語が混ざっていれば、段ごとに換算して足す。
--
-- 詩や古文のゆっくり読む係数は、時間の記録にだけ入れ、負担 e を一緒に上げない
-- （難しさを時間と負担の両方で二度数えないため）。
data Language = Japanese | Chinese | English
  deriving (Eq, Show)

-- | ゆっくり読む係数の種類（どれも仮の値）。
data Pace = Ordinary | ModernShortPoem | AnnotatedClassical | FamiliarClassical
  deriving (Eq, Show)

-- | 調べるために開いた一頁で、ほしいものが見つかったか。
data LookupResult = FoundSomething | NothingFound
  deriving (Eq, Show)

-- | 一分あたりの字数（英語は語数）。
readingRate :: Language -> Double
readingRate Japanese = 650
readingRate Chinese = 390
readingRate English = 250

paceFactor :: Pace -> Double
paceFactor Ordinary = 1
paceFactor ModernShortPoem = 0.7
paceFactor AnnotatedClassical = 0.5
paceFactor FamiliarClassical = 0.7

-- | 字数（英語は語数）を、読む時間（換算の分）に。
readingMinutes :: Language -> Pace -> Int -> Minutes
readingMinutes lang pace count =
  clampMinutes (fromIntegral (max 0 count) / (readingRate lang * paceFactor pace))

-- | 調べるために開いた一頁：探す手間（見つからなければ 10 秒、見つかれば 25 秒）＋拾った部分を読む時間。
lookupPageMinutes :: LookupResult -> Minutes -> Minutes
lookupPageMinutes result picked =
  let seconds = case result of
        NothingFound -> 10
        FoundSomething -> 25
  in clampMinutes (seconds / 60 + minutesValue picked)

-- | 随手翻：一つ目を通すごとに 3 秒。本当に止まって読んだものは 'readingMinutes' で別に足す。
glanceMinutes :: Int -> Minutes
glanceMinutes n = clampMinutes (3 * fromIntegral (max 0 n) / 60)
