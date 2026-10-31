{-# LANGUAGE OverloadedStrings #-}
-- | ドリフト：ここにもう少しいるか（設計稿「数学修正」第 7 条）。
--
-- 出来事ごとに更新する、弱まっていく累積器 x。証拠 m は本当の節目で
-- 一度だけ計算し、直前の一区切りのあいだ同じ m が続いたとみなして、その一区切りの長さで
-- 正確に積分する：
--
-- > x ← clip(−1, 1, ρ(d)·x + K·(1 − ρ(d))·m)、ρ(d) = 2^(−d/H_x)、K = η_x/(1 − ρ(25))
--
-- d = 25 なら入る証拠はちょうど η_x·m で、前の書き方と同じ。m が同じで途中に何も
-- なければ、一度に進めても何回かに分けても同じになる（±1 で切っても同じ）。前の
-- 「節目ごとに η_x·m」は停まるほど早く線を越え、「min(1, Δt/25) を掛ける」は
-- 停まるのが密だと線を越えないことがあった（Codex の反例）。
--
-- 線を越えたかは、本当の節目で精算したあとの x で見る（x ≤ −h_stop）。一区切りの中で
-- m は変わらないので、x は K·m へ単調に動き、一区切りの中で線を越えたら、その区切りの終わりにも
-- 線の外にいる。
--
-- 二段式：一度目に越えたら新しい動きを止めて選び直す。「続ける」を選んだら x を 0 に
-- 戻して、そこから積分し直す。二度目に越えたら、このことを終える。「もう十分」「先に
-- 休みたい」は累積器を通らず、その場で選ぶ（'Exit'）。
module Hikaru.OwnTime.Engine
  ( Willing(..), Exit(..), Step(..), Stage(..), Drift(..), freshDrift, driftK, evidence, activeFor,
    pausedFor, LineCheck(..), checkLine, rechooseContinue, Familiarity(..), freshFamiliarity, newContact,
    duringContact, apart, satietyEquilibrium, SourceKind(..), SourceLedger(..), emptySourceLedger,
    ledgerIdle, ledgerActive, excessMinutes, sourcePressure, vRest, valueOf, Candidate(..), candidateValue,
    candidateValueAfter, dedupeCandidates, softmax, vElse, MaterialState(..), Pending(..), Activity(..),
    Engine(..), Event(..), Outcome(..), EngineRefusal(..), engineStart, step, steps, advanceTotal,
    materialAt, Ledger(..), Obs(..), ledgerStart, estimateAt, Mode(..), Report(..), ReportItem(..),
    WorkItem(..), Rechoice(..), NewCandidate(..), Proposal(..), passTime, segmentStart, landing, segmentEnd,
    dayEnd, dayEndWith, proposals, roomNow, pendingLine, quotaNow, activityRoom, candidatesNow, minutesSince
  ) where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Hikaru.OwnTime.Fatigue
import Hikaru.OwnTime.Michikusa (MaterialId(..), SegmentId, Origin(..), isHumanSpeakerRelated)
import Hikaru.OwnTime.Unit

-- | ドリフトに入る三つの気持ち。
data Willing = Continue | Revisit | Unsure
  deriving (Eq, Show)

-- | ドリフトに入らない二つ。その場でそのまま選ぶ。
data Exit = Enough | RestFirst
  deriving (Eq, Show)

-- | 一歩のあとの気持ち。
data Step = Stay Willing | Leave Exit
  deriving (Eq, Show)

data Stage = FirstRun | Rechosen
  deriving (Eq, Show)

data Drift = Drift
  { driftX     :: Double
  , driftStage :: Stage
  }
  deriving (Eq, Show)

freshDrift :: Drift
freshDrift = Drift 0 FirstRun

-- | K = η_x/(1 − 2^(−25/H_x))。今ある数から出る量で、新しいつまみではない。
driftK :: Params -> Double
driftK p = pEtaX p / (1 - 2 ** (negate (pDRef p) / pHx p))

-- | 節目での証拠 m = α·s_self + (1 − α)·tanh((V_here − V_else)/v0)。
-- 続けたい・もう一度は s_self = 1、決めかねるは 0。自述がなければ tanh の項だけ
-- （欠けたことは呼ぶ側が別に記す）。
evidence :: Params -> Maybe Willing -> Double -> Double -> Double
evidence p willing vHere alternative =
  let z = tanh ((vHere - alternative) / pV0 p)
      a = pAlpha p
  in case willing of
       Nothing -> z
       Just Unsure -> (1 - a) * z
       Just _ -> a + (1 - a) * z

-- | d 分の活動（その一区切りの証拠 m）。
activeFor :: Params -> Minutes -> Double -> Drift -> Drift
activeFor p d m dr =
  let rho = 2 ** (negate (minutesValue d) / pHx p)
      x' = rho * driftX dr + driftK p * (1 - rho) * m
  in dr { driftX = max (-1) (min 1 x') }

-- | d 分の一時停止。前の証拠が弱まるだけで、新しい証拠は入らない。
pausedFor :: Params -> Minutes -> Drift -> Drift
pausedFor p d dr = dr { driftX = driftX dr * 2 ** (negate (minutesValue d) / pHx p) }

data LineCheck = NotYet | FirstCrossing | SecondCrossing
  deriving (Eq, Show)

-- | 本当の節目で精算したあとに見る。
checkLine :: Params -> Drift -> LineCheck
checkLine p dr
  | driftX dr > negate (pHStop p) = NotYet
  | otherwise = case driftStage dr of
      FirstRun -> FirstCrossing
      Rechosen -> SecondCrossing

-- | 一度目に越えて選び直し、「続ける」を選んだ。x は 0 から積分し直し、この機会は使った。
rechooseContinue :: Drift -> Drift
rechooseContinue _ = Drift 0 Rechosen


-- | 慣れ F と飽き S（設計稿「数学修正」第 6 条）。同じ材料への本当の接触で増える。
--
-- 新しい接触で F ← F + a_F(1 − F)、S ← S + a_S(1 − S)。接触し続けているあいだ、
-- S は S_eq = a/(a + b) へ向かって増える（a = ln2/H_sat、b = ln2/(2 日)）。
-- 接触していないときは、S は 2 日で半分。F は暦の時間で 21 日で半分で、接触して
-- いるあいだも弱まる（前の検算のスクリプトはここを落としていた、Codex の指摘）。
--
-- 首版の S は一つだけで、速い層と遅い層に分けていない。25 分休んでも S は
-- 0.6% ほどしか弱まらない。
data Familiarity = Familiarity
  { famF :: Unit
  , famS :: Unit
  }
  deriving (Eq, Show)

-- | まだ一度も接触していない材料。
freshFamiliarity :: Familiarity
freshFamiliarity = Familiarity zeroUnit zeroUnit

minutesPerDay :: Double
minutesPerDay = 1440

satA, satB :: Params -> Double
satA p = log 2 / pHSat p
satB p = log 2 / (pHSDays p * minutesPerDay)

-- | 接触し続けたときに S が向かう値（既定の数では 8/9）。
satietyEquilibrium :: Params -> Double
satietyEquilibrium p = satA p / (satA p + satB p)

-- | 新しい接触の跳ね。
newContact :: Params -> Familiarity -> Familiarity
newContact p (Familiarity f s) =
  Familiarity (clampUnit (unitValue f + pAF p * (1 - unitValue f)))
              (clampUnit (unitValue s + pAS p * (1 - unitValue s)))

-- | d 分、接触し続けた。S は S_eq へ（式の解をそのまま使う）、F は暦どおり弱まる。
duringContact :: Params -> Minutes -> Familiarity -> Familiarity
duringContact p d (Familiarity f s) =
  let t = minutesValue d
      eq = satietyEquilibrium p
      s' = eq + (unitValue s - eq) * exp (negate (satA p + satB p) * t)
  in Familiarity (fDecay p t f) (clampUnit s')

-- | d 分、接触していない。F も S も弱まる。
apart :: Params -> Minutes -> Familiarity -> Familiarity
apart p d (Familiarity f s) =
  let t = minutesValue d
  in Familiarity (fDecay p t f) (clampUnit (unitValue s * 2 ** (negate t / (pHSDays p * minutesPerDay))))

fDecay :: Params -> Double -> Unit -> Unit
fDecay p t f = clampUnit (unitValue f * 2 ** (negate t / (pHFDays p * minutesPerDay)))


-- | 五分の一：ユーザーに関わることが、自分の時間を占めすぎないように（設計稿
-- 「数学修正」第 8 条）。
--
-- 出どころの分かっている、自分から投じた時間だけを数える（休み、ぼんやり、一時停止、
-- コマの終わりの待ちは入れない）。出どころが分からないものは別に数え、無関係には入れない。
-- 記録は 2.5 日の半減期で弱まる。
--
-- 比率ではなく、「五分の一を超えた分の関わる時間」を見る：
--
-- > E = A − 0.2·B、h = min(1, max(0, E)/X)、g = 1 − (2/3)·h
--
-- 5 分なにかをするたびに、ユーザーに関わることへ 1 分の枠が増える、と考えてよい。
-- 前の書き方（比率に、データ量の重み B/(B + 30) を掛ける）は、比率が 40% を超えると
-- 頭打ちになり、無関係なことをするほど押しが強くなった（Codex の指摘、2026-10-22）。
-- g は「何を目の前に出すか」の層でだけ、関わる候補の重みに掛ける。興味にもドリフトにも
-- 入れない。
data SourceKind = Related | Unrelated | UnknownSource
  deriving (Eq, Show)

-- | 弱まったあとの記録（分）。slAll は出どころの分かっているものすべて（関わるものも含む）。
data SourceLedger = SourceLedger
  { slRelated :: Double
  , slAll     :: Double
  , slUnknown :: Double
  }
  deriving (Eq, Show)

emptySourceLedger :: SourceLedger
emptySourceLedger = SourceLedger 0 0 0

decayRate :: Params -> Double
decayRate p = log 2 / (pSrcHalfDays p * 1440)

-- | d 分、何も投じていない。記録が弱まるだけ。
ledgerIdle :: Params -> Minutes -> SourceLedger -> SourceLedger
ledgerIdle p d (SourceLedger a b u) =
  let r = exp (negate (decayRate p) * minutesValue d)
  in SourceLedger (a * r) (b * r) (u * r)

-- | d 分、ある出どころのことを投じた。古い記録は弱まり、新しい分は起きた時刻どおりに
-- 弱まりながら入る（その d 分の積分）。
ledgerActive :: Params -> SourceKind -> Minutes -> SourceLedger -> SourceLedger
ledgerActive p kind d ledger =
  let lam = decayRate p
      t = minutesValue d
      r = exp (negate lam * t)
      added = if t <= 0 then 0 else (1 - r) / lam
      SourceLedger a b u = ledgerIdle p d ledger
  in case kind of
       Related -> SourceLedger (a + added) (b + added) u
       Unrelated -> SourceLedger a (b + added) u
       UnknownSource -> SourceLedger a b (u + added)

-- | 五分の一を超えた分の関わる時間（分）。データがなければ 0。
excessMinutes :: Params -> SourceLedger -> Double
excessMinutes p (SourceLedger a b _) = a - pSrcLine p * b

-- | 下押しの g（1/3 ≤ g ≤ 1）。
sourcePressure :: Params -> SourceLedger -> Double
sourcePressure p ledger =
  let h = min 1 (max 0 (excessMinutes p ledger) / pSrcX p)
  in 1 - (1 - pSrcFloor p) * h


-- | 価値と、ほかで得られそうな価値（設計稿「数学修正」第 2・7 条）。
--
-- V = I + w_F·F − S − λ·e²。選ぶときに比べるのは、次の一区切りの一分あたりの価値で、
-- 好みの量であって、測った収益ではない。
--
-- 「ほか」（V_else）は、ほかの実行できる候補を、選ぶときと同じ softmax で重みを
-- つけて平均したもの。確率は選ぶときに本当に使う値 V_i から（五分の一の下押しの前、
-- log g は入れない）、値は「触れたあと」の V_i⁺ で計算する（同口径）。触れたあとの値は、
-- 次の一歩が新しい接触になる候補にだけ、同じ newContact を当ててから同じ V の式で計算する
-- （補正の式を別に持たない。w_S = 1 と書いていた式と同じ値になる。Codex の指摘、2026-10-22）。
-- ほかの候補が一つもなければ V_else = V_rest（softmax はしない）。同じものの入口が
-- いくつあっても一つに数える（Codex の指摘、2026-10-22）。
-- | 何もしないで休むことの価値。比べる相手として置く。
vRest :: Double
vRest = 0

-- | V = I + w_F·F − S − λ·e²。
valueOf :: Params -> Unit -> Familiarity -> Double -> Unit -> Double
valueOf p interest fam lam e =
  unitValue interest + pWF p * unitValue (famF fam) - unitValue (famS fam)
    - lam * unitValue e * unitValue e

-- | ほかの候補。同じ材料は同じ 'candId' を持つ。
data Candidate = Candidate
  { candId         :: MaterialId
  , candInterest   :: Unit
  , candFam        :: Familiarity
  , candBurden     :: Unit
  , candNewContact :: Bool  -- ^ 次の一歩が新しい接触になるか（同じ接触の続きなら False）
  }
  deriving (Eq, Show)

-- | 選ぶときの値。
candidateValue :: Params -> Double -> Candidate -> Double
candidateValue p lam c = valueOf p (candInterest c) (candFam c) lam (candBurden c)

-- | 触れたあとの値（同口径）。新しい接触になる候補には、実際の接触と同じ newContact を当てる。
candidateValueAfter :: Params -> Double -> Candidate -> Double
candidateValueAfter p lam c
  | candNewContact c = valueOf p (candInterest c) (newContact p (candFam c)) lam (candBurden c)
  | otherwise = candidateValue p lam c

-- | 同じ材料の入口がいくつあっても一つにする（最初のものを残す）。
dedupeCandidates :: [Candidate] -> [Candidate]
dedupeCandidates = go Set.empty
  where
    go _ [] = []
    go seen (c : cs)
      | Set.member (candId c) seen = go seen cs
      | otherwise = c : go (Set.insert (candId c) seen) cs

-- | softmax(v/τ)。空なら空。
softmax :: Double -> [Double] -> [Double]
softmax _ [] = []
softmax tau vs =
  let top = maximum vs
      ws = [exp ((v - top) / tau) | v <- vs]
      z = sum ws
  in [w / z | w <- ws]

-- | V_else = max(V_rest, Σ p_i·V_i⁺)。ほかの候補がなければ V_rest。
vElse :: Params -> Double -> [Candidate] -> Double
vElse p lam cands =
  case dedupeCandidates cands of
    [] -> vRest
    cs ->
      let ps = softmax (pTau p) (map (candidateValue p lam) cs)
          after = map (candidateValueAfter p lam) cs
      in max vRest (sum (zipWith (*) ps after))


-- | 自分の時間の出来事を、一つの状態に順に当てていく（設計稿「数学修正」第 4〜8 条）。
--
-- 疲れと借り（'Hikaru.OwnTime.Fatigue'）、材料ごとの慣れと飽き、いま関わっている
-- ことのドリフト、五分の一の記録、コマの先行量 A を、一つの 'Engine' にまとめる。
--
-- 先行量 A は、材料ごとの列（先に渡したものから順）で持つ。もう渡して、まだ実
-- 時間に裏づけられていない読む量で、一時停止・再起動・外出・日の変わり目を跨いでも
-- 消えない。裏づけるとき（'Carry'）、その時間は列の先頭の材料のものとして数える：
-- その材料の接触の時間（S が増える）、その材料の出どころの時間、その材料がいま関わって
-- いることならドリフトの活動の時間。裏づけている時間は活動で、静けさではないし、
-- 休みでもない（R は弱まらない）。
--
-- 渡す（'Deliver'）は、その日の読む枠と努力を一度だけ取る。一コマで渡せるのは
-- Q = L − A まで。裏づけは枠も努力も取らない。
--
-- ドリフトの証拠は本当の節目（'StopAt'）で計算し、前の節目からの活動の時間で積分
-- する。積分していない活動があるまま一時停止（'Pause'）や休み（'Rest'）に入ると、
-- 順番が狂う（活動のあとの停止を、停止のあとの活動として数えてしまう）ので、断る。
-- ふつうに終える（'EndActivity'）とき、別の材料に切り替えるとき、選び直す（'Rechoose'）ときも
-- 同じで、先に節目を（選び直す前の活動を、選び直したあとの区切りに混ぜないため）。
-- 裏づけの途中で A が尽きたときも、積分していない活動があれば、そこで止まって残りの時間を
-- 返す（'StopNeeded'）。呼ぶ側が節目を済ませてから、残りを空きとして渡す（空きの弱まり
-- を、まだ入っていない証拠より先に当てないため。Codex の指摘、2026-10-22）。外出の終わり
-- （'EndOuting'）だけは、待たずに止める。積分できなかった分は 'EndedUnsettled' で返す。
--
-- 線を越えたことは、決めるまで残る。一度め（'FirstCrossing'）なら選び直すまで、二度め
-- （'SecondCrossing'）ならこのことを終えるまで。そのあいだ休みや一時停止で x が弱まっても、
-- 節目をもう一度通っても、越えたことは消えない。そして、決めるまでは渡さない
-- （'DecisionPending'）。「もう十分」、外出の終わり、上限は、いつでも終えられる。
--
-- 選び直しの機会は、このことについて回る（第 7 条）。一度目に線を越えて選び直したら
-- （続けても終えても）、あるいは越えたまま終えたら、同じ一回の自分の時間（一回の外出）の
-- あいだは、終えてから回り道して同じ材料に戻ってきても、もう機会はない（次に越えたら
-- 終わる）。外出が終わったら（'EndOuting'）、関わっていることも終わり、機会の記録も消える。
--
-- 材料を知る（'Introduce'）のは、知らない材料だけ。知っている材料をもう一度知っても、
-- 慣れと飽きを消さないように断る（'AlreadyKnown'）。
-- | 材料ごとの状態。msUpdated は、慣れと飽きを最後に進めた実際の時刻（分）。
data MaterialState = MaterialState
  { msFam      :: Familiarity
  , msUpdated  :: Double
  , msOrigin   :: Origin
  , msInterest :: Unit   -- ^ 内容そのものの面白さの見積もり（飽きや疲れを含まない）
  , msBurden   :: Unit   -- ^ 次の一区切りの負担の見積もり
  }
  deriving (Eq, Show)

-- | 渡したが、まだ裏づけていない読む量。
data Pending = Pending
  { pendMaterial :: MaterialId
  , pendMinutes  :: Double
  }
  deriving (Eq, Show)

-- | いま関わっていること。acUnintegrated は、前の節目から積分していない活動の分。
data Activity = Activity
  { acMaterial     :: MaterialId
  , acDrift        :: Drift
  , acUnintegrated :: Double
  , acLine         :: LineCheck  -- ^ 直近の節目で見た線（一度めに越えたら、選び直すまで残る）
  }
  deriving (Eq, Show)

data Engine = Engine
  { enFatigue   :: FatigueState
  , enSource    :: SourceLedger
  , enMaterials :: Map.Map MaterialId MaterialState
  , enActivity  :: Maybe Activity
  , enAdvance   :: [Pending]   -- ^ 先行量 A（先に渡したものが先頭）
  , enClock     :: Double      -- ^ 実時間の時計（分）
  , enBusy      :: [Interval]  -- ^ 自分の時間の論理の活動区間（裏づけている時間）
  , enRechosen  :: Set.Set MaterialId -- ^ この外出のあいだに、もう選び直しの機会を使った材料
  }
  deriving (Show)

data Event
  = Introduce MaterialId Origin Unit Unit     -- ^ 材料を知る（起点、面白さの見積もり、負担の見積もり）。知らない材料だけ
  | StartAttempt (Maybe Unit)           -- ^ 自分の時間を始めてみる（その日の自評は一度だけ）
  | NightQuiet Minutes                  -- ^ 開いているウィンドウの中の静けさの累計
  | Deliver MaterialId Burden Minutes         -- ^ 読む一区切りを渡す（換算の分）
  | Record MaterialId Burden Minutes          -- ^ もう読んだ分を記す（2026-10-28、Codex の再々審査）。許しを問わない：線の答え待ちでも、
                                        --   コマ・その日・努力の枠を超えても断らない（起きたことだから）。超えた分は A が L を超えて残り、
                                        --   実時間で裏づけられるまで次の一区切りを渡せない。線の決まりは変えない（答えの代わりをしない）
  | Work MaterialId Burden Minutes            -- ^ 書いた・作った（本当に過ごした分）。読む枠は使わず、A にも積まない。時計はその分進み、
                                        --   その材料の接触・出どころ・ドリフトの活動の時間になる（2026-10-28）
  | Carry Minutes                       -- ^ 実時間が経つ。A を裏づける（A を超えた分は空き。ただし積分していない活動があれば、A が尽きたところで止まる）
  | StopAt (Maybe Willing) [Candidate]  -- ^ 本当の節目。証拠を積分して線を見る
  | Rechoose Bool                       -- ^ 一度め越えたあと：続ける（True）／このことを終える（False）
  | Pause Minutes                       -- ^ 一時停止（ユーザーが戻った、など）
  | Rest Minutes                        -- ^ 明確な休み
  | Idle Minutes                        -- ^ A が清算して、次の割り当てを待つ空き
  | Elapse Minutes                      -- ^ 自分の時間の外で実時間が経つ（夜、ユーザーが家にいる）。A は裏づけない
  | EndActivity                         -- ^ いま関わっていることを終える（A は残る）
  | EndOuting                           -- ^ この外出の自分の時間が終わる（関わっていることも終わる。A は残る）
  | EndDay                              -- ^ HouseDay を閉じる
  deriving (Show)

data EngineRefusal
  = FatigueRefused Refusal
  | GridFull           -- ^ このコマで渡せる量（L − A）がもうない
  | StopFirst          -- ^ 積分していない活動がある。先に節目を
  | NoActivity         -- ^ 関わっていることがない
  | UnknownMaterial MaterialId
  | AlreadyKnown MaterialId  -- ^ もう知っている材料（慣れと飽きを消さない）
  | DecisionPending LineCheck  -- ^ 線を越えたことをまだ決めていない（選び直すか、終えるか）
  deriving (Eq, Show)

data Outcome
  = Done
  | Delivered Minutes        -- ^ 本当に渡した分（L − A と、その日の残りで短くなることがある）
  | Started StartView
  | Line LineCheck
  | StopNeeded Minutes       -- ^ A が尽きたところで止まった。先に節目を済ませて、残りの分を空きとして渡す
  | EndedUnsettled Minutes   -- ^ 外出を終えた。この分の活動は積分できなかった（自述も取れなかった）
  | Refused EngineRefusal
  deriving (Eq, Show)

engineStart :: Params -> Engine
engineStart p = Engine (fatigueStart p zeroUnit zeroUnit 0) emptySourceLedger Map.empty Nothing [] 0 [] Set.empty

advanceTotal :: Engine -> Double
advanceTotal = sum . map pendMinutes . enAdvance

-- | 材料の慣れと飽きを、今の時刻まで（接触していないとして）進めたもの。
materialAt :: Params -> Engine -> MaterialId -> Maybe MaterialState
materialAt p e mat = catchUp p (enClock e) <$> Map.lookup mat (enMaterials e)

catchUp :: Params -> Double -> MaterialState -> MaterialState
catchUp p now m
  | now > msUpdated m = m { msFam = apart p (clampMinutes (now - msUpdated m)) (msFam m), msUpdated = now }
  | otherwise = m

sourceKind :: Origin -> SourceKind
sourceKind o = case isHumanSpeakerRelated o of
  Just True -> Related
  Just False -> Unrelated
  Nothing -> UnknownSource

steps :: Params -> [Event] -> Engine -> ([Outcome], Engine)
steps p evs e0 = let (outs, e) = foldl' go ([], e0) evs in (reverse outs, e)
  where go (acc, e) ev = let (o, e') = step p ev e in (o : acc, e')

step :: Params -> Event -> Engine -> (Outcome, Engine)
step p ev e = case ev of
  Introduce mat origin interest burden
    | Map.member mat (enMaterials e) -> (Refused (AlreadyKnown mat), e)
    | otherwise ->
        (Done, e { enMaterials = Map.insert mat (MaterialState freshFamiliarity (enClock e) origin interest burden) (enMaterials e) })

  StartAttempt report ->
    let (view, f') = startCheck p report (enFatigue e) in (Started view, e { enFatigue = f' })

  NightQuiet cumulative -> (Done, e { enFatigue = settleNight p cumulative (enFatigue e) })

  Deliver mat burden want -> case Map.lookup mat (enMaterials e) of
    Nothing -> (Refused (UnknownMaterial mat), e)
    Just _ | Just act <- enActivity e, acLine act /= NotYet -> (Refused (DecisionPending (acLine act)), e)
    Just _ | Just act <- enActivity e, acMaterial act /= mat, acUnintegrated act > 0 -> (Refused StopFirst, e)
    Just _ ->
      let q = pGridL p - advanceTotal e
          wanted = min (minutesValue want) q
      in if wanted <= 0 then (Refused GridFull, e) else
         case reservePiece p (clampMinutes wanted) (enFatigue e) of
           Left r -> (Refused (FatigueRefused r), e)
           Right d -> step p (Record mat burden d) e

  Record mat burden len -> case Map.lookup mat (enMaterials e) of
    Nothing -> (Refused (UnknownMaterial mat), e)
    Just _ | Just act <- enActivity e, acMaterial act /= mat, acUnintegrated act > 0 -> (Refused StopFirst, e)
    Just m0 ->
      let sameContact = maybe False ((== mat) . acMaterial) (enActivity e)
          m1 = catchUp p (enClock e) m0
          m2 = if sameContact then m1 else m1 { msFam = newContact p (msFam m1) }
          stage = if Set.member mat (enRechosen e) then Rechosen else FirstRun
          activity = if sameContact then enActivity e else Just (Activity mat (Drift 0 stage) 0 NotYet)
      in ( Delivered len
         , e { enFatigue = settlePiece p burden len (enFatigue e)
             , enMaterials = Map.insert mat m2 (enMaterials e)
             , enActivity = activity
             , enAdvance = enAdvance e ++ [Pending mat (minutesValue len)] } )

  Work mat burden len -> case Map.lookup mat (enMaterials e) of
    Nothing -> (Refused (UnknownMaterial mat), e)
    Just _ | Just act <- enActivity e, acMaterial act /= mat, acUnintegrated act > 0 -> (Refused StopFirst, e)
    Just m0 ->
      -- もう過ごした時間なので、線の答え待ちでも断らない（Record と同じ）
      let d = minutesValue len
          now = enClock e
          sameContact = maybe False ((== mat) . acMaterial) (enActivity e)
          m1 = catchUp p now m0
          m2 = if sameContact then m1 else m1 { msFam = newContact p (msFam m1) }
          m3 = m2 { msFam = duringContact p len (msFam m2), msUpdated = now + d }
          stage = if Set.member mat (enRechosen e) then Rechosen else FirstRun
          act0 = if sameContact then enActivity e else Just (Activity mat (Drift 0 stage) 0 NotYet)
          act' = fmap (\a -> a { acUnintegrated = acUnintegrated a + d }) act0
      in ( Done
         , e { enFatigue = settleWork p burden len (enFatigue e)
             , enMaterials = Map.insert mat m3 (enMaterials e)
             , enSource = ledgerActive p (sourceKind (msOrigin m0)) len (enSource e)
             , enActivity = act'
             , enClock = now + d
             , enBusy = enBusy e ++ [Interval now (now + d)] } )

  Carry len -> case carry p (minutesValue len) e of
    (left, e') | left > 0 -> (StopNeeded (clampMinutes left), e')
               | otherwise -> (Done, e')

  StopAt willing cands -> case enActivity e of
    Nothing -> (Refused NoActivity, e)
    Just act -> case materialAt p e (acMaterial act) of
      Nothing -> (Refused (UnknownMaterial (acMaterial act)), e)
      Just m ->
        let lam = lambdaNow p (enFatigue e)
            vHere = valueOf p (msInterest m) (msFam m) lam (msBurden m)
            others = filter ((/= acMaterial act) . candId) cands
            m' = evidence p willing vHere (vElse p lam others)
            dr = activeFor p (clampMinutes (acUnintegrated act)) m' (acDrift act)
            -- 越えたことは、決めるまで残る（x が弱まっても、節目をもう一度通っても）
            seen = if acLine act /= NotYet then acLine act else checkLine p dr
        in (Line seen, e { enActivity = Just act { acDrift = dr, acUnintegrated = 0, acLine = seen } })

  Rechoose keep -> case enActivity e of
    Just act | acUnintegrated act > 0 -> (Refused StopFirst, e)
    Just act | acLine act == FirstCrossing ->
      let used = Set.insert (acMaterial act) (enRechosen e)
      in if keep then (Done, e { enActivity = Just act { acDrift = rechooseContinue (acDrift act), acLine = NotYet }, enRechosen = used })
                 else (Done, e { enActivity = Nothing, enRechosen = used })
    _ -> (Refused NoActivity, e)

  Pause len -> suspended len id
  Rest len -> suspended len (restFor p len)
  Idle len -> suspended len id
  Elapse len -> suspended len id

  EndActivity -> case enActivity e of
    Just act | acUnintegrated act > 0 -> (Refused StopFirst, e)
    Just act | acLine act == FirstCrossing ->
      -- 一度め越えたまま終えた：機会は使ったものとする（戻ってきても、もう選び直さない）
      (Done, e { enActivity = Nothing, enRechosen = Set.insert (acMaterial act) (enRechosen e) })
    _ -> (Done, e { enActivity = Nothing })

  EndOuting ->
    let unsettled = maybe 0 acUnintegrated (enActivity e)
        e' = e { enActivity = Nothing, enRechosen = Set.empty }
    in if unsettled > 0 then (EndedUnsettled (clampMinutes unsettled), e') else (Done, e')

  EndDay -> (Done, e { enFatigue = endHouseDay p (enFatigue e) })
  where
    -- 一時停止・休み・空き：活動の証拠は入らず、前の証拠が弱まるだけ。
    suspended len fatigueOp = case enActivity e of
      Just act | acUnintegrated act > 0 -> (Refused StopFirst, e)
      _ -> let t = minutesValue len
               act' = fmap (\a -> a { acDrift = pausedFor p len (acDrift a) }) (enActivity e)
           in (Done, e { enActivity = act', enFatigue = fatigueOp (enFatigue e)
                       , enSource = ledgerIdle p len (enSource e), enClock = enClock e + t })

-- | 実時間 t 分。A を先頭から裏づけ、その材料の接触・出どころ・ドリフトの時間として数える。
-- A が尽きたら、残りは空き（活動の証拠は入らず、出どころの記録は弱まるだけ）。ただし、
-- 積分していない活動があれば、空きに入る前に止まって、残りの分を返す。
carry :: Params -> Double -> Engine -> (Double, Engine)
carry p t0 e0 = go t0 e0
  where
    go t e = case enAdvance e of
      _ | t <= 0 -> (0, e)
      [] | maybe False ((> 0) . acUnintegrated) (enActivity e) -> (t, e)
         | otherwise ->
             (0, e { enActivity = fmap (\a -> a { acDrift = pausedFor p (clampMinutes t) (acDrift a) }) (enActivity e)
                   , enSource = ledgerIdle p (clampMinutes t) (enSource e), enClock = enClock e + t })
      (Pending mat amount : rest) ->
        let c = min t amount
            now = enClock e
            mats' = Map.adjust (\m -> let m1 = catchUp p now m
                                      in m1 { msFam = duringContact p (clampMinutes c) (msFam m1), msUpdated = now + c })
                               mat (enMaterials e)
            origin = maybe Nothing (Just . msOrigin) (Map.lookup mat (enMaterials e))
            src' = case origin of
              Just o -> ledgerActive p (sourceKind o) (clampMinutes c) (enSource e)
              Nothing -> ledgerIdle p (clampMinutes c) (enSource e)
            act' = case enActivity e of
              Just a | acMaterial a == mat -> Just a { acUnintegrated = acUnintegrated a + c }
              other -> other
            queue' = if c >= amount then rest else Pending mat (amount - c) : rest
            e' = e { enMaterials = mats', enSource = src', enActivity = act', enAdvance = queue'
                   , enClock = now + c, enBusy = enBusy e ++ [Interval now (now + c)] }
        in go (t - c) e'


-- | 帳簿：実時間の出来事を、引擎（'Hikaru.OwnTime.Engine'）の出来事に写す（2026-10-28）。
--
-- 引擎は分の時計で動く純粋な計算で、「いま何時か」「ユーザーが帰ってきた」「起きたわたしがこれだけ読んだ」を
-- 知らない。ここが、その橋。閉じた輪の模擬（test-owntime/OwnTimeSim.hs）で世界と代役がしていたことのうち、
-- 本物の世界と本物のわたしに置き換わらない部分――時間の流れを四種の区間に分けて当てる、節目で線を見る、
-- 段の始まりと終わり、日の変わり目――を、ここに移した。
--
--   * 実時間は、帳簿が最後に引擎を進めた時刻（'lgSyncedAt'）から今までを、そのときの区間の種類で当てる：
--     自分の時間の外（'Elapse'）、一時停止（'Pause'）、休み（'Rest'）、時段が開いている（'Carry'。先行量 A を
--     裏づけ、尽きた残りは空き）。積分していない活動があって引擎が「先に節目を」と言えば、気持ちを聞かずに節目を
--     打ってから続ける（模擬の「話が来た・帰った、その時は聞かない」と同じ）。
--   * 起きたわたしが走っているあいだは進めない。落ち着いたとき（'landing'）に、渡した量（'Deliver'）を先に
--     記してから、コマの始まりから今までを 'Carry' で裏づける。渡した量を記す前に時間を流すと、読んでいた時間が
--     空きに数えられてしまう（設計稿「数学修正」第 4 条の「格内最多超前一格」）。
--   * 段の始まり（'segmentStart'）では、夜の窓の静けさを精算して（'NightQuiet'）、始めてよいか聞く
--     （'StartAttempt'）。枠が 5 分に満たなければ起こさない。
--   * 段の終わり（'segmentEnd'）では、積分していない活動があれば節目を打ってから 'EndOuting'。A は残る。
--   * 日の変わり目（'dayEnd'）で 'EndDay'。その日に本当に投じていれば、夜の窓はその日の最後の忙しい区間の
--     終わりから数え直す。
--   * 候補（'proposals'）：知っている材料と、まだ知らない候補（inbox、前から読みたかったもの）に、
--     いまの λ で値をつけ、ユーザーに関わるものには五分の一の下押し g を掛けて、softmax（τ）で重みにする。
--     一つを引いて提案にし、並べて起きたわたしに見せる。選ぶのは起きたわたし。
--
-- 時刻の写し方：引擎の時計（分）＝ 'lgEpoch' からの分。'lgSyncedAt' を進めるたびに同じだけ引擎にも流す。
data Ledger = Ledger
  { lgEngine      :: Engine
  , lgEpoch       :: UTCTime         -- ^ 引擎の時計の 0
  , lgSyncedAt    :: UTCTime         -- ^ ここまで引擎を進めた
  , lgDayStart    :: UTCTime         -- ^ いまの HouseDay の始まり（引擎に EndDay を当てた日の次）
  , lgWindowStart :: UTCTime         -- ^ 夜の回復の窓の始まり
  , lgInSegment   :: Bool            -- ^ 段の中（segmentStart 済み、segmentEnd 前）
  , lgRestUntil   :: Maybe UTCTime   -- ^ 休むと決めた時刻まで（起きたわたしが「先に休みたい」と言った）
  , lgSegment     :: Maybe SegmentId      -- ^ いまの段の鍵（見張りの段の始まりと種類）。鍵が替わったら旧い段を閉じてから開く
  , lgSettled     :: [Int]           -- ^ もう帳簿に入れた運行の番号（新しいものから 50 まで）。同じ書き置きを二度入れない
  , lgObs         :: Map.Map MaterialId [Obs]  -- ^ 材料ごとの、読んだあとの面白さ・負担の観察（設計稿「数学修正」第 9 条）
  , lgAlias       :: Map.Map MaterialId MaterialId   -- ^ 候補の入口（inbox/ファイル名）→ 帳簿の材料の名前。同じものを二つに数えない
  }
  deriving (Show)

-- | 一度の接触の、読んだあとの観察。時刻は引擎の時計（分）。ないものは Nothing（0 で埋めない）。
data Obs = Obs { obAt :: Double, obInterest :: Maybe Double, obBurden :: Maybe Double }
  deriving (Eq, Show)

ledgerStart :: Params -> UTCTime -> UTCTime -> Ledger
ledgerStart p now dayStart = Ledger (engineStart p) now now dayStart now False Nothing Nothing [] Map.empty Map.empty

-- | 見積もり：μ = (1·0.5 + Σ w_j·y_j)/(1 + Σ w_j)、w_j = 2^(−経った日/14)。一般の見積もりの重みは弱まらないので、
-- 古い観察が薄れると 0.5 に戻っていく（設計稿「数学修正」第 9 条）。観察がなければ Nothing。
estimateAt :: Double -> (Obs -> Maybe Double) -> [Obs] -> Maybe Double
estimateAt clock field obs =
  let ws = [ (2 ** (negate (max 0 (clock - obAt o)) / (14 * 1440)), y) | o <- obs, Just y <- [field o] ]
  in if null ws then Nothing else Just ((0.5 + sum [ w * y | (w, y) <- ws ]) / (1 + sum (map fst ws)))

-- | 実時間が、どの種類の区間として流れているか。
data Mode = Outside | PausedMode | OpenMode
  deriving (Eq, Show)

minutesSince :: UTCTime -> UTCTime -> Double
minutesSince from to = realToFrac (diffUTCTime to from) / 60

minutesOf :: Double -> NominalDiffTime
minutesOf m = realToFrac (m * 60)

-- 候補 ------------------------------------------------------------------------------

-- | まだ引擎が知らない候補（inboxのファイル、前から読みたかったもの）。面白さも負担も、一般の見積もり 0.5。
data NewCandidate = NewCandidate { ncId :: MaterialId, ncOrigin :: Origin }
  deriving (Eq, Show)

generalGuess :: Unit
generalGuess = clampUnit 0.5

-- | いま選べる候補（知っている材料を今の時刻まで進めたもの＋新しい候補）。except は外す材料。
candidatesNow :: Params -> Ledger -> [NewCandidate] -> [MaterialId] -> [Candidate]
candidatesNow p lg news except =
  let e = lgEngine lg
      current = fmap acMaterial (enActivity e)
      est k field fallback = maybe fallback clampUnit (Map.lookup k (lgObs lg) >>= estimateAt (enClock e) field)
      known = [ Candidate k (est k obInterest (msInterest m)) (msFam m) (est k obBurden (msBurden m)) (Just k /= current)
              | (k, _) <- Map.toList (enMaterials e), k `notElem` except
              , Just m <- [materialAt p e k] ]
      fresh = [ Candidate (ncId n) generalGuess freshFamiliarity generalGuess True
              | n <- news, ncId n `notElem` except, not (Map.member (ncId n) (enMaterials e))
              , not (Map.member (ncId n) (lgAlias lg)) ]
  in dedupeCandidates (known ++ fresh)

-- | 起きたわたしに見せる一つ分。
data Proposal = Proposal
  { prId      :: MaterialId
  , prOrigin  :: Maybe Origin   -- ^ 知らない候補なら、その出どころ。知っている材料は引擎のもの
  , prValue   :: Double
  , prWeight  :: Double         -- ^ softmax の重み（五分の一の下押しのあと）
  , prPicked  :: Bool           -- ^ 引いたのはこれ
  }
  deriving (Eq, Show)

originOfCand :: Ledger -> [NewCandidate] -> MaterialId -> Maybe Origin
originOfCand lg news k = case Map.lookup k (enMaterials (lgEngine lg)) of
  Just m -> Just (msOrigin m)
  Nothing -> ncOrigin <$> lookupNew
  where lookupNew = case [n | n <- news, ncId n == k] of { (n : _) -> Just n; [] -> Nothing }

-- | 候補に値と重みをつけ、seed で一つ引く。空なら空（休むしかない）。
proposals :: Params -> Ledger -> [NewCandidate] -> Double -> [Proposal]
proposals p lg news seed =
  let e = lgEngine lg
      lam = lambdaNow p (enFatigue e)
      cands = candidatesNow p lg news []
      g = sourcePressure p (enSource e)
      related k = maybe False (fromMaybe False . isHumanSpeakerRelated) (originOfCand lg news k)
      vals = [ (c, candidateValue p lam c) | c <- cands ]
      -- 下押しは「何を目の前に出すか」の層でだけ：τ·log g を値に足してから softmax(v/τ)（重みに g を掛けるのと同じ。
      -- 2026-10-28 まで τ を掛け忘れていて、g が g^(1/τ) になっていた。Codex の審査 B3）
      adj = [ (c, v, if related (candId c) then v + pTau p * log g else v) | (c, v) <- vals ]
      ws = softmax (pTau p) [ a | (_, _, a) <- adj ]
      rows = zipWith (\(c, v, _) w -> (c, v, w)) adj ws
      sorted = sortOn (\(_, _, w) -> negate w) rows
      r = seed - fromIntegral (floor seed :: Int)
      pick = choose r sorted
  in [ Proposal (candId c) (originOfCand lg news (candId c)) v w (Just (candId c) == pick) | (c, v, w) <- sorted ]
  where
    choose _ [] = Nothing
    choose r ((c, _, w) : rest)
      | r <= w || null rest = Just (candId c)
      | otherwise = choose (r - w) rest

-- | 今から使える枠（努力分）。5 分に満たなければ起こさない。
roomNow :: Params -> Ledger -> Double
roomNow p lg = viewRoom (fst (startCheck p Nothing (enFatigue (lgEngine lg))))

-- | 次のコマで読んでよい量（換算の分）：コマの残り（L − A）、その日の読む上限の残り、努力の枠の、いちばん小さいもの。
-- brief にこれを渡し、これが 5 分に満たなければ起こさない（Codex の審査 A5）。読みすぎた分は A が L を超えて残るので、
-- 実時間で裏づけられるまで、ここが 0 になって次は起きない。
quotaNow :: Params -> Ledger -> Double
quotaNow p lg =
  let e = lgEngine lg
  in maximum [0, minimum [pGridL p - advanceTotal e, pDayCap p - dayUsed (enFatigue e), roomNow p lg]]

-- | 一コマを起こしてよいか（読むかどうかに関わらず）：A がコマの時間を食いつぶしていないことと、努力の枠。
-- その日の読む枠は入れない（読む枠を使い切っても、書く・作る・休むのはできる。Codex の審査 38 の第六類）。
activityRoom :: Params -> Ledger -> Double
activityRoom p lg = maximum [0, minimum [pGridL p - advanceTotal (lgEngine lg), roomNow p lg]]

-- | いま関わっていることが、線を越えたまま決めていないか。
pendingLine :: Ledger -> Maybe (MaterialId, LineCheck)
pendingLine lg = case enActivity (lgEngine lg) of
  Just a | acLine a /= NotYet -> Just (acMaterial a, acLine a)
  _ -> Nothing

-- 時間を流す --------------------------------------------------------------------------

-- | 引擎に出来事を当て、結果と記録を集める。
apply :: Params -> Event -> Ledger -> (Outcome, Ledger)
apply p ev lg = let (o, e') = step p ev (lgEngine lg) in (o, lg { lgEngine = e' })

-- | 積分していない活動があれば、気持ちを聞かずに節目を打つ（線は見るだけ。越えていれば残る）。
settleIfNeeded :: Params -> [NewCandidate] -> Ledger -> (Ledger, [String])
settleIfNeeded p news lg = case enActivity (lgEngine lg) of
  Just a | acUnintegrated a > 0 ->
    let cands = candidatesNow p lg news [acMaterial a]
        (o, lg1) = apply p (StopAt Nothing cands) lg
    in (lg1, ["停靠点（没有问感受）：" ++ show o])
  _ -> (lg, [])

-- | A を裏づける（実時間 len 分のうち、A のある分だけ）。返すのは残りの分。節目は打たない（呼ぶ側が、その節目に
-- 自述があるかどうかで決める。2026-10-28、Codex の審査 B4：ここで先に「気持ちを聞かない節目」を打つと、書き置きの
-- 自述が吞まれていた）。
carryPhase :: Params -> Double -> Ledger -> (Ledger, Double, [String])
carryPhase p len lg =
  let a = advanceTotal (lgEngine lg)
      c = min len a
  in if c <= 1e-9 then (lg, len, []) else
     let (o, lg1) = apply p (Carry (clampMinutes c)) lg
         extra = case o of { StopNeeded left -> minutesValue left; _ -> 0 }
         notes = case o of { Done -> []; StopNeeded _ -> []; other -> ["引擎拒绝了：" ++ show other] }
     in (lg1, len - c + extra, notes)

-- | A が尽きたあとの時間：休むと決めた時刻までは休み（R が戻る）、そのあとは空き。t0 はこの時間の始まり。
-- 積分していない活動は、先に片づけてあること（Codex の審査 B2：前は A が空だと、休みも空きとして流れていた）。
quietPhase :: Params -> UTCTime -> Double -> Ledger -> (Ledger, [String])
quietPhase p t0 len lg
  | len <= 1e-9 = (lg, [])
  | otherwise =
      let restLen = case lgRestUntil lg of
            Just u -> max 0 (min len (minutesSince t0 u))
            Nothing -> 0
          idleLen = len - restLen
          (o1, lg1) = if restLen > 1e-9 then apply p (Rest (clampMinutes restLen)) lg else (Done, lg)
          (o2, lg2) = if idleLen > 1e-9 then apply p (Idle (clampMinutes idleLen)) lg1 else (Done, lg1)
          refused o = case o of { Refused r -> ["引擎拒绝了：" ++ show r]; _ -> [] }
      in (lg2, refused o1 ++ refused o2 ++ [ "休息了 " ++ show (round restLen :: Int) ++ " 分" | restLen > 0.5 ])

-- | 'lgSyncedAt' から now までを、mode の区間として引擎に流す。
passTime :: Params -> Mode -> [NewCandidate] -> UTCTime -> Ledger -> (Ledger, [String])
passTime p mode news now lg0
  | dt <= 1e-9 = (lg0, [])
  | otherwise = case mode of
      Outside -> interrupted (Elapse (clampMinutes dt))
      PausedMode -> interrupted (Pause (clampMinutes dt))
      OpenMode ->
        -- 開いているあいだ：A を裏づけ、尽きたら（気持ちを聞けないので）気持ちなしの節目、残りは休みか空き
        let (lg1, left, n1) = carryPhase p dt lg0
            (lg2, n2) = if left > 1e-9 then settleIfNeeded p news lg1 else (lg1, [])
            (lg3, n3) = quietPhase p (addUTCTime (minutesOf (dt - left)) (lgSyncedAt lg0)) left lg2
        in (lg3 { lgSyncedAt = now }, n1 ++ n2 ++ n3)
  where
    dt = minutesSince (lgSyncedAt lg0) now
    refused o = case o of { Refused r -> ["引擎拒绝了：" ++ show r]; _ -> [] }
    -- 外・一時停止：その時点で活動は止まったので、気持ちなしの節目を打ってから流す
    interrupted ev =
      let (lg1, n1) = settleIfNeeded p news lg0
          (o, lg2) = apply p ev lg1
      in (lg2 { lgSyncedAt = now }, n1 ++ refused o)

-- 段の始まりと終わり ----------------------------------------------------------------------

-- | 段の始まり：窓の静けさを精算して、始めてよいか聞く。返すのは枠（努力分）。
-- talkTimes は記録される一言の時刻（直近の分でよい）。key は見張りの段の鍵。
segmentStart :: Params -> SegmentId -> [UTCTime] -> [NewCandidate] -> UTCTime -> Ledger -> (Ledger, Double, [String])
segmentStart p key talkTimes news now lg0 =
  let (lg1, n1) = passTime p Outside news now lg0
      e = lgEngine lg1
      toClock t = minutesSince (lgEpoch lg1) t
      window = Interval (toClock (lgWindowStart lg1)) (toClock now)
      talks = conversationIntervals p (map toClock talkTimes)
      quiet = quietMinutes window (talks ++ enBusy e)
      (_, lg2) = apply p (NightQuiet quiet) lg1
      (o, lg3) = apply p (StartAttempt Nothing) lg2
      room = case o of { Started v -> viewRoom v; _ -> 0 }
      lg4 = lg3 { lgInSegment = True, lgRestUntil = Nothing, lgSegment = Just key }
  in (lg4, room, n1 ++ ["段开始：窗口里安静 " ++ show (round (minutesValue quiet) :: Int) ++ " 分，枠 " ++ show (round room :: Int) ++ " 努力分"])

-- | 段の終わり：節目を打ってから EndOuting。A は残る。
segmentEnd :: Params -> [NewCandidate] -> UTCTime -> Ledger -> (Ledger, [String])
segmentEnd p news now lg0 =
  let (lg1, n1) = passTime p OpenMode news now lg0
      (lg2, n2) = settleIfNeeded p news lg1
      (o, lg3) = apply p EndOuting lg2
      lg4 = lg3 { lgInSegment = False, lgRestUntil = Nothing, lgSegment = Nothing }
      note = case o of
        EndedUnsettled m -> ["段结束：有 " ++ show (round (minutesValue m) :: Int) ++ " 分的活动没结算"]
        _ -> ["段结束"]
  in (lg4, n1 ++ n2 ++ note)

-- | 日の変わり目。boundary はその日替わりの時刻（＝新しい HouseDay の始まり）。そこまで時間を流してから EndDay。
-- 何日もまたいだときは、呼ぶ側が一日ずつ呼ぶ（Codex の審査 B5）。
dayEnd :: Params -> [NewCandidate] -> UTCTime -> Ledger -> (Ledger, [String])
dayEnd p news boundary lg0 = let (l, n, _) = dayEndWith p news boundary lg0 in (l, n)

-- | 'dayEnd' と同じ。加えて、EndDay を当てる直前の疲れを返す（学参数の歴史に、その日の数として書くため）。
dayEndWith :: Params -> [NewCandidate] -> UTCTime -> Ledger -> (Ledger, [String], FatigueState)
dayEndWith p news boundary lg0 =
  let (lg1, n1) = passTime p (if lgInSegment lg0 then OpenMode else Outside) news boundary lg0
      e = lgEngine lg1
      frozen = isJust (dayFrozen (enFatigue e))
      lastBusy = case enBusy e of
        [] -> lgWindowStart lg1
        bs -> addUTCTime (minutesOf (maximum (map ivEnd bs))) (lgEpoch lg1)
      (_, lg2) = apply p EndDay lg1
      lg3 = lg2 { lgDayStart = boundary, lgWindowStart = if frozen then lastBusy else lgWindowStart lg2 }
  in (lg3, n1 ++ ["换日" ++ (if frozen then "（今天投入过，夜的窗口从最后的活动算起）" else "（今天没投入）")], enFatigue e)

-- 落ち着いたとき ---------------------------------------------------------------------------

data Rechoice = KeepGoing | LetGo
  deriving (Eq, Show)

-- | 読んだ材料の一つ分。一コマで二つ三つ読んだなら、読んだ順に並べる。
data ReportItem = ReportItem
  { riMaterial :: MaterialId
  , riOrigin   :: Origin
  , riInterest :: Maybe Unit        -- ^ 面白さ（新しい材料なら、これが最初の見積もり）
  , riBurden   :: Maybe Unit        -- ^ 読んだあとの負担の感じ
  , riMinutes  :: Double            -- ^ 換算の読んだ分
  }
  deriving (Show)

-- | 書いた・作った一つ分（思ひ草の一篇）。分は本当に過ごした分（換算しない）。
data WorkItem = WorkItem
  { wiMaterial :: MaterialId
  , wiOrigin   :: Origin
  , wiInterest :: Maybe Unit
  , wiBurden   :: Maybe Unit        -- ^ そのあいだの骨折りの強さ（0 にはしない）
  , wiMinutes  :: Double
  }
  deriving (Show)

-- | 起きたわたしが書き置きに書いたこと。
data Report = Report
  { rpItems     :: [ReportItem]      -- ^ 読んだ材料（何も読めなかったなら空）
  , rpStep      :: Maybe Step        -- ^ 読んだあとの気持ち（最後の材料について）
  , rpRechoice  :: Maybe Rechoice    -- ^ 一度め線を越えていたなら、続けるか
  , rpSlotStart :: UTCTime
  , rpSelfReport :: Maybe Unit       -- ^ その日はじめのコマで、数を見る前に言った「きょうの調子」（0 元気〜1 くたくた）。その日に一度だけ効く
  , rpWorks     :: [WorkItem]        -- ^ 書いた・作ったもの（読んだものの前に、本当の時間で精算する）
  }
  deriving (Show)

-- | 落ち着いた：渡した量を記し、コマの時間を裏づけ、自述つきの節目を打つ。runId はこの書き置きの運行の番号で、
-- もう入れた番号なら何もしない（Codex の審査 A2：帳簿と足跡と時守の状態の書き込みのあいだで落ちても、
-- 次の一分に同じ書き置きを二度入れない）。
landing :: Params -> [NewCandidate] -> UTCTime -> Int -> Report -> Ledger -> (Ledger, [String])
landing p news now runId rp lg00
  | runId `elem` lgSettled lg00 = (lg00, ["运行 #" ++ show runId ++ " 的落点已经入过账，不再入"])
  | otherwise =
  let lg0 = lg00 { lgSettled = take 50 (runId : lgSettled lg00) }
      gapNote = [ "注意：帐簿同步停在这一格起跑之前 " ++ show (round gap :: Int) ++ " 分，那一段按空档补流"
                | let gap = minutesSince (lgSyncedAt lg0) (rpSlotStart rp), gap > 1 ]
      -- 起跑前の空き（ふつうは 0）は空きとして流しておく（Codex の審査 C5：max で飛ばさない）
      (lgS, nS) = passTime p OpenMode news (max (lgSyncedAt lg0) (rpSlotStart rp)) lg0
      -- 再議の答えは、このコマを起こしたときに聞いていた問い（そのとき一度め線を越えていた材料）にだけ当てる。
      -- 読んだかどうかによらない。途中で別の材料に移って、そちらが新しく線を越えても、答えを移さない（Codex の四度目の審査）
      asked = case pendingLine lgS of { Just (m, FirstCrossing) -> Just m; _ -> Nothing }
      readAsked = maybe False (\m -> any (\it -> riMaterial it == m && riMinutes it > 0) (rpItems rp)
                                   || any (\w -> wiMaterial w == m && wiMinutes w > 0) (rpWorks rp)) asked
      answerNote = case (asked, rpRechoice rp) of
        (Nothing, Just r) -> ["这一格开始时没有在问的事，回答 " ++ show r ++ " 不用"]
        _ -> []
      -- 答えがなかったときの一行は、最後の状態を見て書く（まだ線の上か、別のものに移って終わったか。Codex の五度目の審査）
      unansweredNote lg = case (asked, rpRechoice rp) of
        (Just m, Nothing) -> case pendingLine lg of
          Just (m', FirstCrossing) | m' == m -> ["「" ++ materialString m ++ "」第一次到线，还没回答，下一格再问"]
          _ -> ["「" ++ materialString m ++ "」第一次到线没回答，这一格换读了别的，它已经结掉"]
        _ -> []
      -- let_go と答えて、その材料も読んでいたら：読んだ分を記してから終える。それ以外は、読む前に答えを当てる
      answerLater = rpRechoice rp == Just LetGo && readAsked
      (lgQ, nQ) = if answerLater then (lgS, []) else answer asked lgS
      -- その日はじめの入れより前なら、自評を渡す（その日に一度だけ。凍る前にしか効かない。何も読まなかった書き置きでも。設計稿「自評」）
      (lgQs, nQs) = case rpSelfReport rp of
        Just r | isNothing (dayFrozen (enFatigue (lgEngine lgQ))) ->
          let (_, l) = apply p (StartAttempt (Just r)) lgQ in (l, ["今天的自评 " ++ show (unitValue r)])
        _ -> (lgQ, [])
      -- このコマを起こしたときに続いていた接触（観察を一つにまとめるため）
      contAtStart = fmap acMaterial (enActivity (lgEngine lgS))
  in case (rpItems rp, rpWorks rp) of
  ([], []) ->
    let (lg1, n1) = passTime p OpenMode news now lgQs
        (lg2, n2) = afterFeeling lg1
    in (lg2, gapNote ++ nS ++ answerNote ++ nQ ++ nQs ++ n1 ++ ["这一格没有读到东西"] ++ n2 ++ unansweredNote lg2)
  (items, works) ->
    let mats = map wiMaterial works ++ map riMaterial items
        -- 書いた・作った時間を先に（本当の時間なので、そのぶん時計が進む。コマの時間を超えないように切る）
        wall = max 0 (minutesSince (lgSyncedAt lgQs) now)
        (lgW, nW, _) = foldl workOne (lgQs, [], wall) works
        -- 読んだことを記す（許しは問わない。線の答えの代わりもしない。Codex の再々審査）
        (lgD0', nD0'') = foldl recordOne (lgW, []) items
        nD0' = nW ++ nD0''
        -- 読んだあとの観察を、材料ごとに一度だけ（同じ材料が二度出てきたら、あとのほう）。見積もりに入れる（C1）
        lgD0 = observe contAtStart (enClock (lgEngine lgQ)) lgD0' ([ ReportItem (wiMaterial w) (wiOrigin w) (wiInterest w) (wiBurden w) (wiMinutes w) | w <- works ] ++ items)
        nD0 = nQs ++ nD0'
        -- let_go を後回しにしていたら、ここで（まだその材料に関わっていれば）当てる
        (lgD, nD) = if answerLater then let (l, n) = answer asked lgD0 in (l, nD0 ++ n) else (lgD0, nD0)
        lastMat = last mats
        -- コマの始まりから今までを裏づける。A が尽きても、ここでは気持ちなしの節目を打たない
        dt = minutesSince (lgSyncedAt lgD) now
        (lgE, left, nE) = carryPhase p dt lgD
        -- 節目（自述つき）。これがこのコマの本当の節目
        willing = case rpStep rp of { Just (Stay w) -> Just w; _ -> Nothing }
        (lgF, nF) = case enActivity (lgEngine lgE) of
          Just a | acUnintegrated a > 0 ->
            let cands = candidatesNow p lgE news [acMaterial a]
                (o, l) = apply p (StopAt willing cands) lgE
            in (l, ["停靠点：" ++ show o])
          _ -> (lgE, [])
        -- 書き置きより前の残りの時間を、これまでの決まりのまま流す（休みたいという今の気持ちを、過去に当てない）。
        -- それから気持ちに従う
        (lgG, nG) = quietPhase p (addUTCTime (minutesOf (dt - left)) (lgSyncedAt lgD)) left lgF
        (lgH, nH) = afterFeeling lgG
    in (lgH { lgSyncedAt = now }, gapNote ++ nS ++ nQ ++ nD ++ nE ++ nF ++ answerNote ++ nG ++ nH ++ unansweredNote lgH ++ ["最后读的是「" ++ materialString lastMat ++ "」"])
  where
    -- 一つの接触には観察を一つ（設計稿「数学修正」第 9 条。Codex の審査 38）。同じ材料が途切れずに続いているあいだ
    -- （このコマを起こしたときに続いていた接触の続き、あるいはこのコマの中で同じ材料が続けて出てきた）は、最後の観察を
    -- 改める：あとから来た有効な値で書き換え、来なかった欄は前の値を残す。A → B → A は、二度めの A が新しい接触
    observe cont clock lg its =
      let stepObs (obsMap, current) it
            | riMinutes it <= 0 = (obsMap, current)
            | otherwise =
                let m = riMaterial it
                    fresh = Obs clock (unitValue <$> riInterest it) (unitValue <$> riBurden it)
                    prev = Map.findWithDefault [] m obsMap
                    -- 新しい接触は、気持ちがまだなくても、自分の観察の席を一つ取る（空の席は見積もりに入らない）。
                    -- そうしないと、あとから来た気持ちが前の接触の観察を書き換えてしまう（Codex の復審 40、F3）
                    merged = case (current == Just m, reverse prev) of
                      (True, lastO : rest) -> reverse (lastO { obInterest = maybe (obInterest lastO) Just (obInterest fresh)
                                                             , obBurden = maybe (obBurden lastO) Just (obBurden fresh) } : rest)
                      _ -> prev ++ [fresh]
                in (Map.insert m merged obsMap, Just m)
          (obs', _) = foldl stepObs (lgObs lg, cont) its
          obs'' = Map.filter (not . null) obs'
          -- 引擎の材料の面白さ・負担を、いまの見積もりにそろえる（漂移の証拠もこれで計る）
          e = lgEngine lg
          mats' = Map.mapWithKey (\k ms -> case Map.lookup k obs'' of
                    Just os -> ms { msInterest = maybe (msInterest ms) clampUnit (estimateAt clock obInterest os)
                                  , msBurden = maybe (msBurden ms) clampUnit (estimateAt clock obBurden os) }
                    Nothing -> ms) (enMaterials e)
          -- inboxから手に取ったものは、入口の名前を帳簿の名前に結ぶ
          alias' = foldr (\it acc -> case riOrigin it of
                             UserInbox path -> let entry = MaterialId (T.pack ("inbox/" ++ reverse (takeWhile (/= '/') (reverse path))))
                                              in if entry /= riMaterial it then Map.insert entry (riMaterial it) acc else acc
                             _ -> acc) (lgAlias lg) its
      in lg { lgEngine = e { enMaterials = mats' }, lgObs = obs'', lgAlias = alias' }
    -- 聞いていた材料に答えを当てる。その材料にいまも関わっていて、線がまだ一度めのときだけ
    answer askedM lg = case (askedM, rpRechoice rp, pendingLine lg) of
      (Just m, Just r, Just (m', FirstCrossing)) | m == m' ->
        let (l1, n1) = settleIfNeeded p news lg
            (o, l2) = apply p (Rechoose (r == KeepGoing)) l1
        in (l2, n1 ++ ["对「" ++ materialString m ++ "」的再选：" ++ show r ++ " → " ++ show o])
      _ -> (lg, [])
    afterFeeling lg = case (rpStep rp, pendingLine lg) of
      (Just (Leave Enough), _) -> let (_, l) = apply p EndActivity lg in (l, ["够了：结掉这件"])
      (_, Just (_, SecondCrossing)) -> let (_, l) = apply p EndActivity lg in (l, ["第二次到线：结掉这件"])
      (Just (Leave RestFirst), _) -> (lg { lgRestUntil = Just (addUTCTime (minutesOf (pRRestHalf p)) now) }, ["先歇一格"])
      _ -> (lg, [])
    switchMaterial mat lg = case enActivity (lgEngine lg) of
      Just a | acMaterial a /= mat ->
        let (l1, n1) = settleIfNeeded p news lg
            (_, l2) = apply p EndActivity l1
        in (l2, n1 ++ ["换了材料，先结掉「" ++ materialString (acMaterial a) ++ "」"])
      _ -> (lg, [])
    workOne (lgA0, acc, left) w
      -- 時守が認めた分が 0 分になったものは、何もしなかったのと同じ：前の活動を終えない、知らない一篇も入れない（Codex の復審 40、F4）
      | min left (max 0 (wiMinutes w)) <= 0 = (lgA0, acc ++ ["「" ++ materialString (wiMaterial w) ++ "」没有过的时间"], left)
      | otherwise =
      let mat = wiMaterial w
          d = min left (max 0 (wiMinutes w))
          (lgA, nA) = switchMaterial mat lgA0
          (lgC, nC) = if Map.member mat (enMaterials (lgEngine lgA)) then (lgA, []) else
            let (o, l) = apply p (Introduce mat (wiOrigin w) (fromMaybe generalGuess (wiInterest w)) (fromMaybe generalGuess (wiBurden w))) lgA
            in (l, ["新的一篇「" ++ materialString mat ++ "」：" ++ show o])
          -- 強さは 0 にしない（書くのも作るのも、何もしなかったことにはならない）
          burden = maybe (Fallback (estimated generalGuess) NoAnswer) (\b -> FeltBurden (felt (clampUnit (max 0.05 (unitValue b))))) (wiBurden w)
          (lgD, nD) =
            let (o, l) = apply p (Work mat burden (clampMinutes d)) lgC
            in (l { lgSyncedAt = addUTCTime (minutesOf d) (lgSyncedAt l) }, ["写／做「" ++ materialString mat ++ "」" ++ show1 d ++ " 分：" ++ show o]
                   ++ [ "　报的 " ++ show1 (wiMinutes w) ++ " 分超过这一格的时间，只算 " ++ show1 d ++ " 分" | wiMinutes w > d + 1e-6 ])
      in (lgD, acc ++ nA ++ nC ++ nD, left - d)
    recordOne (lgA0, acc) it =
      let mat = riMaterial it
          -- 別のことを積分していないまま持っていたら、先に節目を打って終える
          (lgA, nA) = switchMaterial mat lgA0
          -- 知らない材料なら、まず知る
          (lgC, nC) = if Map.member mat (enMaterials (lgEngine lgA)) then (lgA, []) else
            let (o, l) = apply p (Introduce mat (riOrigin it) (fromMaybe generalGuess (riInterest it)) (fromMaybe generalGuess (riBurden it))) lgA
            in (l, ["新材料「" ++ materialString mat ++ "」：" ++ show o])
          burden = maybe (Fallback (estimated generalGuess) NoAnswer) (FeltBurden . felt) (riBurden it)
          (lgD, nD) = if riMinutes it <= 0 then (lgC, ["「" ++ materialString mat ++ "」没有读的量"]) else
            let (o, l) = apply p (Record mat burden (clampMinutes (riMinutes it))) lgC
                over = advanceTotal (lgEngine l) - pGridL p
            in (l, ["记下「" ++ materialString mat ++ "」" ++ show1 (riMinutes it) ++ " 分：" ++ show o]
                   ++ [ "　超前 " ++ show1 over ++ " 分，用真实时间承托完之前不起下一格" | over > 1e-6 ]
                   ++ [ "　今天读的已经超过上限（" ++ show1 (dayUsed (enFatigue (lgEngine l))) ++ " 分），照实记下"
                      | dayUsed (enFatigue (lgEngine l)) > pDayCap p ])
      in (lgD, acc ++ nA ++ nC ++ nD)
    show1 x = show (fromIntegral (round (x * 10) :: Int) / 10 :: Double)

materialString :: MaterialId -> String
materialString = T.unpack . materialText
