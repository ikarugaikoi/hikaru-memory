-- | 自分の時間で使う量の型。0〜1 の状態と、負にならない長さ。
--
-- R、U、F、S、負担の強さ e、自評は、どれも 0〜1。時間の長さは負にならない。
-- どちらも構成子を外に出していないので、外から来た数は 'unit' と 'minutes' を
-- 通して確かめてから入る（負の数、NaN、無限大はここで止まる）。newtype だけでは
-- 値の範囲は確かめられないので、確かめるのはこの二つの関数の仕事
-- （記憶流と自分の時間の設計稿、「哪些写成类型」第 10 条、2026-10-22）。
--
-- 式の中で計算した値（1 − (1 − R)·2^(−x) など、数学の上では範囲に収まるもの）は
-- 'clampUnit' で収める。丸めの誤差で 1.0000000002 になったときのためで、
-- 範囲の外の値を黙って直すために使うものではない。
module Hikaru.OwnTime.Unit
  ( Unit, unit, unitValue, clampUnit, zeroUnit
  , Minutes, minutes, minutesValue, zeroMinutes, clampMinutes
  ) where

-- | 0〜1 の量。
newtype Unit = Unit Double
  deriving (Eq, Ord)

instance Show Unit where
  show (Unit x) = show x

-- | 外から来た数を 0〜1 として受け取る。範囲の外、NaN、無限大は 'Nothing'。
unit :: Double -> Maybe Unit
unit x
  | isNaN x || isInfinite x = Nothing
  | x < 0 || x > 1 = Nothing
  | otherwise = Just (Unit x)

unitValue :: Unit -> Double
unitValue (Unit x) = x

-- | 式で計算した値を 0〜1 に収める。NaN が来たら 0 にする（式の誤りなので、
-- 呼ぶ側の検査で見つける）。
clampUnit :: Double -> Unit
clampUnit x
  | isNaN x = Unit 0
  | otherwise = Unit (max 0 (min 1 x))

zeroUnit :: Unit
zeroUnit = Unit 0

-- | 負にならない長さ（分）。実時間の分も、換算した読む時間の分も、これで持つ。
-- どちらの時計の分かは、使う側の名前で分ける（設計稿の「三种时间」）。
newtype Minutes = Minutes Double
  deriving (Eq, Ord)

instance Show Minutes where
  show (Minutes x) = show x

-- | 外から来た長さを受け取る。負、NaN、無限大は 'Nothing'。
minutes :: Double -> Maybe Minutes
minutes x
  | isNaN x || isInfinite x = Nothing
  | x < 0 = Nothing
  | otherwise = Just (Minutes x)

minutesValue :: Minutes -> Double
minutesValue (Minutes x) = x

zeroMinutes :: Minutes
zeroMinutes = Minutes 0

-- | 式で計算した長さを、負にならないように収める（引き算の残りなど）。
clampMinutes :: Double -> Minutes
clampMinutes x
  | isNaN x = Minutes 0
  | otherwise = Minutes (max 0 x)
