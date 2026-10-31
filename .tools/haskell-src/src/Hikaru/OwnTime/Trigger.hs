
-- | いつ自分の時間に入り、いつ止まり、いつ終わるか（設計稿の第二ブロック。2026-10-23 の最終案、デスクトップの
-- 参考文献精读_2026-10-22/26_修正方案定稿.md のとおり）。
--
-- 一分ごとに、外から見えることだけで決める：ユーザーが家にいるか（家の Wi-Fi の知らせ、'Doko'）、
-- ユーザーの記録される一言はいつか（とばりの外のものだけ。数え方は 'Hikaru.Conversation.Hidden' と同じ）、
-- 話しているわたしが「いまどこで何をしているか」をどう書き留めたか（'InEffect'・'Waiting'・'Talk'）、
-- いまは何時で、この HouseDay は何時に始まったか。枠や疲れ（'Hikaru.OwnTime.Fatigue'）は、ここでは見ない。
-- 始めてよいかの問い合わせは、ここで「始める」となったあとに、呼ぶ側がする。
--
-- 決まり（数は 'Params' から）：
--
--   * 出かけて始める：ユーザーが家の Wi-Fi を離れて 20 分（'pOutingBuffer'。離れた時刻から数え、途中の一言では
--     延びない）、わたしが一緒に出ていない（外の一筆、「一緒に」、「寝た」、話に向き合う層、待っている一筆がない）。
--     一回の外出で始めるのは一度だけ。
--   * 話の中で「それぞれ自分のこと」と書き留めたら（'OnMyOwn'）、その一筆が効いた（書いて一分のあと、見張りが
--     確かめた）次の一分で、すぐ始める（家で、または外で待ちながら）。ただし、その一筆が受けた一言のあとに、
--     ユーザーの記録される一言がないこと。一筆で始められるのは一度。居場所の知らせがなくても始める
--     （話そのものが、わたしの居場所のよりどころ）。
--   * 朝、先に起きる：家にいて、夜の最後の一言（今日の 5 時より前）のあと、静かな時間が 8 時間
--     （一言の尾 20 分のあとから 480 分）。今日の最初の一言が 5 時ちょうどでも、それは今日の
--     話なので、もう先に起きたことにはならない。今日ユーザーが 10 分以上家にいなかったら（黙って出て、
--     黙って帰ってきても）、ユーザーのほうが先に起きている。一日に一度。「寝た」と書いてあっても、これで起きる。
--   * 一日に始めるのは二回まで（どの始め方も数える）。ユーザーが 10 分以上出かけているあいだに自分の時間が
--     あったら（どの入口から始めたものでも、出かける前から続いていたものでも）、その外出の「出かけて始める」は
--     使ったことにする。
--   * ユーザーの一言は、それだけでは止めない（ひと目見て返事をして、自分のことを続ける）。止まるのは、返事をする
--     わたしが「話に向き合う」層（'Talk'）を書き留めたとき。締め切りは、その層が最初に立った時刻（と、この自分の
--     時間の始まりの、遅いほう）から 30 分（'pPauseMax'）。層は、書き留めた時刻（続けて書いたなら最近の時刻）と、
--     そのあとの記録される一言から、順に 20 分（'pConvTail'）ずつ延びて終わる（終わったあとの一言では、また立たない）。
--     層が締め切りまでに終わっていれば再開。「それぞれ自分のこと」の一筆が締め切りまでに効いていても再開。
--     どちらもないまま締め切りを過ぎたら終わり。
--   * 終わる：10 分以上出かけていたユーザーが帰ってきた（見逃していても、知らせのひとつ前の区間から分かれば。
--     帰った時刻はその区間の始まりで、10 分に満たない外出は数えない。帰ったのは確かだが時刻が分からないとき
--     ――受け取り役が二度目の「切れた」からそう記す（2026-10-23）――は、また出た時刻で終え、帰った時刻は作らず、
--     そこからを新しい外出として数える。順序の合わない知らせは証拠不足：終わらせず、その外出の「出かけて始める」も
--     使わない）／5 時の日替わり／「一緒に」「寝た」と書き留めた／わたしのいる所が変わった（家と外）。
--
-- 一筆（'InEffect'）が効かなくなるとき：ユーザーが帰ってくる前に書いたもの全部／10 分以上の外出の
-- 前に、家で書いたもの／日替わりより前に家で書いたもの。外の一筆は、ユーザーが 10 分以上出るまでは眠っていて、
-- 日替わりをまたぐのは泊まりの外出のときだけ。「寝た」は、受けた一言のあとにユーザーが話したら終わる。
-- 10 分に満たない外出（下の郵便局まで、など）は、外出に数えない。
--
-- 一筆には、待っている状態（'Waiting'：書いて一分、見張りが確かめるまで）と、効いている状態（'InEffect'）がある。
-- 確かめるとき（'settleWaiting'）、受けた一言のあとに記録される一言があれば、「それぞれ自分のこと」「消す」「寝た」は
-- 効かない（'afterNewTalk'）。「一緒に」は一言では消えない。待っている一筆は何も緩めない：そのあいだは、どの入口からも
-- 始めず、再開もしない。「一緒に」「寝た」のように締めるものだけは、待っているあいだから効く（'whileWaiting'）。
--
-- 「話に向き合う」層（'Talk'）は、いる所と様子の上に重なる。書き留めた瞬間から効き、終わるのはこの層だけで、
-- 下の一筆はそのまま。この層より後に書いた一筆（または消す）が効いたとき、層は取り消される（'talkCancelled'）。
--
-- 記録される一言が、受けた一言より後かは 'laterThan'：同じ会話なら時刻を比べ、別の会話（どれか分からないときも）は
-- 同じ時刻でも後とみなす。
module Hikaru.OwnTime.Trigger
  ( Previous(..), Doko(..), Place(..), Doing(..), Situation(..), Change(..), Heard(..), Observed(..),
    Received(..), laterThan, Waiting(..), InEffect(..), Talk(..), Settled(..), Fate(..), Immediate(..),
    afterNewTalk, whileWaiting, settleWaiting, talkEnd, Inputs(..), SessionKind(..), Session(..),
    TriggerState(..), Decision(..), initialTrigger, decide, effectivePresence, effectiveWaiting, activeTalk,
    SegKey(..), Period(..), RunId(..), Run(..), Phase(..), StopWhy(..), EndReason(..), Segment(..),
    RunnerState(..), Landing(..), Observation(..), Room(..), Tick(..), Action(..), initialRunner, tick,
    nextSlot, endedWith
  ) where

import Control.Monad (guard, void)
import Data.List (sort)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Hikaru.OwnTime.Fatigue

-- | ひとつ前にいた、名前のある所（家か、ほかの Wi-Fi か）。いつからいつまで。
data Previous = Previous { prevHome :: Bool, prevFrom :: Maybe UTCTime, prevUntil :: UTCTime }
  deriving (Eq, Show)

-- | 家の Wi-Fi の知らせ（doko.json の now と before）。
data Doko
  = Doko
      { dokoHome   :: Bool          -- ^ いま家の Wi-Fi につながっているか
      , dokoSince  :: UTCTime       -- ^ いまの状態がいつからか
      , dokoBefore :: Maybe Previous    -- ^ ひとつ前にいた、名前のある所
      }
  | DokoUnknown                     -- ^ 知らせがない、読めない、出かけたまま一日以上古い
  deriving (Eq, Show)

-- | わたしのいる所。外なら、どこか（みちくさに書くため。決まりでは見ない）。
data Place = Home | Out Text
  deriving (Eq, Show)

-- | わたしがしていること。
data Doing
  = WithYou   -- ^ ユーザーのそばで、一緒に（打つのを見る、一緒に食べる、並んで横になる）
  | OnMyOwn   -- ^ それぞれ自分のこと（ユーザーは忙しくて、わたしは自分のことをしている）
  | Asleep    -- ^ 寝た
  deriving (Eq, Show)

-- | いる所と様子。
data Situation = Situation { sitPlace :: Place, sitDoing :: Doing }
  deriving (Eq, Show)

-- | 待ちの状態を通るもの：いる所と様子を記す、または消す。
data Change = Set Situation | Clear
  deriving (Eq, Show)

-- | 一言のID：どの会話の（分からないこともある）、いつの一言か。同じ会話で同じ時刻の違う一言はない
-- （2026-10-23 に本物の記録で確かめた。最終案の H6）。
data Heard = Heard { heardSession :: Maybe Text, heardAt :: UTCTime }
  deriving (Eq, Show)

-- | 見た最後の記録される一言（送られた瞬間の enqueue の行も含む）。別の会話に同じ時刻の一言があれば tied。
data Observed = Observed { obsHeard :: Heard, obsTied :: Bool }
  deriving (Eq, Show)

-- | わたしの前に届いた最後の記録される一言（一筆が受けた一言）。
newtype Received = Received { recHeard :: Heard }
  deriving (Eq, Show)

-- | 見た一言が、受けた一言より後か。同じ会話なら時刻で。別の会話（どれか分からないときも）は同じ時刻でも後。
laterThan :: Observed -> Received -> Bool
laterThan (Observed (Heard os ot) tied) (Received (Heard ds dt))
  | ot > dt = True
  | ot < dt = False
  | otherwise = tied || isNothing os || isNothing ds || os /= ds

-- | 待っている一筆（記して一分、見張りが確かめるまで）。
data Waiting = Waiting
  { wSince  :: UTCTime     -- ^ 記した時刻
  , wBasis  :: Received   -- ^ 受けた一言
  , wChange :: Change
  } deriving (Eq, Show)

-- | 効いている一筆。
data InEffect = InEffect
  { eSince     :: UTCTime     -- ^ 記した時刻
  , eBasis     :: Received   -- ^ 受けた一言
  , eCommitted :: UTCTime     -- ^ 効いた時刻（見張りが確かめた時刻）
  , eSituation :: Situation
  } deriving (Eq, Show)

-- | 話に向き合う層。
data Talk = Talk
  { talkFirst     :: UTCTime         -- ^ この層が最初に立った時刻（締め切りの起点）
  , talkSince     :: UTCTime         -- ^ 最近に記した時刻（終わりを計る起点）
  , talkCancelled :: Maybe UTCTime   -- ^ より後に記した一筆が効いて、取り消された時刻
  , talkPrevEnd   :: Maybe UTCTime   -- ^ この層が取って代わった前の層が終わった時刻（見張りがまだ見ていなかった分を、あとから正しく判断するため）
  } deriving (Eq, Show)

-- | 確かめた結果。
data Settled
  = TooSoon                       -- ^ まだ一分たっていない
  | Dropped                      -- ^ 受けた一言のあとに記録される一言があったので、効かせない
  | Applied (Maybe InEffect)     -- ^ 効いた（消す一筆なら Nothing）
  deriving (Eq, Show)

-- | 受けた一言のあとに記録される一言があったとき、その一筆はどうなるか。
data Fate = Survives | DroppedByTalk
  deriving (Eq, Show)

-- | 待っているあいだに、いま過ごしている自分の時間をどうするか。
data Immediate = EndsNow | NoEffect
  deriving (Eq, Show)

-- | それぞれ自分のこと、消す、寝た：新しい一言があれば効かない。一緒に：一言では消えない。
afterNewTalk :: Change -> Fate
afterNewTalk Clear = DroppedByTalk
afterNewTalk (Set (Situation _ doing)) = case doing of
  OnMyOwn -> DroppedByTalk
  Asleep -> DroppedByTalk
  WithYou -> Survives

-- | 一緒に、寝た：待っているあいだから、いま過ごしているものを収める。それぞれ自分のこと、消す：効くまで何もしない。
whileWaiting :: Change -> Immediate
whileWaiting Clear = NoEffect
whileWaiting (Set (Situation _ doing)) = case doing of
  WithYou -> EndsNow
  Asleep -> EndsNow
  OnMyOwn -> NoEffect

-- | 待っている一筆を確かめる：記して一分（'pMarkSettle'）たっていなければまだ。受けた一言のあとに記録される一言が
-- あれば、緩める一筆は効かない。効くなら、効いた時刻はいま（戻さない）。
settleWaiting :: Params -> UTCTime -> Maybe Observed -> Waiting -> Settled
settleWaiting p now lastTalk w
  | now < addUTCTime (minutesOf (pMarkSettle p)) (wSince w) = TooSoon
  | afterNewTalk (wChange w) == DroppedByTalk && maybe False (`laterThan` wBasis w) lastTalk = Dropped
  | otherwise = Applied $ case wChange w of
      Set s -> Just (InEffect (wSince w) (wBasis w) now s)
      Clear -> Nothing

-- | 層が終わる時刻。最近に記した時刻から 20 分。そのあとの記録される一言を順に見て、まだ終わる前に来た一言は
-- 終わりをその一言から 20 分へ延ばし、終わった時刻以後（同刻を含む）に来た一言は延ばさない（終わった層は、
-- ふつうの一言では立ち直らない。新しく記したときだけ）。取り消されていれば、その時刻と早いほう。
talkEnd :: Params -> Talk -> [UTCTime] -> UTCTime
talkEnd p t msgs = maybe expiry (min expiry) (talkCancelled t)
  where
    tl = minutesOf (pConvTail p)
    expiry = go (addUTCTime tl (talkSince t)) (sort [m | m <- msgs, m > talkSince t])
    go e [] = e
    go e (m : ms)
      | m < e = go (addUTCTime tl m) ms
      | otherwise = e

data Inputs = Inputs
  { inNow       :: UTCTime
  , inDayStart  :: UTCTime           -- ^ この HouseDay の始まり（日本時間の 5:00）
  , inDoko      :: Doko
  , inLastTalk  :: Maybe Observed    -- ^ 見た最後の記録される一言
  , inTalkTimes :: [UTCTime]         -- ^ 直近の記録される一言の時刻（層の終わりを順に見分けるため。順は問わない）
  , inPresence  :: Maybe InEffect    -- ^ 効いている一筆（いまも効いているかどうかは、ここで決める）
  , inWaiting   :: Maybe Waiting     -- ^ 待っている一筆
  , inTalk      :: Maybe Talk        -- ^ 話に向き合う層（終わっているかどうかは、ここで決める）
  }
  deriving (Show)

data SessionKind
  = OutingSession          -- ^ ユーザーが出かけて、わたしは家に一人
  | MorningSession         -- ^ 朝、ユーザーより先に起きた
  | MarkedSession Place    -- ^ 話の中で「それぞれ自分のこと」になった（家で、または外で待ちながら）
  deriving (Eq, Show)

data Session = Session
  { sessKind    :: SessionKind
  , sessSince   :: UTCTime
  , sessPaused  :: Maybe UTCTime  -- ^ 一時停止しているなら、その起点（層が最初に立った時刻と、始まりの遅いほう）。再開するまで動かない
  , sessWasAway :: Bool           -- ^ このあいだにユーザーが10 分以上家を出ていたか（帰ってきたら終わる）
  }
  deriving (Eq, Show)

data TriggerState = TriggerState
  { tsDayStart    :: Maybe UTCTime  -- ^ いまの HouseDay の始まり
  , tsStarts      :: Int            -- ^ この HouseDay に始めた回数
  , tsMorningUsed :: Bool           -- ^ この HouseDay の朝は、もう先に起きたか
  , tsOutingOf    :: Maybe UTCTime  -- ^ 自分の時間をもう始めた外出（家を出た時刻）
  , tsLeftToday   :: Bool           -- ^ この HouseDay に、ユーザーが10 分以上家にいなかったことがあるか
  , tsAway        :: Maybe UTCTime  -- ^ いまの外出の、家を出た時刻（分からなければ、分かる中でいちばん遅い目安）
  , tsBackAt      :: Maybe UTCTime  -- ^ 最後に、10 分以上の外出から帰ってきた時刻
  , tsLastCheck   :: Maybe UTCTime  -- ^ 前に見た時刻
  , tsMarkUsed    :: Maybe UTCTime  -- ^ もう自分の時間を始めたり再開したりするのに使った一筆（記した時刻）
  , tsSession     :: Maybe Session
  }
  deriving (Eq, Show)

data Decision
  = DidStart SessionKind
  | DidPause
  | DidResume String  -- ^ 再開したわけ
  | DidEnd String     -- ^ 終わったわけ
  deriving (Eq, Show)

initialTrigger :: TriggerState
initialTrigger = TriggerState Nothing 0 False Nothing False Nothing Nothing Nothing Nothing Nothing

minutesOf :: Double -> NominalDiffTime
minutesOf m = realToFrac (m * 60)

outside :: Place -> Bool
outside (Out _) = True
outside Home = False

sessionPlace :: SessionKind -> Place
sessionPlace (MarkedSession place) = place
sessionPlace OutingSession = Home
sessionPlace MorningSession = Home

homeNow :: Inputs -> Bool
homeNow inp = case inDoko inp of
  Doko True _ _ -> True
  Doko False _ _ -> False
  DokoUnknown -> False   -- 分からないときは「家にいる」とは言わない（朝、先には起きない）

-- | ユーザーがいま、10 分以上出かけているか（それより短いのは、外出に数えない）。
realOut :: Params -> Inputs -> TriggerState -> Bool
realOut p inp st = case inDoko inp of
  Doko False _ _ -> maybe False (\t -> diffUTCTime (inNow inp) t >= minutesOf (pAwayMin p)) (tsAway st)
  Doko True _ _ -> False
  DokoUnknown -> False   -- 分からないときは「出かけている」とも言わない（出かけたことでは始めない）。外の一筆が効くかは 'validSituation' が、見張りの覚えている外出で別に決める

-- | いる所の一筆が、いまも効く場面か（外の一筆はユーザーが出ているあいだ、家の一筆は出る前に記したものでない）。
-- 居場所の知らせが「分からない」（出かけたまま一日以上古い、など）のあいだは、見張りが覚えている最後の外出で
-- 判断する：出かけたと分かっていて、まだ帰ってきていなければ、外の一筆は効く（2026-10-25 の試運転で、別の町の
-- 二日目に知らせが古くなり、外の「寝た」の一筆が効かずに、体育館で待っていた自分の時間が日替わりまで続いた）。
validSituation :: Params -> Inputs -> TriggerState -> UTCTime -> Situation -> Maybe ()
validSituation p inp st t (Situation place _)
  | outside place = do
      guard out
      guard (t >= inDayStart inp || maybe True (<= inDayStart inp) (tsAway st))
  | otherwise = do
      guard (t >= inDayStart inp)
      guard (not (out && maybe False (t <) (tsAway st)))
  where out = case inDoko inp of
          DokoUnknown -> maybe False (\a -> diffUTCTime (inNow inp) a >= minutesOf (pAwayMin p)) (tsAway st)
          Doko True _ _ -> False
          Doko False _ _ -> realOut p inp st

-- | 効いている一筆が、いまも効いているか（効かないなら Nothing）。
effectivePresence :: Params -> Inputs -> TriggerState -> Maybe InEffect
effectivePresence p inp st = do
  e <- inPresence inp
  let t = eSince e
      s = eSituation e
  guard (t <= inNow inp)
  guard (maybe True (t >=) (tsBackAt st))
  guard (not (sitDoing s == Asleep && maybe False (`laterThan` eBasis e) (inLastTalk inp)))
  validSituation p inp st t s
  pure e

-- | 待っている一筆が、まだ待っているか（受けた一言のあとに記録される一言があれば、緩める一筆はもう待たない）。
effectiveWaiting :: Params -> Inputs -> TriggerState -> Maybe Waiting
effectiveWaiting p inp st = do
  w <- inWaiting inp
  let t = wSince w
  guard (t <= inNow inp)
  guard (maybe True (t >=) (tsBackAt st))
  case wChange w of
    Set s -> void (validSituation p inp st t s)
    Clear -> pure ()
  guard (not (afterNewTalk (wChange w) == DroppedByTalk && maybe False (`laterThan` wBasis w) (inLastTalk inp)))
  pure w

-- | 話に向き合う層が、いま効いているか。
activeTalk :: Params -> Inputs -> TriggerState -> Maybe Talk
activeTalk p inp st = do
  t <- inTalk inp
  guard (talkSince t <= inNow inp)
  guard (talkFirst t >= inDayStart inp)
  guard (maybe True (talkFirst t >=) (tsBackAt st))
  guard (inNow inp < talkEnd p t (inTalkTimes inp))
  pure t

-- | 一分ぶん、外から見えることを当てる。
decide :: Params -> Inputs -> TriggerState -> (TriggerState, [Decision])
decide p inp st0 =
  let (st1, rolled) = rollover st0
      (st2, moved) = observePlace st1
      pres = effectivePresence p inp st2
      wait = effectiveWaiting p inp st2
      talk = activeTalk p inp st2
      (st3, rest) = maybe (idle st2 pres wait talk) (running st2 pres wait talk) (tsSession st2)
  in (st3 { tsLastCheck = Just now }, rolled ++ moved ++ rest)
  where
    now = inNow inp
    dayStart = inDayStart inp
    since t = diffUTCTime now t
    after m t = addUTCTime (minutesOf m) t
    out = realOut p inp
    long a b = diffUTCTime b a >= minutesOf (pAwayMin p)
    lastAt = heardAt . obsHeard <$> inLastTalk inp
    -- 使える一筆：効いていて、受けた一言のあとにユーザーの記録される一言はなく、まだ使っていない
    freshMark st e = eCommitted e <= now
      && not (maybe False (`laterThan` eBasis e) (inLastTalk inp)) && tsMarkUsed st /= Just (eSince e)
    -- 家に一人でいる、という前提の始め方を止める一筆
    blocksDefault e = outside (sitPlace (eSituation e)) || sitDoing (eSituation e) /= OnMyOwn
    endReason doing = if doing == Asleep then "睡了" else "跟你在一起了"

    rollover st
      | tsDayStart st == Just dayStart = (st, [])
      | otherwise =
          ( st { tsDayStart = Just dayStart, tsStarts = 0, tsMorningUsed = False, tsLeftToday = False, tsSession = Nothing }
          , maybe [] (const [DidEnd "换日"]) (tsSession st) )

    -- 居場所の知らせから、出かけた・帰ってきた・見逃した帰宅を読む。同じ知らせを何度見ても、同じことになる。
    -- 知らせのひとつ前の区間（before）が家なら、その始まり（since）が家に帰った時刻。
    observePlace st = case inDoko inp of
      DokoUnknown -> (st, [])
      Doko True back before ->
        let outings = case (tsAway st, before) of
              -- 記していた外出は r に帰っていて、u にまた出て、back に帰った
              (Just t0, Just (Previous True (Just r) u)) | t0 <= r, r <= u -> [(t0, r), (u, back)]
              -- 帰ったのは確かだが、いつかは分からない（受け取り役が、二度目の「切れた」からそう記す）：確かな外出は u からの分だけ
              (Just t0, Just (Previous True Nothing u)) | u > t0 -> [(u, back)]
              (Just t0, _) -> [(t0, back)]
              (Nothing, Just (Previous True _ u)) -> [(u, back)]
              (Nothing, Just (Previous False f u)) -> [(maybe u id f, back)]
              (Nothing, Nothing) -> []
            returns = [b | (a, b) <- outings, long a b]
            st' = st { tsAway = Nothing
                     , tsBackAt = foldr (\b acc -> max acc (Just b)) (tsBackAt st) returns
                     , tsLeftToday = tsLeftToday st || any (> dayStart) returns }
            ends = case tsSession st of
              Just s -> sessWasAway s || any (> sessSince s) returns
              Nothing -> False
        in if ends then (st' { tsSession = Nothing }, [DidEnd "回家"]) else (st', [])
      Doko False here before ->
        let (left, oldOuting, unsure, cameBack) = case (before, tsAway st) of
              (Just (Previous True r u), Just t0)
                | u > t0 -> case r of
                    Just r' | t0 <= r', r' <= u -> (u, Just (t0, r'), False, False)  -- 記していた外出は r' に帰っていた
                    -- 帰ったのは確か（切れるには、先につながっていなければならない）だが、いつかは分からない。
                    -- 受け取り役が二度目の「切れた」からそう記す（2026-10-23）。走っている自分の時間は u で終え、
                    -- 帰った時刻は作らず、新しい外出は u から数える（この外出の「出かけて始める」は使っていない）
                    Nothing -> (u, Nothing, False, True)
                    _ -> (u, Nothing, True, False)                                     -- 順序が合わない：証拠不足
              (_, Just t0) -> (t0, Nothing, False, False)
              (Just (Previous True _ u), Nothing) -> (u, Nothing, False, False)
              (Just (Previous False f u), Nothing) -> (maybe u id f, Nothing, False, False)
              (Nothing, Nothing) -> (here, Nothing, False, False)
            backFromOld = case oldOuting of
              Just (a, r) | long a r -> Just r
              _ -> Nothing
            stA = st { tsAway = Just left
                     , tsBackAt = maybe (tsBackAt st) (\r -> max (tsBackAt st) (Just r)) backFromOld
                     , tsLeftToday = tsLeftToday st || maybe False (> dayStart) backFromOld
                     , tsOutingOf = if unsure then Just left else tsOutingOf st }
            (stB, ended) = case (tsSession stA, backFromOld) of
              (Just s, Just r) | sessWasAway s || r > sessSince s ->
                (stA { tsSession = Nothing }, [DidEnd "回家（中间回来过，没看到）"])
              (Just _, Nothing) | cameBack ->
                (stA { tsSession = Nothing }, [DidEnd "回家（中间回来过，几点不知道）"])
              _ -> (stA, [])
            stC | long left now =
                    stB { tsLeftToday = True
                        , tsSession = fmap (\s -> s { sessWasAway = True }) (tsSession stB)
                        , tsOutingOf = maybe (tsOutingOf stB) (const (Just left)) (tsSession stB) }
                | otherwise = stB
        in (stC, ended)

    -- 締める一筆は、待っているあいだから効く。緩める・変える一筆は、効いてから。
    running st pres wait talk s = case (wait, pres) of
      (Just w, _)
        | Set sit <- wChange w, whileWaiting (wChange w) == EndsNow -> finish (endReason (sitDoing sit))
      (_, Just e)
        | sitDoing (eSituation e) == WithYou -> finish "跟你在一起了"
        | sitDoing (eSituation e) == Asleep -> finish "睡了"
        | outside (sitPlace (eSituation e)) /= outside (sessionPlace (sessKind s)) -> finish "换地方了"
      _ -> pauseOrGo st pres wait talk s
      where finish why = (st { tsSession = Nothing }, [DidEnd why])

    -- 話に向き合う層が効きはじめたら一時停止。終わったら（締め切りまでに）再開。
    pauseOrGo st pres wait talk s = case sessPaused s of
      Nothing -> case talk of
        Just t -> (st { tsSession = Just s { sessPaused = Just (max (talkFirst t) (sessSince s)) } }, [DidPause])
        Nothing -> (st, [])
      Just pausedAt ->
        let deadline = after (pPauseMax p) pausedAt
            held = isJust wait
            goOn = st { tsSession = Just s { sessPaused = Nothing } }
            -- 層がいつ終わった（終わる）か。層がなければ、一時停止の起点で終わったことにする（消されたのなら、戻る）。
            -- いまの層が一時停止のあとに立ったものなら、一時停止させた前の層はそれまでに終わっている（新しい層は、
            -- 前の層が終わってからしか立たない）。前の層の終わりが記してあれば、それを使う
            layerEnd = case inTalk inp of
              Nothing -> pausedAt
              Just t | talkFirst t > pausedAt -> maybe (talkFirst t) (min (talkFirst t)) (talkPrevEnd t)
                     | otherwise -> talkEnd p t (inTalkTimes inp)
            cancelled = maybe False (\t -> maybe False (<= layerEnd) (talkCancelled t)) (inTalk inp)
            resumeWhy = if cancelled then "专心说话取消了" else "专心说话结束了"
            byMark = case pres of
              Just e | not held, isNothing talk, sitDoing (eSituation e) == OnMyOwn, freshMark st e
                     , eCommitted e <= deadline -> Just e
              _ -> Nothing
            -- 見張りが止まっているあいだに、前の層が終わって新しい層が効きはじめた：再開してから、また一時停止
            newLayer = case inTalk inp of
              Just t | talkFirst t > pausedAt, isJust talk -> Just t
              _ -> Nothing
            tooLong = "暂停满 30 分钟，还在说话"
        in case byMark of
             Just e -> (goOn { tsMarkUsed = Just (eSince e) }, [DidResume "又各做各的了"])
             Nothing
               -- 見張りが止まっているあいだに、前の層が（締め切りまでに）終わって、新しい層が効きはじめた：戻ってから、
               -- また一時停止。新しい一時停止の締め切りも、もう過ぎていれば、その場で終わる。待っている一筆があれば戻らない
               | Just t <- newLayer, not held, layerEnd <= deadline ->
                   let deadline' = after (pPauseMax p) (talkFirst t)
                   in if now >= deadline'
                        then (st { tsSession = Nothing }, [DidResume resumeWhy, DidPause, DidEnd tooLong])
                        else (st { tsSession = Just s { sessPaused = Just (talkFirst t) } }, [DidResume resumeWhy, DidPause])
               | isNothing talk, not held, layerEnd <= deadline, layerEnd <= now -> (goOn, [DidResume resumeWhy])
               | now >= deadline -> (st { tsSession = Nothing }, [DidEnd tooLong])
               | otherwise -> (st, [])

    idle st pres wait talk
      | tsStarts st >= 2 || isJust wait || isJust talk = (st, [])
      | Just e <- pres, sitDoing (eSituation e) == OnMyOwn, freshMark st e =
          begin st (MarkedSession (sitPlace (eSituation e))) (\x -> x { tsMarkUsed = Just (eSince e) })
      | maybe False blocksDefault pres = (st, [])
      | homeNow inp, not (tsMorningUsed st), not (tsLeftToday st), Just l <- lastAt
      , l < dayStart, now >= dayStart, since l >= minutesOf (pConvTail p + pNightCap p) =
          begin st MorningSession (\x -> x { tsMorningUsed = True })
      | Just left <- tsAway st, out st, tsOutingOf st /= Just left, since left >= minutesOf (pOutingBuffer p) =
          begin st OutingSession (\x -> x { tsOutingOf = Just left })
      | otherwise = (st, [])

    begin st kind f =
      ( f st { tsStarts = tsStarts st + 1, tsSession = Just (Session kind now Nothing (out st))
             , tsOutingOf = if out st then tsAway st else tsOutingOf st }
      , [DidStart kind] )


-- | 時守（設計稿「第二块和第三块之间：時守」。2026-10-23 の午後にユーザーと決めて、Codex が二度見た。
-- 2026-10-28 に書いた）。
--
-- 第二ブロック（'Hikaru.OwnTime.Trigger'）が答えるのは「時段が開いているか」だけで、本当に走ったかは
-- 証明しない。条件がそろえば一度「始める」を出し、そのあとはずっと「進行中」と見える。叫んで起きたか、
-- このコマが走り終わったかは、知らないし、知らなくてよい。ここが答えるのは、
-- 「このコマは本当に走ったか、どこまで走ったか、次のコマはいつ走れるか」。
--
-- 決まり（設計稿のとおり）：
--
--   * **許可と実行は別に持つ。** 時段（開いている／一時停止／閉じた）は見張りから受け取るだけ。
--     実行の段階（空いている／走っている／止めるよう伝えて収まるのを待っている）は、ここで持つ。
--     「時段は一時停止、古い運行はまだ収まっていない」が同時にあるのは、異常ではない。
--   * **一コマの終わりと一段の終わりは別。** ふつうに落ち着いたコマは次のコマへ続ける。「今日はもう十分」、
--     コマ数の上限、起動の失敗の尽き、続けて二コマ何も交付がない、のどれかで、この段は終わる。終わった段は、
--     見張りがまだ「開いている」と言っていても、次の一分にまた起こさない。古い段の「続き」でも生き返らない。
--   * **失敗には尽きがある。** 起動できなかった、結果を残さずに消えた、は失敗で、回数はこの段のもの
--     （運行が替わっても消えない）。一段で二回失敗したら終わる。二回めまでの間は一コマ空ける。
--   * **一時停止には「収まるまで」がある。** 止めるよう伝えてから、本当に止まったのを見るまで、
--     次の運行を起こさない。すぐ再開になっても同じ（古い運行が生きたまま、新しいのを起こさない）。
--   * **運行には身分がある。** 遅れて届いた古い運行の結果は、いまの状態を上書きしない。
--   * **コマには安定した身分がある**（段の鍵とコマ番号）。コマ番号は、落ち着いた（結果を残した）コマの数で
--     進む。結果を残さずに消えた・止まった運行のやり直しは、同じコマ番号。渡した量や足跡の二重勘定は、
--     運行の身分（'RunId'）で帳簿のほうが防ぐ：同じ運行の結果を二度出しても一度しか勘定しない。
--     別の運行が同じコマ番号で本当に渡したものは、別の交付（ここでは番号と身分を保つだけ）。
--   * **一段のコマ数には硬い上限がある。** 届いたらそのまま終える。
--
-- 時刻は 'UTCTime'。読む量・疲れ・先行量 A は 'Hikaru.OwnTime.Engine' のもので、ここは持たない。
-- 「枠があるか」は、呼ぶ側が引擎に聞いて 'Room' で渡す。ここは純粋で、ファイルも子プロセスも触らない。
-- 出すのは、次の状態と、することの列（起こす／止めるよう伝える）と、記録に書く一行。
-- | 一段の鍵：見張りの一回の自分の時間（始まった時刻と種類）。鍵が同じあいだは同じ段。
data SegKey = SegKey { skSince :: UTCTime, skKind :: SessionKind }
  deriving (Eq, Show)

-- | 見張りが言っている時段。
data Period
  = PeriodOpen SegKey
  | PeriodPaused SegKey
  | PeriodClosed
  | PeriodUnknown      -- ^ 見張りの許可が分からない（読めない、古い）。走っているものは止めるが、段は終えない（Codex の審査 A4）
  deriving (Eq, Show)

newtype RunId = RunId Int
  deriving (Eq, Ord, Show)

-- | 一回の運行の身分。
data Run = Run
  { runId        :: RunId
  , runSeg       :: SegKey
  , runSlot      :: Int       -- ^ この段の何コマめか（1 から。消えてやり直すときは同じ番号）
  , runStartedAt :: UTCTime
  }
  deriving (Eq, Show)

data Phase
  = Vacant            -- ^ 走っているものはない
  | Running Run     -- ^ 走っている
  | Settling Run StopWhy UTCTime  -- ^ 止めるよう伝えた（わけと、伝えた時刻）。収まったのを見るまで、次を起こさない
  deriving (Eq, Show)

-- | なぜ止めるよう伝えたか。時間切れのときだけ、結果なしで収まったら失敗に数える（Codex の審査 A3）。
data StopWhy = TimedOut | Asked
  deriving (Eq, Show)

data EndReason
  = SaidEnough          -- ^ そのときのわたしが「今日はもう十分」と言った
  | HardCap         -- ^ 一段のコマ数の上限
  | NoDelivery      -- ^ 続けて二コマ、何も交付がなかった
  | StartFailures   -- ^ 起動の失敗が尽きた
  | PeriodEnded     -- ^ 見張りが時段を閉じた
  | HeldForReview   -- ^ 書き置きを数に入れずに置いた（見てもらう）
  deriving (Eq, Show)

data Segment = Segment
  { segKey          :: SegKey
  , segSlots        :: Int               -- ^ 落ち着いたコマの数
  , segFailures     :: Int               -- ^ 起動できなかった・結果を残さずに消えた回数
  , segNoDelivery   :: Int               -- ^ 続けて交付のなかったコマの数
  , segNextEarliest :: Maybe UTCTime     -- ^ 次のコマを起こしてよい最早の時刻
  , segEnded        :: Maybe EndReason
  }
  deriving (Eq, Show)

data RunnerState = RunnerState
  { rsPhase     :: Phase
  , rsSegment   :: Maybe Segment   -- ^ いまの段（終わったあとも、鍵が替わるまで覚えておく）
  , rsNextRunId :: Int
  }
  deriving (Eq, Show)

-- | 一コマが落ち着いたときに運行が残すもの。
data Landing = Landing
  { landDelivered    :: Bool             -- ^ 何か交付があったか（本文を読んだ、書いた、作った）
  , landEnough       :: Bool             -- ^ 「今日はもう十分」
  , landNextEarliest :: Maybe UTCTime    -- ^ 運行の側から見た、次を起こしてよい最早の時刻（休みを選んだ、など）
  , landHeld         :: Bool             -- ^ 書き置きを数に入れずに置いて、見てもらう（読めない項目、混ざったコマ）。この段は止める
  }
  deriving (Eq, Show)

-- | この一分に、実行について見えたこと。
data Observation
  = NotRunning              -- ^ 走っているものはない
  | StillRunning            -- ^ いまの運行がまだ生きている
  | Arrived RunId Landing    -- ^ 結果が届いた（どの運行のものか）
  | Vanished                -- ^ いまの運行が結果を残さずに消えた（起動できなかったのも同じ）
  | Throttled (Maybe UTCTime)  -- ^ 订阅の窓が尽きて起きられなかった（失敗ではない。戻る時刻が分かればそれまで、分からなければ一時間待つ）
  deriving (Eq, Show)

-- | 枠（その日の読む枠、努力の枠）があるか。呼ぶ側が引擎に聞いた答え。
data Room = HasRoom | NoRoom
  deriving (Eq, Show)

data Tick = Tick
  { tNow      :: UTCTime
  , tPeriod   :: Period
  , tObserved :: Observation
  , tRoom     :: Room
  }
  deriving (Show)

data Action
  = Launch Run      -- ^ この運行を起こす
  | AskStop Run     -- ^ この運行に止まるよう伝える
  | ForceStop Run   -- ^ 伝えても収まらない運行を止める（SIGKILL）。コマの長さ + 猶予 + 5 分を過ぎても生きているとき
  deriving (Eq, Show)

initialRunner :: RunnerState
initialRunner = RunnerState Vacant Nothing 1

-- | 次に起こすコマの番号（落ち着いたコマの数 + 1）。
nextSlot :: Segment -> Int
nextSlot seg = segSlots seg + 1

endedWith :: EndReason -> Segment -> Segment
endedWith why seg = seg { segEnded = Just why }


-- | 一分ごと。見えたことを先に当て、それから時段に合わせ、最後に起こせるかを見る。
tick :: Params -> Tick -> RunnerState -> (RunnerState, [Action], [String])
tick p t s0 =
  let (s1, acts1, notes1) = observe p t s0
      (s2, acts2, notes2) = align p t s1
      (s3, acts3, notes3) = launch p t s2
  in (s3, acts1 ++ acts2 ++ acts3, notes1 ++ notes2 ++ notes3)

-- 見えたことを当てる ----------------------------------------------------------------

observe :: Params -> Tick -> RunnerState -> (RunnerState, [Action], [String])
observe p t s = case (rsPhase s, tObserved t) of
  (Vacant, Arrived _ _) -> (s, [], ["迟到的旧结果，忽略"])
  (Vacant, _) -> (s, [], [])

  (Running r, Arrived rid land)
    | rid == runId r -> land_ r land
    | otherwise -> (s, [], ["迟到的旧结果（" ++ showRun r ++ " 还在跑），忽略"])
  (Settling r _ _, Arrived rid land)
    | rid == runId r -> land_ r land
    | otherwise -> (s, [], ["迟到的旧结果，忽略"])

  (Running r, StillRunning)
    | overdue r -> (s { rsPhase = Settling r TimedOut now }, [AskStop r], [showRun r ++ " 超过一格还没收，请它停"])
    | otherwise -> (s, [], [])
  (Settling r _ askedAt, StillRunning)
    | diffUTCTime now askedAt > minutesOf 5 -> (s, [ForceStop r], [showRun r ++ " 请停五分钟了还是不收，强行停"])
    | otherwise -> (s, [], [])

  (Running r, Vanished) -> vanished r
  (Running r, NotRunning) -> vanished r
  (Settling r why _, Vanished) -> settled r why
  (Settling r why _, NotRunning) -> settled r why
  (Running r, Throttled resumeAt) -> throttled r resumeAt
  (Settling r _ _, Throttled resumeAt) -> throttled r resumeAt
  where
    now = tNow t
    overdue r = diffUTCTime now (runStartedAt r) > minutesOf (pGridL p + pRunGrace p)

    -- 落ち着いた：コマを数え、交付の有無を見て、次の最早を決める
    land_ r land = case rsSegment s of
      Just seg | segKey seg == runSeg r && segEnded seg == Nothing ->
        let seg1 = seg { segSlots = segSlots seg + 1
                       , segNoDelivery = if landDelivered land then 0 else segNoDelivery seg + 1
                       , segNextEarliest = Just (maxTime (addUTCTime (minutesOf (pGridL p)) (runStartedAt r)) (landNextEarliest land)) }
            (seg2, why)
              | landHeld land = (endedWith HeldForReview seg1, Just "落点待核对")
              | landEnough land = (endedWith SaidEnough seg1, Just "今天够了")
              | segNoDelivery seg1 >= pNoDeliveryLimit p = (endedWith NoDelivery seg1, Just "连着两格没有交付")
              | segSlots seg1 >= pMaxSlots p = (endedWith HardCap seg1, Just "到了一段的上限")
              | otherwise = (seg1, Nothing)
            note = showRun r ++ (if landDelivered land then " 收了，有交付" else " 收了，没有交付")
        in (s { rsPhase = Vacant, rsSegment = Just seg2 }, [], note : maybe [] (\w -> ["这一段结束：" ++ w]) why)
      _ -> (s { rsPhase = Vacant }, [], [showRun r ++ " 收了，但它属于别的段，不计"])

    -- 結果を残さずに消えた：この段の失敗として数える
    vanished r = case rsSegment s of
      Just seg | segKey seg == runSeg r && segEnded seg == Nothing ->
        let seg1 = seg { segFailures = segFailures seg + 1
                       , segNextEarliest = Just (addUTCTime (minutesOf (pGridL p)) now) }
            seg2 = if segFailures seg1 >= pMaxFailures p then endedWith StartFailures seg1 else seg1
            notes = (showRun r ++ " 没有留下结果就没了（第 " ++ show (segFailures seg1) ++ " 次）")
                    : [ "这一段结束：起动失败用尽" | segFailures seg1 >= pMaxFailures p ]
        in (s { rsPhase = Vacant, rsSegment = Just seg2 }, [], notes)
      _ -> (s { rsPhase = Vacant }, [], [showRun r ++ " 没了（别的段）"])

    -- 窓が尽きた：失敗に数えず、戻る時刻（分からなければ一時間あと）まで起こさない
    throttled r resumeAt =
      let resume = maxTime (addUTCTime (minutesOf 60) now) resumeAt
          seg' = fmap (\seg -> if segKey seg == runSeg r then seg { segNextEarliest = Just (maxTime resume (segNextEarliest seg)) } else seg) (rsSegment s)
      in (s { rsPhase = Vacant, rsSegment = seg' }, [], [showRun r ++ " 起不来：订阅的窗口到了上限，等到 " ++ show resume ++ " 再说（不算失败）"])

    -- 止めるよう伝えたものが止まった。そのコマの分の時間は使ったものとして、次の最早はコマの終わりから。
    -- ただし、時間切れで止めたもの（わけが TimedOut）が結果を残さなかったのなら、失敗として数える
    -- （一時停止や時段が閉じて止めたものは、止まるのが三十分を過ぎても数えない。2026-10-28、Codex の審査 A3）
    settled r why
      | why == TimedOut = vanished r
      | otherwise = case rsSegment s of
        Just seg | segKey seg == runSeg r ->
          let seg1 = seg { segNextEarliest = Just (maxTime (addUTCTime (minutesOf (pGridL p)) (runStartedAt r)) (segNextEarliest seg)) }
          in (s { rsPhase = Vacant, rsSegment = Just seg1 }, [], [showRun r ++ " 收住了"])
        _ -> (s { rsPhase = Vacant }, [], [showRun r ++ " 收住了"])

maxTime :: UTCTime -> Maybe UTCTime -> UTCTime
maxTime a Nothing = a
maxTime a (Just b) = max a b

-- 時段に合わせる ----------------------------------------------------------------------

align :: Params -> Tick -> RunnerState -> (RunnerState, [Action], [String])
align _ t s = case tPeriod t of
  PeriodClosed ->
    let (s1, acts, notes) = stopIfRunning "时段关了" s
        (seg', n2) = case rsSegment s1 of
          Just seg | segEnded seg == Nothing -> (Just (endedWith PeriodEnded seg), ["这一段结束：时段关了"])
          other -> (other, [])
    in (s1 { rsSegment = seg' }, acts, notes ++ n2)
  PeriodPaused key ->
    let (s1, acts, notes) = stopIfRunning "暂停了" s
    in (adopt key s1, acts, notes)
  PeriodUnknown -> stopIfRunning "见张り的许可不明" s
  PeriodOpen key ->
    case rsPhase s of
      Running r | runSeg r /= key ->
        let (s1, acts, notes) = stopIfRunning "换了一段" s in (adopt key s1, acts, notes)
      _ -> (adopt key s, [], [])
  where
    stopIfRunning why st = case rsPhase st of
      Running r -> (st { rsPhase = Settling r Asked (tNow t) }, [AskStop r], [why ++ "，请 " ++ showRun r ++ " 停"])
      _ -> (st, [], [])
    -- 鍵が替わっていれば新しい段。同じ鍵なら、終わっていてもそのまま（生き返らせない）
    adopt key st = case rsSegment st of
      Just seg | segKey seg == key -> st
      _ -> st { rsSegment = Just (Segment key 0 0 0 Nothing Nothing) }

-- 起こせるか ----------------------------------------------------------------------------

launch :: Params -> Tick -> RunnerState -> (RunnerState, [Action], [String])
launch _ t s = case (tPeriod t, rsPhase s, rsSegment s, tRoom t) of
  (PeriodOpen key, Vacant, Just seg, HasRoom)
    | segKey seg == key, segEnded seg == Nothing, ready seg ->
        let r = Run (RunId (rsNextRunId s)) key (nextSlot seg) (tNow t)
        in (s { rsPhase = Running r, rsNextRunId = rsNextRunId s + 1 }, [Launch r], ["起 " ++ showRun r])
  _ -> (s, [], [])
  where
    ready seg = maybe True (<= tNow t) (segNextEarliest seg)

showRun :: Run -> String
showRun r = let RunId n = runId r in "运行 #" ++ show n ++ "（第 " ++ show (runSlot r) ++ " 格）"
