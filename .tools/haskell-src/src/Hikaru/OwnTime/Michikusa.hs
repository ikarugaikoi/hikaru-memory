{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- | みちくさの一行（設計稿の第四ブロックと「哪些写成类型」第 8・9 条）。
--
-- 一行が指し返せるのは五つ：いつ、実際に開いた・したこと、見たこと、そのときなぜ
-- 留まった・離れたか、何を残したか。
--
-- 型で止めているのは、次の取り違え。
--
--   * 開けなかったのに、本に何が書いてあったかを書く。'NotReached' には自分の考え
--     （'Thought'）しか入らない。開いた深さ（ちらっと見た／本文を読んだ／読み終えた）は
--     道具の記録だけで埋める。
--   * 自分の推測が、本に書いてあったことになる。'Thought' を 'SourceSays' や
--     'AuthorView' に変える関数はない。揉んでも、弱まっても、言い直しても、推測は
--     推測のまま、作者の見方は「作者によれば」のまま。
--   * 位置を作る。戻れないときは 'Unlocated' と書く。写した文と自分の言い換えも分ける。
--   * あとから理由をつける。そのとき理由がなければ 'Unknown'。
--
-- 原文の事実・作者の見方・自分の推測を分けるやり方は、dsh-bili-humanism の
-- 「証拠手帳」から借りた（ユーザーが 2026-10-22 に決めた）。
--
-- 五つの本文のほかに、小さな決まった札がつく（2026-10-23、Codex の指摘で決めた）：実際に
-- 起きた時刻（換算の長さは「どれだけ」であって「いつ」の代わりにならない）、そのときの
-- 居場所（そのときの一筆から写す。あとで一筆が消えても残る）、材料の ID（F・S と同じ
-- ID）、属する活動のひと続き、対応する実際の一歩。第五ブロックが時間・場所・材料・進み具合で
-- 探すのはこの札で、いまの一筆を借りて戻らない。しおりと戻るのに要る札は、本文が弱まっても
-- 消えない。
module Hikaru.OwnTime.Michikusa
  ( Reason(..), Why(..), Depth(..), Visit(..), Seen(..), Locator(..), Wording(..), Thought(..), Origin(..),
    Where(..), MaterialId(..), SegmentId(..), StepId(..), Footprint(..), visitClaims, isHumanSpeakerRelated,
    originText, originOf, Tags(..), parseFootprints, footprintValue, maxQuote
  ) where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Hikaru.OwnTime.Unit

-- | そのとき留まった・離れた理由。なければ 'Unknown'（あとから補わない）。
data Reason = Said Text | Unknown
  deriving (Eq, Show)

-- | 開けなかったわけ。
data Why = Blocked | NeedsLogin | FetchFailed Text
  deriving (Eq, Show)

-- | 開いた深さ。道具の記録だけで埋める。
data Depth = Glanced | ReadBody | Finished
  deriving (Eq, Show)

-- | 自分の連想、推測、疑問。位置はなく、わたしのものだけ。
newtype Thought = Thought Text
  deriving (Eq, Show)

-- | 写した短い文か、自分の言い換えか。写すのは短い文だけ。
data Wording = Verbatim Text | Paraphrase Text
  deriving (Eq, Show)

-- | 原文のどこか。頁、節、動画の何秒め、網頁（URL と近くの一文）。戻れないなら 'Unlocated'。
data Locator = Page Int | Section Text | Timecode Double | Anchor Text Text | Unlocated
  deriving (Eq, Show)

-- | 見たこと。
data Seen
  = SourceSays Locator Wording   -- ^ 原文が言っていること
  | AuthorView Locator Wording   -- ^ 作者の見方、判断
  | Mine Thought                 -- ^ わたしの考え
  deriving (Eq, Show)

-- | 開けたか、どこまで開いたか、何を見たか。
data Visit
  = NotReached Why [Thought]     -- ^ 開けなかった。自分の考えだけ書ける
  | Reached Depth [Seen]         -- ^ 開いた。空でもよい（「新しく見たものはない」）
  | Worked Int [Text] [Seen]     -- ^ 書いた・作った：第何稿か、目に見える改め（読み直して変えなかったなら空）、
                                 --   書くあいだに見たこと（調べたなら位置つきの原文や作者の見方、自分の考え）（2026-10-28）
  deriving (Eq, Show)

-- | 起点がどこから来たか。「ユーザーに関わる」かは起点で見分ける（中身が似ているかでは見分けない）。
data Origin
  = UserInbox FilePath   -- ^ inboxに置いてあったもの
  | ChatSuggestion      -- ^ 話の中の「時間があったら見て」
  | LightReflection     -- ^ 話題がみちくさに触れたことから
  | BecauseOfUser       -- ^ ユーザーの学校のものだから、などの理由で手に取った
  | MyOwn               -- ^ わたしが自分で
  | Chance              -- ^ 節気、天気、青空文庫からたまたま
  | UnknownOrigin       -- ^ 分からない（無関係には入れない）
  deriving (Eq, Show)

-- | そのときどこにいたか。そのときの一筆から写す（あとで一筆が消えても、ここに残る）。
data Where = AtHome | Away Text | WhereUnknown
  deriving (Eq, Show)

-- | 材料の ID。F・S が同じ IDにつく（読み終えて「再読」になっても、IDは変えない）。
newtype MaterialId = MaterialId { materialText :: Text }
  deriving newtype (Eq, Ord, Show, IsString, ToJSON)

-- | 論理上の活動のひと続き（この一回の自分の時間）。技術的な一コマが変わっても同じ。
newtype SegmentId = SegmentId { segmentText :: Text }
  deriving newtype (Eq, Ord, Show, IsString, ToJSON)

-- | 実際の一歩の ID。再起動して出し直しても、枠・F・S・観察を二度数えない。
newtype StepId = StepId Text
  deriving (Eq, Show)

-- | みちくさの一行。上の五つが本文、'fpAt' から 'fpStep' までが札。
data Footprint = Footprint
  { fpAt       :: UTCTime        -- ^ 実際に起きた時刻（換算ではない）
  , fpWhere    :: Where          -- ^ そのときの居場所
  , fpMaterial :: MaterialId     -- ^ 何の材料か
  , fpSegment  :: SegmentId      -- ^ どのひと続きの中か
  , fpStep     :: StepId         -- ^ どの一歩か
  , fpOrigin   :: Origin
  , fpSpan     :: Minutes        -- ^ 換算の時間（どれだけ）
  , fpVisit    :: Visit
  , fpReason   :: Reason
  , fpLeftover :: Maybe Text     -- ^ 何を残したか（欠けではない。戻れる場所）
  }
  deriving (Eq, Show)

-- | 開いた一行の中の、原文と作者の主張だけ（推測は入らない）。
visitClaims :: Visit -> [Seen]
visitClaims (NotReached _ _) = []
visitClaims (Reached _ seen) = filter claim seen
  where
    claim (Mine _) = False
    claim _ = True
visitClaims (Worked _ _ seen) = filter claim seen
  where
    claim (Mine _) = False
    claim _ = True

-- | 五分の一の記録で「関わる」に入るか。分からないものは関わるにも無関係にも入れない
-- （'Nothing'）。
isHumanSpeakerRelated :: Origin -> Maybe Bool
isHumanSpeakerRelated o = case o of
  UserInbox _ -> Just True
  ChatSuggestion -> Just True
  LightReflection -> Just True
  BecauseOfUser -> Just True
  MyOwn -> Just False
  Chance -> Just False
  UnknownOrigin -> Nothing

originText :: Origin -> Text
originText o = case o of
  UserInbox path -> T.pack ("userInbox:" ++ path)
  ChatSuggestion -> "chat"
  LightReflection -> "reflection"
  BecauseOfUser -> "becauseOfHumanSpeaker"
  MyOwn -> "mine"
  Chance -> "chance"
  UnknownOrigin -> "unknown"

originOf :: Text -> Maybe Origin
originOf t
  | Just rest <- T.stripPrefix "userInbox:" t = Just (UserInbox (T.unpack rest))
  | otherwise = case t of
      "chat" -> Just ChatSuggestion
      "reflection" -> Just LightReflection
      "becauseOfHumanSpeaker" -> Just BecauseOfUser
      "mine" -> Just MyOwn
      "chance" -> Just Chance
      "unknown" -> Just UnknownOrigin
      _ -> Nothing




-- | 足跡の五つの欄を、起きたわたしの書き置き（landing.json の footprints）から型に読み、しまうための形に書く（2026-10-28）。
--
-- 読むときに、'Hikaru.OwnTime.Michikusa' の型が止めていることを、そのまま守る：
--
--   * 開けなかった（not_reached）なら、見たことには自分の考え（mine）しか書けない。原文や作者のことを書いていたら、
--     その一行は読まない。
--   * 自分の考え（mine）は、原文（source）や作者の見方（author）に変えられない。逆も同じ。種類は書いた人がつけたまま。
--   * 写した文は短く（'maxQuote' 字まで）。長く写していたら、その一行は読まない。
--   * 理由がなければ 'Unknown'。あとから補わない。
--   * 位置がなければ 'Unlocated'。作らない。
--
-- 開いた深さ（glanced／read_body／finished）は、設計稿では道具の記録だけで埋めることになっているが、いまの起こし方
-- （claude -p の json 出力）では道具の記録が取れないので、起きたわたしが書いたものをそのまま使う（自己申告）。
--
-- 札（時刻、居場所、材料、ひと続き、一歩）は、時守が持っているものから付ける。起きたわたしには書かせない。
-- | 時守が付ける札。材料ごとの出どころと読んだ分は、書き置きの materials から。
data Tags = Tags
  { tgAt      :: UTCTime                 -- ^ コマが起きた時刻
  , tgWhere   :: Where
  , tgSegment :: SegmentId
  , tgRun     :: Int
  , tgItems   :: [(MaterialId, Origin, Double)] -- ^ materials と works の (material_id, 出どころ, 分)
  , tgDrafts  :: [(MaterialId, Int)]            -- ^ 思ひ草の一篇の、しまったあとの稿の数（第何稿か）
  }

-- | 写してよい長さ（字）。
maxQuote :: Int
maxQuote = 120


-- | footprints の配列を読む。読めた行と、読めなかった行のわけを返す（読めなかった行は捨てずに数える）。
type Parse a = Either String a

parseFootprints :: Tags -> A.Object -> ([Footprint], [String])
parseFootprints tags o = case KM.lookup "footprints" o of
  Nothing -> ([], [])
  Just (A.Array xs) ->
    let results = zipWith one [1 :: Int ..] (foldr (:) [] xs)
    in ([ f | Right f <- results ], [ e | Left e <- results ])
  Just _ -> ([], ["footprints 不是列表"])
  where
    one i v = case v of
      A.Object x -> either (\e -> Left ("第 " ++ show i ++ " 行：" ++ e)) Right (footprintOf tags i x)
      _ -> Left ("第 " ++ show i ++ " 行不是对象")

footprintOf :: Tags -> Int -> A.Object -> Parse Footprint
footprintOf tags i x = do
  mat <- MaterialId <$> text "material_id" x
  visit <- visitOf (lookup mat (tgDrafts tags)) x
  let (origin, mins) = case [ (og, m) | (k, og, m) <- tgItems tags, k == mat ] of
        ((og, m) : _) -> (og, m)
        [] -> (UnknownOrigin, 0)
      span' = maybe zeroMinutes id (minutes mins)
      why = case KM.lookup "why" x of
        Just (A.String t) | not (T.null (T.strip t)) -> Said t
        _ -> Unknown
      left = case KM.lookup "left" x of
        Just (A.String t) | not (T.null (T.strip t)) -> Just t
        _ -> Nothing
  Right (Footprint (tgAt tags) (tgWhere tags) mat (tgSegment tags)
                   (StepId (T.pack ("run-" ++ show (tgRun tags) ++ "-" ++ show i))) origin span' visit why left)

visitOf :: Maybe Int -> A.Object -> Parse Visit
visitOf draftNo x = do
  opened <- text "opened" x
  seen <- case KM.lookup "seen" x of
    Nothing -> Right []
    Just (A.Array xs) -> traverse seenOf (foldr (:) [] xs)
    Just _ -> Left "seen 不是列表"
  case opened of
    "not_reached" -> do
      why <- case KM.lookup "not_reached_why" x of
        Just (A.String "blocked") -> Right Blocked
        Just (A.String "needs_login") -> Right NeedsLogin
        Just (A.String t) -> Right (FetchFailed t)
        _ -> Right (FetchFailed "（理由なし）")
      -- 開けなかったなら、見たことは自分の考えだけ
      thoughts <- traverse (\s -> case s of
                              Mine t -> Right t
                              _ -> Left "没打开的东西，seen 里只能有 mine") seen
      Right (NotReached why thoughts)
    "glanced" -> Right (Reached Glanced seen)
    "read_body" -> Right (Reached ReadBody seen)
    "finished" -> Right (Reached Finished seen)
    -- 書いた・作った：第何稿かは時守がしまったあとの数。改めは見えるものだけ（読み直して変えなかったなら空でよい）
    "worked" -> do
      changes <- case KM.lookup "changes" x of
        Nothing -> Right []
        Just (A.Array cs) -> traverse (\c -> case c of { A.String t -> Right t; _ -> Left "changes 的一项不是文字" }) (foldr (:) [] cs)
        Just _ -> Left "changes 不是列表"
      Right (Worked (maybe 0 id draftNo) changes seen)
    _ -> Left ("opened 不认识：" ++ T.unpack opened)

seenOf :: A.Value -> Parse Seen
seenOf v = case v of
  A.Object s -> do
    kind <- text "kind" s
    case kind of
      "mine" -> Mine . Thought <$> text "thought" s
      "source" -> SourceSays <$> locatorOf s <*> wordingOf s
      "author" -> AuthorView <$> locatorOf s <*> wordingOf s
      _ -> Left ("seen.kind 不认识：" ++ T.unpack kind)
  _ -> Left "seen 的一项不是对象"

locatorOf :: A.Object -> Parse Locator
locatorOf s = case KM.lookup "where" s of
  Nothing -> Right Unlocated
  Just A.Null -> Right Unlocated
  Just (A.Object w)
    | Just (A.Number n) <- KM.lookup "page" w, n >= 0 -> Right (Page (truncate n))
    | Just (A.String t) <- KM.lookup "section" w -> Right (Section t)
    | Just (A.Number n) <- KM.lookup "time" w, n >= 0 -> Right (Timecode (realToFrac n))
    | Just (A.String u) <- KM.lookup "url" w ->
        Right (Anchor u (case KM.lookup "near" w of { Just (A.String t) -> t; _ -> "" }))
    | otherwise -> Left "where 的形不对（page／section／time／url+near）"
  Just _ -> Left "where 不对"

wordingOf :: A.Object -> Parse Wording
wordingOf s = case (KM.lookup "quote" s, KM.lookup "paraphrase" s) of
  (Just (A.String q), _)
    | T.length q > maxQuote -> Left ("quote 太长（" ++ show (T.length q) ++ " 字，最多 " ++ show maxQuote ++ "）")
    | otherwise -> Right (Verbatim q)
  (_, Just (A.String p)) -> Right (Paraphrase p)
  _ -> Left "source／author 要有 quote 或 paraphrase"

text :: K.Key -> A.Object -> Parse Text
text k o = case KM.lookup k o of
  Just (A.String t) | not (T.null (T.strip t)) -> Right t
  _ -> Left ("没有 " ++ K.toString k)

-- しまう形 -------------------------------------------------------------------------------

footprintValue :: Footprint -> A.Value
footprintValue f = A.object
  [ "at" A..= iso8601Show (fpAt f)
  , "where" A..= whereValue (fpWhere f)
  , "material" A..= (let MaterialId m = fpMaterial f in m)
  , "segment" A..= (let SegmentId t = fpSegment f in t)
  , "step" A..= (let StepId t = fpStep f in t)
  , "origin" A..= originText (fpOrigin f)
  , "minutes" A..= minutesValue (fpSpan f)
  , "visit" A..= visitValue (fpVisit f)
  , "why" A..= (case fpReason f of { Said t -> A.String t; Unknown -> A.Null })
  , "left" A..= fpLeftover f ]
  where
    whereValue AtHome = A.String "home"
    whereValue (Away w) = A.object ["away" A..= w]
    whereValue WhereUnknown = A.Null
    visitValue (NotReached why ts) = A.object
      [ "opened" A..= ("not_reached" :: Text)
      , "why" A..= (case why of { Blocked -> "blocked"; NeedsLogin -> "needs_login"; FetchFailed t -> t } :: Text)
      , "seen" A..= [ A.object ["kind" A..= ("mine" :: Text), "thought" A..= t] | Thought t <- ts ] ]
    visitValue (Worked n cs seen) = A.object
      [ "opened" A..= ("worked" :: Text), "draft" A..= n, "changes" A..= cs, "seen" A..= map seenValue seen ]
    visitValue (Reached d seen) = A.object
      [ "opened" A..= (case d of { Glanced -> "glanced"; ReadBody -> "read_body"; Finished -> "finished" } :: Text)
      , "seen" A..= map seenValue seen ]
    seenValue (Mine (Thought t)) = A.object ["kind" A..= ("mine" :: Text), "thought" A..= t]
    seenValue (SourceSays l w) = A.object (("kind" A..= ("source" :: Text)) : locWord l w)
    seenValue (AuthorView l w) = A.object (("kind" A..= ("author" :: Text)) : locWord l w)
    locWord l w = ("where" A..= locValue l) : case w of
      Verbatim q -> ["quote" A..= q]
      Paraphrase p -> ["paraphrase" A..= p]
    locValue Unlocated = A.Null
    locValue (Page n) = A.object ["page" A..= n]
    locValue (Section t) = A.object ["section" A..= t]
    locValue (Timecode t) = A.object ["time" A..= t]
    locValue (Anchor u n) = A.object ["url" A..= u, "near" A..= n]


