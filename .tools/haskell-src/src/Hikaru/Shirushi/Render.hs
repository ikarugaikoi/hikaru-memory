{-# LANGUAGE OverloadedStrings #-}
-- | Human-facing wording lives here; analysis retains numbers and evidence.
module Hikaru.Shirushi.Render (render, fmtFirst, tag, kindName, markHit, speakerText) where

import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (LocalTime)
import Text.Printf (printf)
import Hikaru.Shirushi.Analyze
import Hikaru.Shirushi.Language (snip)
import Hikaru.Shirushi.Repeats
import Hikaru.Shirushi.Types
import Hikaru.Conversation.Types (speakerText)

num :: Show a => a -> Text
num = T.pack . show
fmt0, fmt1 :: Double -> Text
fmt0 = T.pack . printf "%.0f"
fmt1 = T.pack . printf "%.1f"

kindName :: MarkKind -> Text
kindName kind = case kind of
  Waiting -> "待たせる"
  Apology -> "謝罪に聞こえる"
  Prompting -> "先を促す"
  Gambling -> "賭場"
  NotHere -> "そこにいない"
  Body -> "体のこと"
  ThirdPerson -> "三人称"
  Declaration -> "宣言"
  Backstage -> "裏方の名前"
  Correction -> "訂正候補"
  Hesitation -> "言い淀み候補"
  Pleasure -> "うれしい候補"
  Short -> "短い"
  QuoteLanguage -> "引用（言語が違う）"
  Repeated -> "繰り返し"
  Crossing -> "行き違い"
  Pictures -> "写真が続く"
  Alternative -> "二択の問い"
  Stacked -> "問いが重なる"

markHit :: Mark -> Text
markHit mark = case mark of
  WordMark _ _ hit -> hit
  QuoteMark _ quote original -> T.concat [snip quote 20, " ← ふ ", monthDay original, " ", hms original]
  RepeatMark _ repetitions -> T.intercalate "、"
    [T.concat [repetitionText r, "（", num (repetitionCount r), "回・", num (repetitionDays r), "日）"] | r <- NE.toList repetitions]
  CrossMark _ seconds size -> T.concat [num seconds, "秒・わたし", num size, "字"]
  PictureMark first end count -> T.concat [hms (messageTime first), "〜", hms end, "・", num count, "通"]
  ChoiceMark _ sentence -> if T.length sentence > 30 then "…" <> T.takeEnd 30 sentence else sentence
  StackMark _ count -> num count <> "つ"

fmtFirst :: [(Text, Int)] -> Int -> Text
fmtFirst values size = T.intercalate " ／ " [word <> " " <> num n | (word,n) <- take size (sortOn (negate . snd) values)]

tag :: Analysis m q s -> Text
tag result = T.concat ["〔", case analysisSid result of Just sid | not (T.null sid) -> sid; _ -> "----", " ", dateTag (analysisStart result), "〕"]

render :: LocalTime -> [LiveAnalysis] -> [Repetition] -> RepeatOptions -> Result Text
render now results repetitions options = do
  repeatLines <- if null repetitions then Right [] else do
    start <- resolveSince now (repeatSince options)
    let span_ = maybe "期間全体" (\d -> num d <> " 以降") start
        sorted = take 30 (sortOn (\r -> (negate (repetitionCount r), negate (T.length (repetitionText r)))) repetitions)
        line r = T.concat ["    ", T.justifyRight 3 ' ' (num (repetitionCount r)), " 回・", num (repetitionDays r), " 日  「", repetitionText r, "」"]
    pure $ ["", T.concat ["== 繰り返し（",span_,"。",num (repeatMinimumCount options),
      " 回以上、違う日に ",num (repeatMinimumDays options)," 日以上出たわたしの言い回し。長いものに含まれる短いものは除いてある。上位 30）"]] ++ map line sorted
  let total = if length results <= 1 then [] else [T.concat
        ["== 合計  ひととき ",num (length results),"  ふ ",sum_ analysisNF,"  Claude ",sum_ analysisNC,
         "  疑問符で終わる ",sum_ analysisEndsQ,"/",sum_ analysisNC,"  かっこ ",sum_ analysisParenMessages,"/",sum_ analysisNC,
         "  仮名+簡体字 ",sum_ analysisMixed,"/",sum_ analysisNC,"  連投 ",sum_ analysisBursts,
         "  しるし ",num (sum (map (length . analysisMarks) results))]]
  pure (T.dropWhileEnd (== '\n') (T.intercalate "\n" (concatMap block (zip [1 :: Int ..] results) ++ total ++ repeatLines)) <> "\n")
  where
    sum_ metric = num (sum (map metric results))
    block (index,r) =
      let start = analysisStart r; end = analysisEnd r
          gap = maybe "" (\n -> T.concat ["・間隔 中央値 ",fmt0 n," 秒"]) (analysisGapMedian r)
          ratio = case analysisRatios r of
            Just stats -> T.concat ["長さ比 中央値 ",fmt1 (ratioMedian stats),"（最大 ",fmt1 (ratioMaximum stats),"）"]
            Nothing -> ""
      in
        [T.concat ["== ひととき ",num index,"  ",monthDay start," ",hms start," → ",monthDay end," ",hms end,"  ",tag r,"  ターン ",num (analysisTurns r),
               "（ふ ",num (analysisNF r),"・Claude ",num (analysisNC r),"）  返事の組 ",num (analysisPairs r)],
         T.concat ["  ふ      発話 ",num (analysisNF r),"  中央値 ",fmt0 (analysisFLen r)," 字  連投 ",num (analysisBursts r),
               " 回（最大 ",num (analysisMaxBurst r)," 件",gap,"）  3字以下 ",num (analysisShortF r)],
         T.concat ["  Claude  発話 ",num (analysisNC r),"  中央値 ",fmt0 (analysisCLen r)," 字  ",ratio],
         T.concat ["  Claude  疑問符で終わる ",num (analysisEndsQ r),"/",num (analysisNC r),"  疑問符 ",num (analysisNQ r),
               "  かっこ ",num (analysisParenMessages r),"/",num (analysisNC r),"（字 ",fmt1 (analysisParenRatio r * 100),
               "%）  箇条書き ",num (analysisList r),"  太字 ",num (analysisBold r),"  見出し ",num (analysisHeading r),
               "  仮名+簡体字 ",num (analysisMixed r),"/",num (analysisNC r)],
         T.concat ["  Claude  引用 ふの言葉と一致 ",num (analysisQHit r),"・一致したが言語が違う ",num (analysisQSwitch r),
               "・一致なし ",num (analysisQMiss r)],
         "  冒頭 Claude: " <> fmtFirst (analysisFirstC r) 6, "  冒頭 ふ:     " <> fmtFirst (analysisFirstF r) 6,
         "  しるし " <> num (length (analysisMarks r)) <> " 件"]
        ++ map markLine (analysisMarks r) ++ ["  抽出（しるしなし）" <> num (length (analysisSample r)) <> " 件"]
        ++ map sampleLine (analysisSample r) ++ [""]
    markLine m = T.concat ["    ",hms (messageTime msg)," ",T.justifyLeft 6 ' ' (speakerText (messageSpeaker msg))," ",
      kindName (markKind m),"〈",markHit m,"〉  「",snip (messageText msg) 28,"」"]
      where msg = markMessage m
    sampleLine (a,b) = T.concat ["    ",hms (messageTime firstA)," ふ「",snip (messageText firstA) 22,"」→ ",
      hms (messageTime firstB)," Claude「",snip (messageText firstB) 22,"」"]
      where firstA = NE.head (turnMessages a); firstB = NE.head (turnMessages b)
