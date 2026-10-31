{-# LANGUAGE OverloadedStrings #-}
-- | Pure analysis. Shapes accumulate with a strict fold; nonempty blocks and
-- turns carry the preconditions formerly checked by dictionary lookups.
module Hikaru.Shirushi.Analyze
  ( Analysis(..), RatioStats(..), LiveAnalysis, analyze, monthDay, hms, dateTag ) where

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.IntSet as IS
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Hikaru.HouseDay (HouseDay)
import Hikaru.Shirushi.Id
import Hikaru.Shirushi.Syntax (strip)
import qualified Hikaru.Shirushi.Language as L
import qualified Hikaru.Shirushi.RepeatTable as Table
import Hikaru.Shirushi.Types
import System.Random (mkStdGen, uniformR)

data Analysis mark quote sample = Analysis
  { analysisId :: !BlockId, analysisSid :: !(Maybe Text), analysisFullSid :: !(Maybe Text)
  , analysisDay :: !HouseDay, analysisStart :: !LocalTime, analysisEnd :: !LocalTime
  , analysisTurns :: !Int, analysisNF :: !Int, analysisNC :: !Int
  , analysisFLen :: !Double, analysisCLen :: !Double
  , analysisBursts :: !Int, analysisMaxBurst :: !Int
  , analysisGapMedian :: !(Maybe Double), analysisRatios :: !(Maybe RatioStats)
  , analysisEndsQ :: !Int, analysisNQ :: !Int, analysisParenMessages :: !Int
  , analysisParenRatio :: !Double, analysisList :: !Int, analysisBold :: !Int
  , analysisHeading :: !Int, analysisMixed :: !Int
  , analysisFirstC :: ![(Text, Int)], analysisFirstF :: ![(Text, Int)]
  , analysisShortF :: !Int, analysisQHit :: !Int, analysisQSwitch :: !Int, analysisQMiss :: !Int
  , analysisQuotes :: ![quote], analysisMarks :: ![mark]
  , analysisSample :: ![sample], analysisPairs :: !Int
  } deriving (Eq, Show)

data RatioStats = RatioStats { ratioMedian :: !Double, ratioMaximum :: !Double }
  deriving (Eq, Show)

type LiveAnalysis = Analysis Mark (Message, Quote) (Turn, Turn)

monthDay, hms, dateTag :: LocalTime -> Text
monthDay = T.pack . formatTime defaultTimeLocale "%m-%d"
hms = T.pack . formatTime defaultTimeLocale "%H:%M:%S"
dateTag = T.pack . formatTime defaultTimeLocale "%04Y-%m-%dT%H:%M:%S"

data Totals = Totals
  { totalCount :: !Int, lengths :: ![Double], endQuestions :: !Int, questions :: !Int
  , parenMessages :: !Int, parenSum :: !Rational, lists :: !Int, bolds :: !Int
  , headings :: !Int, mixed :: !Int, firsts :: !(Table.Table Text Int)
  }

totals :: Foldable f => f Shape -> Totals
totals = foldl' add (Totals 0 [] 0 0 0 0 0 0 0 0 Table.empty)
  where
    add t s = Totals
      (totalCount t + 1) (fromIntegral (shapeLen s):lengths t)
      (endQuestions t + fromEnum (shapeEndQ s)) (questions t + shapeNQ s)
      (parenMessages t + fromEnum (shapeParenN s /= 0)) (parenSum t + toRational (shapeParenRatio s))
      (lists t + shapeList s) (bolds t + shapeBold s) (headings t + shapeHeading s)
      (mixed t + fromEnum (shapeMixed s)) (Table.alter (shapeFirst s) (maybe 1 (+1)) (firsts t))

median :: [Double] -> Maybe Double
median = fmap medianOf . NE.nonEmpty

medianOf :: NonEmpty Double -> Double
medianOf xs = if odd size then ordered NE.!! middle
              else (ordered NE.!! (middle - 1) + ordered NE.!! middle) / 2
  where ordered = NE.sort xs
        size = length xs
        middle = size `div` 2

ratioStats :: [Double] -> Maybe RatioStats
ratioStats = fmap (\xs -> RatioStats (medianOf xs) (maximum xs)) . NE.nonEmpty

-- Same random library, generator and removal order as the deployed core.
-- The count is bounded by the population here, so there is no invalid-size API.
sample :: Integer -> [a] -> [a]
sample seed input = draw (min 3 (length input)) (Seq.fromList input) (mkStdGen (fromInteger seed))
  where
    draw 0 _ _ = []
    draw n pool generator =
      let (index, next) = uniformR (0, Seq.length pool - 1) generator
      in Seq.index pool index : draw (n - 1) (Seq.deleteAt index pool) next

analyze :: Integer -> [Message] -> [Repetition] -> Sessions -> NonEmpty Message -> Result LiveAnalysis
analyze seed history repetitions = analyzeCandidates seed history repeatCandidates
  where
    -- Keep preparation outside the per-block function. Inlining this small
    -- wrapper lets a partially applied analyzer share the sorted list.
    repeatCandidates = sortOn (negate . repetitionCount) (filter eligible repetitions)
    eligible r = not (T.any (`elem` ("。、，！？!?…：:；;「」『』“”\"（）()・" :: String)) (repetitionText r))
{-# INLINE analyze #-}

analyzeCandidates :: Integer -> [Message] -> [Repetition] -> Sessions -> NonEmpty Message -> Result LiveAnalysis
analyzeCandidates seed history repeatCandidates sessions block = do
  day <- hikaruDay (messageTime first)
  pure Analysis
    { analysisId = blockId full (messageTime first), analysisSid = messageSession first, analysisFullSid = fullSessionText full
    , analysisDay = day, analysisStart = messageTime first, analysisEnd = messageTime (NE.last block)
    , analysisTurns = length ts, analysisNF = totalCount ff, analysisNC = totalCount cf
    , analysisFLen = fromMaybe 0 (median (lengths ff)), analysisCLen = fromMaybe 0 (median (lengths cf))
    , analysisBursts = length bursts, analysisMaxBurst = maximum (0:map (length . turnMessages) fumiTurns)
    , analysisGapMedian = median gaps, analysisRatios = ratioStats ratios
    , analysisEndsQ = endQuestions cf, analysisNQ = questions cf, analysisParenMessages = parenMessages cf
    , analysisParenRatio = if totalCount cf == 0 then 0 else fromRational (parenSum cf / fromIntegral (totalCount cf))
    , analysisList = lists cf, analysisBold = bolds cf, analysisHeading = headings cf, analysisMixed = mixed cf
    , analysisFirstC = Table.toList (firsts cf), analysisFirstF = Table.toList (firsts ff)
    , analysisShortF = length [() | (_, m) <- fumi, T.length (strip (messageText m)) <= 3]
    , analysisQHit = hits, analysisQSwitch = switches, analysisQMiss = misses
    , analysisQuotes = [(m, q) | (_, m, q) <- cited]
    , analysisMarks = sortOn (messageTime . markMessage) (map fst evidence)
    , analysisSample = sample seed quiet, analysisPairs = length pairs }
  where
    first = NE.head block
    full = resolveSession sessions (messageSession first)
    indexed = zip [0..] (NE.toList block)
    fumi = filter ((== HumanSpeaker) . messageSpeaker . snd) indexed
    claude = filter ((== Claude) . messageSpeaker . snd) indexed
    ff = totals (map (L.features . snd) fumi)
    cf = totals (map (L.features . snd) claude)
    ts = NE.toList (turns block)
    fumiTurns = filter ((== HumanSpeaker) . turnSpeaker) ts
    bursts = filter ((>=2) . length . turnMessages) fumiTurns
    consecutive xs = zip xs (drop 1 xs)
    elapsed a b = realToFrac (diffLocalTime (messageTime b) (messageTime a)) :: Double
    seconds a b = floor (elapsed a b) :: Integer
    gaps = [elapsed a b | turn <- bursts, (a,b) <- consecutive (NE.toList (turnMessages turn))]
    pairs = [(a,b) | (a,b) <- consecutive ts, turnSpeaker a == HumanSpeaker, turnSpeaker b == Claude]
    size = sum . fmap (T.length . messageText) . turnMessages
    ratios = [fromIntegral (size b) / fromIntegral (size a) | (a,b) <- pairs, size a /= 0]
    dictionaries select inputs = [(WordMark m kind hit, [i]) | (i,m) <- inputs, (kind, hit) <- select m, kind /= Short]
    citations = [(i,m,L.quotes m history) | (i,m) <- claude]
    cited = [(i,m,q) | (i,m,qs) <- citations, q <- qs]
    (hits,switches,misses) = foldl' countQuote (0,0,0) cited
    countQuote (h,s,m) (_,_,q) = case quoteHit q of
      Nothing -> (h,s,m+1)
      Just _ | quoteSameLanguage q -> (h+1,s,m)
             | otherwise -> (h,s+1,m)
    quoteEvidence (i,m,qs) = [(QuoteMark m (quoteText q) (messageTime original), [i])
      | q <- qs, Just original <- [quoteHit q], not (quoteSameLanguage q)] ++ repeated i m
    repeated i m = case NE.nonEmpty (take 2 found) of
      Nothing -> []
      Just rs -> [(RepeatMark m rs, [i])]
      where
        segments = map L.squeeze (T.splitOn "\n" (L.stripParentheses (messageText m)))
        found = filter (\r -> any (T.isInfixOf (repetitionText r)) segments) repeatCandidates
    crossed = [(CrossMark b delta (T.length (messageText a)), [i])
      | ((_,a),(i,b)) <- consecutive indexed, messageSpeaker a == Claude, messageSpeaker b == HumanSpeaker
      , let delta = seconds a b, delta >= 0, delta <= 5, T.length (messageText a) >= 60
      , T.length (messageText b) < 200, not ("pasted_content" `T.isInfixOf` messageText b)]
    pictureRuns = foldl' addPicture [] [(i,m) | (i,m) <- fumi, "【画像" `T.isInfixOf` messageText m]
    addPicture (current@((_,previous) :| _) : rest) entry@(_,m)
      | seconds previous m <= 600 = (entry NE.<| current):rest
    addPicture done entry = (entry :| []):done
    shots = [(PictureMark start (messageTime end) (length run), map fst (NE.toList run))
      | run <- map NE.reverse (reverse pictureRuns), length run >= 3
      , let start = snd (NE.head run), let end = snd (NE.last run)]
    asked = concatMap questionEvidence claude
    questionEvidence (i,m) = [(ChoiceMark m q, [i]) | q <- sentences, L.alternativeQuestion q]
      ++ [(StackMark m (length sentences), [i]) | length sentences >= 3]
      where sentences = L.questionSentences (L.stripParentheses (messageText m))
    evidence = dictionaries L.marksClaude claude ++ dictionaries L.marksHumanSpeaker fumi
      ++ concatMap quoteEvidence citations ++ crossed ++ shots ++ asked
    marked = IS.fromList (concatMap snd evidence)
    quiet = [(a,b) | (a,b) <- pairs, not (any (`IS.member` marked) (turnIndices a ++ turnIndices b))]
