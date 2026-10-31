{-# LANGUAGE OverloadedStrings #-}
-- | The unchanged mark.json schema, encoded by aeson from a separate saved
-- report: saved samples carry times, while live samples carry whole turns.
module Hikaru.Shirushi.Json (SavedAnalysis, SavedMark(..), SavedQuote(..), SavedSample(..), encodeReports, toJson) where

import Data.Aeson
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Key (fromText)
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (LocalTime)
import Hikaru.Shirushi.Analyze
import Hikaru.Shirushi.Render (kindName, markHit, speakerText)
import qualified Hikaru.Shirushi.Syntax as S
import Hikaru.Shirushi.Types

data SavedMark = SavedMark !LocalTime !Speaker !MarkKind !Text deriving (Eq, Show)
data SavedQuote = SavedQuote !LocalTime !Text !(Maybe LocalTime) !Bool deriving (Eq, Show)
data SavedSample = SavedSample !LocalTime !LocalTime deriving (Eq, Show)
type SavedAnalysis = Analysis SavedMark SavedQuote SavedSample

saved :: LiveAnalysis -> SavedAnalysis
saved a = a
  { analysisMarks = [SavedMark (messageTime m) (messageSpeaker m) (markKind cue) (markHit cue) | cue <- analysisMarks a, let m = markMessage cue]
  , analysisQuotes = [SavedQuote (messageTime m) (quoteText q) (messageTime <$> quoteHit q) (quoteSameLanguage q) | (m,q) <- analysisQuotes a]
  , analysisSample = [SavedSample (first a') (first b) | (a',b) <- analysisSample a] }
  where first = messageTime . NE.head . turnMessages

encodeReport :: SavedAnalysis -> Encoding
encodeReport r = pairs $
  "id" .= analysisId r <> "sid" .= analysisSid r <> "sid_full" .= analysisFullSid r <>
  "day" .= show (analysisDay r) <> "start" .= S.stamp (analysisStart r) <> "end" .= S.stamp (analysisEnd r) <>
  "turns" .= analysisTurns r <> "n_f" .= analysisNF r <> "n_c" .= analysisNC r <>
  E.pair "f_len" (lengthNumber (analysisNF r) (analysisFLen r)) <> E.pair "c_len" (lengthNumber (analysisNC r) (analysisCLen r)) <>
  "bursts" .= analysisBursts r <> "max_burst" .= analysisMaxBurst r <>
  "gap_med" .= analysisGapMedian r <> "ratio_med" .= fmap ratioMedian (analysisRatios r) <> "ratio_max" .= fmap ratioMaximum (analysisRatios r) <>
  "ends_q" .= analysisEndsQ r <> "n_q" .= analysisNQ r <> "paren_msgs" .= analysisParenMessages r <>
  E.pair "paren_ratio" (if analysisNC r == 0 then toEncoding (0 :: Int) else toEncoding (analysisParenRatio r)) <>
  "list" .= analysisList r <> "bold" .= analysisBold r <> "heading" .= analysisHeading r <> "mixed" .= analysisMixed r <>
  "short_f" .= analysisShortF r <> "q_hit" .= analysisQHit r <> "q_switch" .= analysisQSwitch r <> "q_miss" .= analysisQMiss r <>
  E.pair "quotes" (E.list quote (analysisQuotes r)) <> "pairs" .= analysisPairs r <>
  E.pair "first_c" (counter (analysisFirstC r)) <> E.pair "first_f" (counter (analysisFirstF r)) <>
  E.pair "marks" (E.list mark (analysisMarks r)) <> E.pair "sample" (E.list sample (analysisSample r))
  where
    lengthNumber n x | n == 0 || odd n = toEncoding (round x :: Integer)
                     | otherwise = toEncoding x
    counter = pairs . foldMap (\(key,n) -> fromText key .= n)
    quote (SavedQuote time text hit same) = pairs ("t" .= S.stamp time <> "q" .= text <> "hit" .= fmap S.stamp hit <> "same" .= same)
    mark (SavedMark time who kind hit) = pairs ("t" .= S.stamp time <> "who" .= speakerText who <> "kind" .= kindName kind <> "hit" .= hit)
    sample (SavedSample f c) = pairs ("f" .= S.stamp f <> "c" .= S.stamp c)

-- Whitespace for the persisted indent=1 file format. aeson owns all escaping,
-- scalar encoding and parsing; this only lays out its compact JSON tokens.
pretty :: Text -> Text
pretty = T.pack . outside 0 . T.unpack
  where
    newline depth = '\n' : replicate depth ' '
    outside _ [] = []
    outside depth ('"':rest) = '"' : quoted depth rest
    outside depth (open:close:rest) | [open,close] `elem` ["[]", "{}"] = open:close:outside depth rest
    outside depth (c:rest)
      | c == '[' || c == '{' = c : newline (depth + 1) ++ outside (depth + 1) rest
      | c == ']' || c == '}' = newline (depth - 1) ++ c : outside (depth - 1) rest
      | c == ',' = c : newline depth ++ outside depth rest
      | c == ':' = c : ' ' : outside depth rest
      | otherwise = c : outside depth rest
    quoted _ [] = []
    quoted depth ('\\':c:rest) = '\\':c:quoted depth rest
    quoted depth ('"':rest) = '"':outside depth rest
    quoted depth (c:rest) = c:quoted depth rest

encodeReports :: [SavedAnalysis] -> Text
encodeReports = pretty . TE.decodeUtf8 . BL.toStrict . E.encodingToLazyByteString . E.list encodeReport

toJson :: [LiveAnalysis] -> Text
toJson = encodeReports . map saved
