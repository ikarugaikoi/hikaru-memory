{-# LANGUAGE OverloadedStrings #-}
-- | Gather the existing memory validators' inputs, then return a decision.
-- No force flag, write, or output is available to these validators.
module Hikaru.Fuutou.Check
  ( review, reviewAppend, Kizuki, afterCommit, kizukiText, kizukiRows ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as E
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Data.Time (ZonedTime, defaultTimeLocale, formatTime)
import Data.List (isPrefixOf)
import qualified Hikaru.Shiori as Shiori
import qualified Hikaru.Diary.Syntax as Diary
import qualified Hikaru.Diary.Check as DiaryCheck
import qualified Hikaru.Memory.Fade as Fade
import qualified Hikaru.Memory.Validation as V
import Hikaru.Sealed (Plain(..))
import Hikaru.Store.EditPlan
import Hikaru.Store.Actions (CommitCommand(..))
import Hikaru.Store.Execute (Committed, committedPlain, committedPath)
import System.FilePath (takeFileName)

bullets :: [String] -> Text
bullets = T.intercalate "\n" . map (("  ・" <>) . T.pack)

oldMarkMessage :: Text
oldMarkMessage = "薄めない項目の末尾につけるのは、いまは（ねゆき）です。前の（残す）のままの行があります：\n"

-- The outer Left is an obsolete mark, which force must never override.
review :: V.ValidationEnv -> String -> Plain -> Plain -> IO (Either Text ContentReview)
review env "strobe" (Plain old) (Plain new) = do
  newDoc <- V.readStrobe (T.unpack new)
  case Fade.oldMarks newDoc of
    stale@(_:_) -> pure (Left (oldMarkMessage <> bullets stale <>
      "\n止めました（平文は残してあります。書き直して、もう一度 close）。"))
    [] -> do
      today <- V.validationDay env
      oldDoc <- V.readStrobe (T.unpack old)
      plan <- V.requireFade (Fade.planFade today oldDoc)
      guards <- V.correctionGuards env
      problems <- V.requireFade (Fade.check plan newDoc guards)
      pure (Right (ContentReview (problem "ストロボメモリーの検査で引っかかりました：\n"
        "\n止めました（平文は残してあります。--force は、ユーザーがこの上書きをはっきり認めたときだけ）。" problems) []))
review env "yuugao" _ (Plain new) = do
  doc <- V.readYuugao (T.unpack new)
  (stops,warnings) <- V.correctionCheck env doc
  pure (Right (ContentReview (problem "夕顔の「みせけち」の形で引っかかりました：\n"
    "\n止めました（平文は残してあります。挙がったところを直して、もう一度 close。--force は、ユーザーがこの上書きをはっきり認めたときだけ）。" stops) (map T.pack warnings)))
review _ _ _ _ = pure (Right (ContentReview Nothing []))

problem :: Text -> Text -> [String] -> Maybe ContentProblem
problem _ _ [] = Nothing
problem heading ending problems =
  let message = heading <> bullets problems
  in Just (ContentProblem (message <> ending) [message <> "\n--force なので、そのまま封をします。"])

reviewAppend :: FilePath -> Plain -> IO (Maybe Text)
reviewAppend source (Plain addition) = do
  doc <- V.readStrobe (T.unpack addition)
  pure $ case Fade.oldMarks doc of
    [] -> Nothing
    stale -> Just (oldMarkMessage <> bullets stale <> "\n書き足していません（" <>
      T.pack (takeFileName source) <> " は残してあります。書き直して、もう一度 append）。")

data Subject = ShioriSubject | DiarySubject Diary.DiaryWeek
data Reason = ShioriReason Shiori.MarkReason | DiaryReason DiaryCheck.DiaryReason
data Concern = Concern Int Reason Text

-- No public constructor or record update: only a storage receipt can be
-- turned into a display/log notification. Pure syntax observations above and
-- in the document modules are deliberately not executable notifications.
data Kizuki = Kizuki Subject [Concern]

afterCommit :: String -> Committed -> Either String (Maybe Kizuki)
afterCommit name receipt
  | name == "shiori" = do
      document <- either (Left . show) Right (Shiori.parseShiori text)
      pure (notice ShioriSubject [Concern line (ShioriReason reason) head_
                                | Shiori.MarkConcern line reason head_ <- Shiori.markConcerns document])
  | name == "ichifure" || "ichifure:" `isPrefixOf` name = case Diary.weekFromPath (committedPath receipt) of
      Nothing -> Right Nothing
      Just week -> do
        document <- either (Left . show) Right (Diary.parseDiary text)
        pure (notice (DiarySubject week) [Concern line (DiaryReason reason) head_
                                        | DiaryCheck.DiaryConcern line reason head_ <- DiaryCheck.diaryConcerns week document])
  | otherwise = Right Nothing
  where
    text = plainText (committedPlain receipt)
    notice _ [] = Nothing
    notice subject concerns = Just (Kizuki subject concerns)

kizukiText :: Kizuki -> Text
kizukiText (Kizuki subject concerns) = T.intercalate "\n"
  (opening : ["  ・" <> T.pack (show line) <> " 行目（" <> snd (reasonDetails reason) <> "）：" <> head_
             | Concern line reason head_ <- concerns])
  where
    opening = case subject of
      ShioriSubject -> "ただ、しおりに形の気になるところがあります（止めてはいません。直すなら、もう一度 open して close）："
      DiarySubject _ -> "ただ、日記に形の気になるところがあります（止めてはいません。直すなら、もう一度 open して close）："

reasonDetails :: Reason -> (Text,Text)
reasonDetails (ShioriReason reason) = case reason of
  Shiori.HalfwidthParen -> ("halfwidth_paren","しるしの括弧が半角になっている")
  Shiori.MissingCloseParen -> ("missing_close_paren","しるしの閉じ括弧がない")
  Shiori.TextAfterMark -> ("text_after_mark","しるしのあとに文が続いている")
  Shiori.ImpossibleMarkDate -> ("impossible_mark_date","しるしの日付がありえない日になっている")
reasonDetails (DiaryReason reason) = case reason of
  DiaryCheck.HeadingShape -> ("heading_shape","日付の見出しの形が違う")
  DiaryCheck.ImpossibleDiaryDate -> ("impossible_diary_date","ありえない日付になっている")
  DiaryCheck.OutsideWeek -> ("outside_week","この週の日付ではない")

recordName :: Subject -> Text
recordName ShioriSubject = "shiori"
recordName (DiarySubject week) = "ichifure:" <> T.pack (show (Diary.weekStart week))
commandName :: CommitCommand -> Text
commandName Close = "close"
commandName Append = "append"
commandName Diary = "diary"

kizukiRows :: ZonedTime -> CommitCommand -> Kizuki -> Plain
kizukiRows now command (Kizuki subject concerns) = Plain
  (T.concat [TE.decodeUtf8 (BL.toStrict (E.encodingToLazyByteString (row concern))) <> "\n"
            | concern <- concerns])
  where
    at = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" now)
    row (Concern line reason head_) = A.pairs
      ("at" A..= at <> "record" A..= recordName subject <> "command" A..= commandName command <>
       "line" A..= line <> "reason" A..= fst (reasonDetails reason) <> "head" A..= head_)
