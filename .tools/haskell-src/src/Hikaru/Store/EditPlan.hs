{-# LANGUAGE OverloadedStrings #-}
-- | Pure editing decisions. Force has no function accepting VersionConflict.
module Hikaru.Store.EditPlan
  ( Fingerprint(..), VersionConflict(..), VersionMatch, matchVersion, conflictText
  , Force(..), ContentProblem(..), ContentReview(..), ContentFailure, ContentPermit
  , lengthReview, reviewContent, forceContent, failureText, permittedPlain, contentWarnings, forcedNotices
  , closeActions, closeWrite, openActions, reopenActions, cleanupActions, joinedPlain
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Hikaru.RecordText as RecordText
import Hikaru.Sealed (Plain(..))
import Hikaru.Store.Actions
import Hikaru.Store.Work (WorkFile)

newtype Fingerprint = Fingerprint Text deriving (Eq, Show)
data VersionConflict = MissingFingerprint | ChangedElsewhere deriving (Eq, Show)
-- A data constructor rather than a newtype also prevents Coercible forgery.
data VersionMatch = VersionMatch

matchVersion :: Maybe Fingerprint -> Fingerprint -> Either VersionConflict VersionMatch
matchVersion Nothing _ = Left MissingFingerprint
matchVersion (Just expected) actual
  | expected == actual = Right VersionMatch
  | otherwise = Left ChangedElsewhere

conflictText :: String -> VersionConflict -> Text
conflictText name MissingFingerprint = T.concat
  ["open のときの封の記録（.sha1）がありません。open が古い版だったか、作業フォルダが片づけられています。\n"
  ,"いまの封と比べられないので止めました（平文は残してあります）。\n"
  ,"`.tools/bin/fuutou reopen ",T.pack name,"` で、いまの封を取り直してください（--force でも、ここは越えられません）。"]
conflictText name ChangedElsewhere = T.concat
  ["open のあとで、封がほかから書き換えられています（別の会話が直した？）。止めました（平文は残してあります）。\n"
  ,"`.tools/bin/fuutou reopen ",T.pack name,"` で、いまの封を取り直してください"
  ,"（あなたが直したところは差分で表示され、別の名前で残ります。それを入れ直してから close）。\n"
  ,"--force でも、ここは越えられません（ほかの会話の直しが消えるので）。"]

data Force = RespectChecks | ForceContent deriving (Eq, Show)
data ContentProblem = ContentProblem Text [Text]
data ContentReview = ContentReview (Maybe ContentProblem) [Text]
data ContentFailure = ContentFailure Plain ContentProblem [Text]
data ContentPermit = ContentPermit Plain [Text] [Text]

lengthReview :: Plain -> Plain -> ContentReview
lengthReview (Plain old) (Plain new) = ContentReview problem []
  where
    problem
      | T.null (RecordText.strip new) || 2 * T.length new < T.length old = Just (ContentProblem
          "中身が空か、元の半分より短くなっています。止めました（平文は残してあります。--force は、ユーザーがこの上書きをはっきり認めたときだけ）。" [])
      | otherwise = Nothing

reviewContent :: Plain -> ContentReview -> Either ContentFailure ContentPermit
reviewContent plain (ContentReview Nothing warnings) = Right (ContentPermit plain warnings [])
reviewContent plain (ContentReview (Just problem) warnings) = Left (ContentFailure plain problem warnings)

forceContent :: Force -> Either ContentFailure ContentPermit -> Either ContentFailure ContentPermit
forceContent RespectChecks result = result
forceContent ForceContent (Left (ContentFailure plain (ContentProblem _ notices) warnings)) =
  Right (ContentPermit plain warnings notices)
forceContent ForceContent result = result

failureText :: ContentFailure -> Text
failureText (ContentFailure _ (ContentProblem message _) _) = message
permittedPlain :: ContentPermit -> Plain
permittedPlain (ContentPermit plain _ _) = plain
contentWarnings :: ContentPermit -> [Text]
contentWarnings (ContentPermit _ warnings _) = warnings
forcedNotices :: ContentPermit -> [Text]
forcedNotices (ContentPermit _ _ notices) = notices

-- Both independent gates are required to reach a changed close's commit.
closeActions :: VersionMatch -> ContentPermit -> Bool -> FilePath -> Either Text [Action progress]
closeActions version permit exists target =
  ([Backup target | exists] ++) . writeActions <$> closeWrite version permit target

closeWrite :: VersionMatch -> ContentPermit -> FilePath -> Either Text RecordWrite
closeWrite VersionMatch permit target = prepareWrite target (permittedPlain permit)

openActions :: WorkFile -> WorkFile -> WorkFile -> Plain -> [Action progress]
openActions plain base mine text = [WriteWork plain text, WriteWork base text, RemoveWork mine]

reopenActions :: WorkFile -> WorkFile -> WorkFile -> WorkFile -> Plain -> Plain -> Fingerprint -> [Action progress]
reopenActions mine plain base sha old latest (Fingerprint fingerprint) =
  [WriteWork mine old, WriteWork plain latest, WriteWork base latest, WriteWork sha (Plain fingerprint)]

cleanupActions :: [WorkFile] -> [Action progress]
cleanupActions = map RemoveWork

joinedPlain :: Plain -> Plain -> Plain
joinedPlain (Plain old) (Plain addition) = Plain
  (T.dropWhileEnd (=='\n') old <> "\n\n" <> T.dropAround (=='\n') addition <> "\n")
