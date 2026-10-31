{-# LANGUAGE OverloadedStrings #-}
-- | Read observations, ask the pure editing policy, interpret its actions.
-- The few PyString conversions here are the retained public/diagnostic/diff
-- adapters; the transaction and its force decision consume only Plain/Text.
module Hikaru.Store.Edit
  ( EditEnv(..), EditStopped(..), stopEdit, sha1Of
  , openRecord, reopenRecord, closeRecord, closeRecordWithNotice, appendRecord, joined ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)
import Crypto.Hash (Digest, SHA1, hash)
import qualified Data.ByteString as BS
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void, absurd)
import Hikaru.Compat.Diff (UnifiedOptions(..),defaultUnifiedOptions,unifiedDiff,splitLF)
import Hikaru.Compat.Exception (fromPython)
import Hikaru.Compat.FileSystem (pathExists)
import Hikaru.Compat.Json (PyString,pyStringFromText,pyStringToText,pyStringFromChars,pyStringToChars)
import qualified Hikaru.RecordText as RecordText
import Hikaru.Sealed (Plain(..))
import Hikaru.Store.Actions
import Hikaru.Store.Coordinator (StoreEnv)
import Hikaru.Store.EditPlan
import Hikaru.Store.Execute (execute, locked, Committed, commit, readCommitted)
import Hikaru.Store.Work
import System.FilePath (takeFileName,takeExtension)

newtype EditStopped = EditStopped PyString deriving Show
instance Exception EditStopped
stopEdit :: PyString -> IO a
stopEdit = throwIO . EditStopped
stopText :: Text -> IO a
stopText = stopEdit . pyStringFromText

data EditEnv = EditEnv
  { editStore :: StoreEnv
  , editWork :: WorkEnv
  , editReadRecord :: FilePath -> IO Plain
  , editReview :: String -> Plain -> Plain -> IO (Either Text ContentReview)
  , editAppendCheck :: FilePath -> Plain -> IO (Maybe Text)
  , editOutput :: Text -> IO ()
  , editAfterCommit :: String -> CommitCommand -> Committed -> IO ()
  }

run :: EditEnv -> [Action Void] -> IO ()
run env = execute (editStore env) absurd (editOutput env)
held :: EditEnv -> String -> FilePath -> IO a -> IO a
held env name target = locked (editStore env) (recordLock name target)
work :: EditEnv -> String -> WorkPart -> IO WorkFile
work env = workFile (editWork env)
readWork :: WorkFile -> IO Plain
readWork = readPlainFile . workFilePath
say :: EditEnv -> Text -> IO ()
say env text = run env [Say text]
pathText :: FilePath -> Text
pathText = T.pack

sha1Of :: FilePath -> IO Fingerprint
sha1Of path = do
  exists <- pathExists path
  Fingerprint <$> if exists
    then T.pack . show . (hash :: BS.ByteString -> Digest SHA1) <$> BS.readFile path
    else pure ""

openRecord :: EditEnv -> String -> FilePath -> Bool -> IO ()
openRecord env name target force = do
  plain <- work env name Draft
  exists <- pathExists (workFilePath plain)
  when (exists && not force) $ stopText $ T.concat
    ["同じ名前の平文がもう残っています: ",pathText (workFilePath plain),"\n"
    ,"前に open して close していないものです。close が「ほかから書き換えられています」で止まった続きなら、\n"
    ,"`.tools/bin/fuutou reopen ",pathText name,"`（直したところを残したまま、いまの封を取り直す）。\n"
    ,"中を見て、その直しがもう封に入っているか、もう要らないと分かってから、--force で上書き（または clean）。"]
  text <- held env name target $ do
    current <- editReadRecord env target
    savedHash <- work env name FingerprintFile
    Fingerprint fingerprint <- sha1Of target
    run env [WriteWork savedHash (Plain fingerprint)]
    pure current
  -- Preserve resolution/write order, including errors resolving later paths.
  run env [WriteWork plain text]
  base <- work env name Base
  run env [WriteWork base text]
  mine <- work env name Mine
  run env [RemoveWork mine, Say (pathText (workFilePath plain))]

reopenRecord :: EditEnv -> String -> FilePath -> IO ()
reopenRecord env name target = do
  plain <- work env name Draft
  sha <- work env name FingerprintFile
  base <- work env name Base
  mine <- work env name Mine
  exists <- pathExists (workFilePath plain)
  unless exists $ stopText ("開いていません: " <> pathText (workFilePath plain) <> "（まだなら、ふつうに open を）")
  own <- readWork plain
  hasBase <- pathExists (workFilePath base)
  reference <- if hasBase then Just <$> readWork base else pure Nothing
  (latest,fingerprint) <- held env name target $ (,) <$> editReadRecord env target <*> sha1Of target
  run env (reopenActions mine plain base sha own latest fingerprint)
  say env (pathText (workFilePath plain))
  say env ("いまの封を取り直して、同じ場所に出しました。あなたが直した版は " <>
    pathText (takeFileName (workFilePath mine)) <> " に残してあります（close で消えます）。")
  let asLines = splitLF . pyStringFromText . plainText
      diff = drop 2 (unifiedDiff (defaultUnifiedOptions {contextLines=0,lineTerm=""})
        (asLines (maybe latest id reference)) (asLines own))
  if null diff then say env "あなたが直したところはありません（開いたときのまま）。このまま close してかまいません。"
  else do
    unless hasBase $ say env "（開いたときの版が残っていないので、いまの封との違いを出しています。ほかの会話の直しも、逆向きに混ざっています）"
    say env "あなたが直したところ（- が元、+ が直したあと。@@ は元の行の番号）。これを、取り直した版に入れ直してから close："
    rendered <- traverse (fromPython . pyStringToText) (take 120 diff)
    run env [Say ("  " <> if T.length line <= 240 then line else T.take 240 line <> "…") | line <- rendered]
    when (length diff > 120) $ say env ("  …（あと " <> T.pack (show (length diff-120)) <>
      " 行。全部は " <> pathText (workFilePath mine) <> " に）")

closeRecord :: EditEnv -> String -> FilePath -> Bool -> IO ()
closeRecord = closeRecordWithNotice (\_ _ -> [])

closeRecordWithNotice :: (PyString -> PyString -> [PyString]) -> EditEnv -> String -> FilePath -> Bool -> IO ()
closeRecordWithNotice notice env name target force = do
  plain <- work env name Draft
  savedHash <- work env name FingerprintFile
  exists <- pathExists (workFilePath plain)
  unless exists $ stopText ("開いていません: " <> pathText (workFilePath plain))
  new <- readWork plain
  (changed,receipt,warnings,notices) <- held env name target $ do
    hasHash <- pathExists (workFilePath savedHash)
    -- Missing hashes stop before reading the target, just like changed hashes.
    unless hasHash $ stopText (conflictText name MissingFingerprint)
    expected <- Fingerprint . RecordText.strip . plainText <$> readWork savedHash
    actual <- sha1Of target
    matched <- either (stopText . conflictText name) pure (matchVersion (Just expected) actual)
    old <- editReadRecord env target
    if plainText new == plainText old then do
      -- No new write is needed. Only an existing seal can supply a receipt;
      -- the missing-shiori template must never be mistaken for a saved file.
      present <- if name == "shiori" || name == "ichifure" || "ichifure:" `isPrefixOf` name
                 then pathExists target else pure False
      receipt <- if present then Just <$> readCommitted target else pure Nothing
      pure (False,receipt,[],[])
    else do
      when (name /= "strobe") $ do
        _ <- approve new (lengthReview old new)
        pure ()
      review <- editReview env name old new >>= either stopText pure
      permit <- approve new review
      present <- pathExists target
      plan <- either stopText pure (closeWrite matched permit target)
      run env [Backup target | present]
      committed <- commit (editStore env) plan
      pure (True,Just committed,contentWarnings permit,notice (pyStringFromText (plainText old)) (pyStringFromText (plainText new)))
  -- Resolve both paths before the first removal, retaining partial-failure order.
  base <- work env name Base
  mine <- work env name Mine
  run env (cleanupActions [plain,savedHash,base,mine])
  say env (if changed then "封をし直しました: " <> pathText (takeFileName target) <> "（平文は消しました）"
    else "変更なし。平文は消しました。")
  unless (null warnings) $ say env
    ("「みせけち」で気になるところ（封はしました。直せるなら、もう一度 open yuugao → close yuugao で）：\n" <>
      T.intercalate "\n" (map ("  ・" <>) warnings))
  traverse (fromPython . pyStringToText) notices >>= mapM_ (say env)
  mapM_ (editAfterCommit env name Close) receipt
  where
    approve new review = do
      permit <- either (stopText . failureText) pure
        (forceContent (if force then ForceContent else RespectChecks) (reviewContent new review))
      run env (map Say (forcedNotices permit))
      pure permit

-- Compatibility adapter for the unchanged diary/read command boundary.
joined :: PyString -> PyString -> PyString
joined old added = pyStringFromChars (dropWhileEnd (=='\n') (pyStringToChars old) ++ "\n\n"
  ++ dropWhileEnd (=='\n') (dropWhile (=='\n') (pyStringToChars added)) ++ "\n")

appendRecord :: EditEnv -> String -> FilePath -> FilePath -> IO ()
appendRecord env name target source = do
  when (takeExtension source == ".b64") $ stopText
    ("封をしたファイルは足せません: " <> pathText (takeFileName source) <> "（平文を渡してください）")
  addition <- readPlainFile source
  when (T.null (RecordText.strip (plainText addition))) (stopText "足す中身が空です。")
  when (name == "strobe") $ editAppendCheck env source addition >>= maybe (pure ()) stopText
  receipt <- held env name target $ do
    old <- editReadRecord env target
    exists <- pathExists target
    plan <- either (ioError . userError . T.unpack) pure (prepareWrite target (joinedPlain old addition))
    run env [Backup target | exists]
    commit (editStore env) plan
  run env [RemoveSource source, Say ("書き足しました: " <> pathText (takeFileName target) <>
    "（" <> pathText (takeFileName source) <> " は消しました）")]
  editAfterCommit env name Append receipt
