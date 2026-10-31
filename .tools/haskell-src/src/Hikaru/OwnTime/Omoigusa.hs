{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hikaru.OwnTime.Omoigusa
  ( omoigusaDir, renderPiece, keepPieces, stagePieces, mapPieces
  ) where

import Control.Monad (forM, forM_, unless)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf, sort)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Hikaru.Config (Config(dataRoot))
import Hikaru.OwnTime.Engine (wiMaterial)
import Hikaru.OwnTime.Michikusa
import Hikaru.OwnTime.Records
import Hikaru.OwnTime.Runtime
import Hikaru.OwnTime.Store hiding (count)
import Hikaru.OwnTime.Trigger
import Hikaru.OwnTime.Errors (invalidFile)
import qualified Hikaru.Store.Coordinator as Store
import System.Directory (canonicalizePath, createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>))

-- 思ひ草 -------------------------------------------------------------------------------
--
-- 自分の時間に書いたもの・作ったもの。一篇一つ、.思ひ草/<名前>.json.b64（稿をすべて持つ元）と
-- .思ひ草/<名前>.md.b64（読むための形。fuutou show omoigusa:<名前>）。どちらも封をする。稿は消さずに足していく
-- （夕顔のみせけちのように、どう変わってきたかを振り返れるように）。同じ運行の稿は二度足さない。

omoigusaDir :: RunEnv -> FilePath
omoigusaDir env = dataRoot (reConfig env) </> ".思ひ草"

-- | landing.json の works を、思ひ草にしまう。返すのは、一篇ごとの、しまったあとの稿の数。
keepPieces :: RunEnv -> UTCTime -> Run -> LandingFile -> IO [(MaterialId, Int)]
keepPieces env now r lf = do
  let rdir = runDirOf (reWork env) r
      RunId rid = runId r
      dir = omoigusaDir env
      okIds = map wiMaterial (lfWorks lf)
  createDirectoryIfMissing True dir
  fmap concat $ forM [ (pid, x) | x <- lfWorkRaw lf, Just pid <- [wdId x], pid `elem` okIds ] $ \(pid, x) -> do
    let jsonPath = dir </> (T.unpack (materialText pid) ++ ".json.b64")
        field' get = maybe A.Null A.String (get x)
    let renderedPath = dir </> (T.unpack (materialText pid) ++ ".md.b64")
    withRunRecord env renderedPath $ \store ->
      Store.withRecordLock store False jsonPath $ \_ -> do
        oldRead <- readSealedJson jsonPath
        case oldRead of
         PieceBroken why -> do
          runLog (reDir env) now ("思ひ草「" ++ T.unpack (materialText pid) ++ "」的档读不开（" ++ why ++ "）：不覆盖，这一格的稿留在作业场 " ++ rdir ++ "，等人看")
          pure []
         _ -> do
           let old = loadedOr oldRead
           rendered <- missingAsNothing (readRecordBytes renderedPath)
           forM_ rendered $ \actual -> do
             let expected = TE.encodeUtf8 . renderPiece <$> old
             unless (maybe False (\text -> actual == Right text) expected) $
               ioError (userError ("piece render changed outside OwnTime: " ++ renderedPath ++ "; run materials retained"))
           -- 稿の本文：作業場の中のファイルだけ読む
           draftText <- case wdFile x of
             Just rel -> do
               target <- canonicalizePath (rdir </> T.unpack rel)
               base <- canonicalizePath rdir
               if not ((base ++ "/") `isPrefixOfS` target) then pure Nothing else do
                 bytes <- B.readFile target
                 pure (Just (T.unpack rel, TE.decodeUtf8With (\_ _ -> Just '?') bytes))
             Nothing -> pure Nothing
           let prevDrafts = case old >>= KM.lookup "drafts" of { Just (A.Array ds) -> foldr (:) [] ds; _ -> [] }
               lastText = case reverse prevDrafts of { (A.Object d : _) -> textField "text" d; _ -> Nothing }
               sameRun = any (\d -> case d of { A.Object o' -> KM.lookup "run" o' == Just (A.toJSON rid); _ -> False }) prevDrafts
               newDraft = case draftText of
                 Just (file, t) | Just t /= lastText, not sameRun -> Just (A.object
                   [ "at" A..= iso8601Show (runStartedAt r), "run" A..= rid, "file" A..= takeFileNameS file, "text" A..= t
                   , "change" A..= field' wdChange ])
                 _ -> Nothing
               drafts = prevDrafts ++ maybe [] pure newDraft
               -- 試しで走らせた結果は、稿の改めとは別に残す（正文が変わらなくても、走らせたことは経験。どの稿を走らせたかも）
               prevRuns = case old >>= KM.lookup "runs" of { Just (A.Array rs) -> foldr (:) [] rs; _ -> [] }
               runAlready = any (\v -> case v of { A.Object o' -> KM.lookup "run" o' == Just (A.toJSON rid); _ -> False }) prevRuns
               newRun = case wdRan x of
                 Just b | not runAlready -> Just (A.object
                   [ "run" A..= rid, "at" A..= iso8601Show (runStartedAt r), "draft" A..= length drafts
                   , "ran" A..= b, "note" A..= field' wdRunNote ])
                 _ -> Nothing
               runsL = prevRuns ++ maybe [] pure newRun
               keep get k dflt = case get x of
                 Just t -> A.String t
                 Nothing -> fromMaybe dflt (old >>= KM.lookup k)
               piece = KM.fromList
                 [ ("id", A.toJSON pid)
                 , ("kind", keep wdKind "kind" (A.String "writing"))
                 , ("title", keep wdTitle "title" (A.toJSON pid))
                 , ("origin", keep wdOrigin "origin" (A.String "mine"))
                 , ("from", fromMaybe (A.String (T.pack ("run-" ++ show rid))) (old >>= KM.lookup "from"))
                 , ("status", keep (fmap pieceStatusText . wdStatus) "status" (A.String "writing"))
                 , ("memo", keep wdMemo "memo" A.Null)
                 , ("how_to_run", keep wdHowToRun "how_to_run" A.Null)
                 , ("shown", fromMaybe A.Null (old >>= KM.lookup "shown"))
                 , ("drafts", A.toJSON drafts)
                 , ("runs", A.toJSON runsL) ]
           Store.backup store jsonPath
           Store.backup store renderedPath
           writeSealed jsonPath (BL.toStrict (A.encode (A.Object piece)))
           writeTextRecord renderedPath (renderPiece piece)
           runLog (reDir env) now ("思ひ草「" ++ T.unpack (materialText pid) ++ "」" ++ (if isJust newDraft then "存了第 " ++ show (length drafts) ++ " 稿" else "没有新的稿（只更新了状态和覚え書き）"))
           pure [(pid, length drafts)]
  where
    isPrefixOfS a b = take (length a) b == a
    takeFileNameS = reverse . takeWhile (/= '/') . reverse

-- | 読むための形。いまの稿の全文と、稿のうつりかわり。
renderPiece :: A.Object -> T.Text
renderPiece o =
  let t k = fromMaybe "" (textField k o)
      statusWord = pieceStatusWord (pieceStatusOf (t "status"))
      kindWord = if t "kind" == "making" then "作るもの" else "書くもの"
      drafts = case KM.lookup "drafts" o of { Just (A.Array ds) -> [ d | A.Object d <- foldr (:) [] ds ]; _ -> [] }
      n = length drafts
      line i d = "- 第 " <> T.pack (show i) <> " 稿（" <> T.take 16 (fromMaybe "" (textField "at" d)) <> "）："
                 <> fromMaybe "（改めの一言なし）" (textField "change" d)
      runs = case KM.lookup "runs" o of { Just (A.Array rs) -> [ x | A.Object x <- foldr (:) [] rs ]; _ -> [] }
      runLine x = "- " <> T.take 16 (fromMaybe "" (textField "at" x)) <> "、第 " <> T.pack (maybe "?" (\v -> case v of { A.Number count -> show (truncate count :: Int); _ -> "?" }) (KM.lookup "draft" x)) <> " 稿を走らせた："
                  <> (case KM.lookup "ran" x of { Just (A.Bool True) -> "動いた（起きたわたしの申告）"; Just (A.Bool False) -> "動かなかった（起きたわたしの申告）"; _ -> "？" })
                  <> maybe "" ("。" <>) (textField "note" x)
      current = case reverse drafts of
        (d : _) -> "## いまの稿（第 " <> T.pack (show n) <> " 稿、" <> fromMaybe "" (textField "file" d) <> "）\n\n" <> fromMaybe "" (textField "text" d) <> "\n"
        [] -> "（まだ稿がない）\n"
  in T.unlines $
       [ "# " <> t "title" <> "（" <> kindWord <> "、" <> statusWord <> "）", "" ]
       ++ [ "覚え書き：" <> t "memo" | not (T.null (t "memo")) ]
       ++ [ "走らせ方：" <> t "how_to_run" | not (T.null (t "how_to_run")) ]
       ++ [ "ユーザーに見せた：" <> t "shown" | not (T.null (t "shown")) ]
       ++ [ "", current, "## 稿のうつりかわり", "" ]
       ++ zipWith line [1 :: Int ..] drafts
       ++ (if null runs then [] else [ "", "## 試し", "" ] ++ map runLine runs)

-- | 書きかけ・置いてある一篇を、作業場に写す（いまの稿と覚え書き）。brief に並べるための一覧を返す。
stagePieces :: RunEnv -> FilePath -> IO [(T.Text, T.Text, PieceStatus)]
stagePieces env rdir = mapPieces env $ \o ->
    case textField "id" o of
      Just pid | pieceStatusOf (fromMaybe "writing" (textField "status" o)) `elem` [Writing, Resting] -> do
        let pdir = rdir </> "思ひ草" </> T.unpack pid
            drafts = case KM.lookup "drafts" o of { Just (A.Array ds) -> [ d | A.Object d <- foldr (:) [] ds ]; _ -> [] }
        createDirectoryIfMissing True pdir
        case reverse drafts of
          (d : _) | Just txt <- textField "text" d ->
            B.writeFile (pdir </> T.unpack (fromMaybe "draft.md" (textField "file" d))) (TE.encodeUtf8 txt)
          _ -> pure ()
        B.writeFile (pdir </> "覚え書き.md") (TE.encodeUtf8 (fromMaybe "" (textField "memo" o)))
        pure [(pid, fromMaybe pid (textField "title" o), pieceStatusOf (fromMaybe "writing" (textField "status" o)))]
      _ -> pure []


-- Keep each read adjacent to its action; staging writes before the next read.
mapPieces :: RunEnv -> (A.Object -> IO [a]) -> IO [a]
mapPieces env action = do
  let dir = omoigusaDir env
  names <- maybe [] id <$> missingAsNothing (listDirectory dir)
  fmap concat $ forM [ n | n <- sort names, ".json.b64" `isSuffixOf` n ] $ \n -> do
    piece <- readSealedJson (dir </> n)
    case piece of
      PieceMissing -> pure []
      PieceLoaded object -> action object
      PieceBroken why -> invalidFile (dir </> n) why

