{-# LANGUAGE OverloadedStrings #-}
-- | Collect-specific day enumeration and unsealed user-picture storage.
-- Record locks, backup and sealed text storage remain in Store.
module Hikaru.Ochibo.Files
  ( parseCollectDay, timeMachineDays, picturePath, putPicture
  , savePictures, fillPictures, oldPictures, reportPictures ) where

import Hikaru.HouseDay (HouseDay, houseDayNamed)
import Control.Monad (foldM, forM, filterM)
import Data.List (isSuffixOf, isInfixOf)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import System.Directory
import System.FilePath ((</>),takeDirectory,takeFileName,normalise,dropTrailingPathSeparator)
import System.IO.Error (catchIOError,isDoesNotExistError,tryIOError)
import Hikaru.Compat.Base64 (decodeBase64)
import Hikaru.Compat.Diagnostic (Diagnostic(..),raiseDiagnostic,readSealedDiagnostic)
import Hikaru.Compat.FileSystem (pathExists,pathIsDirectory,pathIsFile)
import Hikaru.Compat.Json (PyString, pyStringToChars,pyStringFromText)
import Hikaru.Ochibo.Format (pictureNames, collectDay)
import Hikaru.Ochibo.Archivable (Archivable, archivedMessage)
import Hikaru.Ochibo.Read (globNames)
import Hikaru.Conversation.Line (LogPicture(..), Utterance(..))
import Data.Text (Text)

-- The unmodified Seal caller still passes PyString at this boundary.
parseCollectDay :: PyString -> Maybe Day
parseCollectDay = collectDay . T.pack . pyStringToChars

timeMachineDays :: FilePath -> IO [(HouseDay,FilePath)]
timeMachineDays root = do
  names <- globNames root
  pure (foldl insert []
    [(houseDayNamed day, root </> name) | name <- names
      , length name == 17, ".md.b64" `isSuffixOf` name
      , Just day <- [collectDay (T.pack (take 10 name))]])
  where
    -- First spelling fixes the position; the last spelling supplies the path.
    insert [] entry = [entry]
    insert (old:rest) entry
      | fst old == fst entry = entry : rest
      | otherwise = old : insert rest entry

picturePath :: FilePath -> Text -> FilePath
picturePath root name = dropTrailingPathSeparator(normalise(root </> T.unpack name))

putPicture :: FilePath -> Text -> Text -> IO Bool
putPicture root name payload = do
  let chars=T.unpack name; path=picturePath root name
  if ".." `isInfixOf` chars || take 1 chars=="/" then ioError (userError "invalid picture path") else pure ()
  exists<-pathExists path
  if exists then pure False else do
    if T.all (< '\x80') payload then pure () else ioError (userError "non-ASCII picture data")
    raw<-either (ioError . userError . show) pure (decodeBase64 (TE.encodeUtf8 payload))
    createDirectoryIfMissing True (takeDirectory path)
    let temporary=takeDirectory path </> (takeFileName path++".tmp")
    BS.writeFile temporary raw
    renameFile temporary path
    pure True

-- Only archivable messages reach here: a picture sent inside とばり
-- has no way into .おしばな.
savePictures :: FilePath -> [Archivable] -> IO (Int,[Text])
savePictures root = foldM save (0,[])
  where
    save state row = foldM one state (utterancePictures(archivedMessage row))
    one (count,failed) picture = do
      result<-tryIOError (putPicture root (pictureName picture) (pictureData picture))
      pure (case result of
        Left _ -> (count,failed++[pictureName picture])
        Right added -> (count+fromEnum added,failed))

referencedPictures :: FilePath -> IO (S.Set Text)
referencedPictures tm = do
  days<-timeMachineDays tm
  S.fromList . concat <$> mapM (fmap (pictureNames).readSealedDiagnostic.snd) days

fillPictures :: FilePath -> FilePath -> [Archivable] -> IO (Int,[Text],[Text])
fillPictures tm root rows = do
  names<-referencedPictures tm
  want<-filterM (fmap not.pathExists.picturePath root) (S.toAscList names)
  -- The baseline returns before constructing have if there are no missing
  -- paths. Duplicate picture names retain the last allowed message's data.
  if null want then pure (0,[],[]) else do
    let have=M.fromList [(pictureName picture,pictureData picture)
          | row<-rows,picture<-utterancePictures(archivedMessage row)]
    foldM (one have) (0,[],[]) want
  where
    one have (count,missing,failed) name = case M.lookup name have of
      Nothing -> pure (count,missing++[name],failed)
      Just data_ -> do
        result<-tryIOError (putPicture root name data_)
        pure (case result of
          Left _ -> (count,missing,failed++[name])
          Right added -> (count+fromEnum added,missing,failed))

oldPictures :: FilePath -> FilePath -> IO Int
oldPictures tm root = do
  names<-referencedPictures tm
  dirs<-listDirectory root `catchIOError` \err ->
    if isDoesNotExistError err then pure [] else ioError err
  counts<-forM dirs $ \dir -> do
    isDir<-pathIsDirectory (root</>dir)
    if not isDir then pure 0 else do
      files<-listDirectory(root</>dir)
      sum <$> forM files (\file -> do
        isFile<-pathIsFile(root</>dir</>file)
        pure (fromEnum (isFile && take 1 file/="." && S.notMember(T.pack(dir++"/"++file)) names)))
  pure(sum counts)

reportPictures :: (Text -> IO ()) -> Int -> [Text] -> [Text] -> Text -> IO ()
reportPictures output n missing failed word = do
  if n/=0 then output(T.concat["画像 ",num n," 枚を .おしばな/ に",word,"。\n"]) else pure ()
  if null missing then pure () else output(T.concat["原文にあって会話記録に見つからない画像 ",num(length missing)," 枚: ",T.intercalate "、" missing,"\n"])
  if null failed then pure () else do
    output(T.concat["置けなかった画像 ",num(length failed)," 枚: ",T.intercalate "、" failed,"\n"])
    raiseDiagnostic (Diagnostic "OSError" (pyStringFromText
      ("failed to save pictures: " <> T.intercalate ", " failed)))
  where num = T.pack . show
