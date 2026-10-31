{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI #-}
-- | Text-free scan checkpoints. Missing, stale or corrupt data can be rebuilt;
-- cache IO failures propagate just like source and consumer failures.
module Hikaru.Choubo.Store
  ( scanConsumer, invalidateConversation, invalidateAll
  , contextDigest ) where

import Control.Exception
import Control.Monad (unless,when)
import Data.Bits ((.&.))
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord,isAscii,isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Data.Unique (newUnique,hashUnique)
import Foreign.C.Error (throwErrnoIfMinus1,throwErrnoIfMinus1_,getErrno,eWOULDBLOCK,throwErrno)
import Foreign.C.String (CString,withCString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Hikaru.Choubo.Consumer
import Hikaru.Choubo.Reader
import Hikaru.Choubo.Types
import qualified Hikaru.Choubo.Canonical as Canonical
import System.Directory (makeAbsolute)
import System.FilePath ((</>),takeDirectory,takeFileName,splitDirectories,normalise,joinPath)
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files hiding (fileSize)
import System.Posix.IO
import System.Posix.Types (Fd(..))
import System.Posix.User (getEffectiveUserID)
import System.Posix.Process (getProcessID)

data Entry = Entry
  { entryVersion :: Word32, entryContext :: Text, entryStamp :: FileStamp
  , entryHead :: Text, entryBoundary :: Text, entryPrefix :: Maybe Text
  , entryPoint :: ResumePoint, entryStable :: Projection, entryResult :: Projection
  , entryGeneration :: Text, entryCoverage :: Integer, entryStopped :: Bool, entryTail :: Bool }
instance ToJSON Entry where
  toEncoding e=pairs ("boundary_checksum" .= entryBoundary e <> "context_checksum" .= entryContext e
    <> "coverage_position" .= entryCoverage e <> "generation_checksum" .= entryGeneration e
    <> "head_checksum" .= entryHead e <> "prefix_checksum" .= entryPrefix e
    <> "result" .= entryResult e <> "resume" .= entryPoint e <> "rule_version" .= entryVersion e
    <> "source" .= entryStamp e <> "stable" .= entryStable e <> "stopped_boolean" .= entryStopped e
    <> "tail_boolean" .= entryTail e)
  toJSON e=object["rule_version" .= entryVersion e,"context_checksum" .= entryContext e
    ,"source" .= entryStamp e,"head_checksum" .= entryHead e,"boundary_checksum" .= entryBoundary e
    ,"prefix_checksum" .= entryPrefix e,"resume" .= entryPoint e
    ,"stable" .= entryStable e,"result" .= entryResult e,"generation_checksum" .= entryGeneration e
    ,"coverage_position" .= entryCoverage e,"stopped_boolean" .= entryStopped e,"tail_boolean" .= entryTail e]
instance FromJSON Entry where
  parseJSON=withObject "entry" $ \o->Entry <$> o .: "rule_version" <*> o .: "context_checksum"
    <*> o .: "source" <*> o .: "head_checksum" <*> o .: "boundary_checksum" <*> o .: "prefix_checksum"
    <*> o .: "resume" <*> o .: "stable" <*> o .: "result" <*> o .: "generation_checksum"
    <*> o .: "coverage_position" <*> o .: "stopped_boolean" <*> o .: "tail_boolean"

data Envelope=Envelope Entry Text
instance ToJSON Envelope where
  toEncoding(Envelope e h)=pairs ("checksum" .= h <> "entry" .= e <> "format_version" .= (3::Int))
  toJSON(Envelope e h)=object["format_version" .= (3::Int),"entry" .= e,"checksum" .= h]
instance FromJSON Envelope where
  parseJSON=withObject "envelope" $ \o->do
    v<-o .: "format_version";unless(v==(3::Int))(fail"unknown format")
    Envelope <$> o .: "entry" <*> o .: "checksum"

data Loaded = Loaded { loadedToken :: Text, loadedEntry :: Maybe Entry
  , loadedGeneration :: Text }

-- The payload has already been encoded for its checksum. Keep those exact
-- bytes in the existing canonical envelope rather than traverse it again.
-- Assembly is demanded only at publishBytes' existing write point.
encodeEnvelopePayload :: B.ByteString -> Text -> B.ByteString
encodeEnvelopePayload payload checksum=B.concat
  ["{\"checksum\":",BL.toStrict(encode checksum),",\"entry\":",payload,",\"format_version\":3}"]

-- Only a positive canonical hit bypasses Aeson's intermediate Value tree.
-- Every miss runs the untouched complete decoder, including its exact errors.
fromCanonicalEntry :: Canonical.CanonicalEntry -> Entry
fromCanonicalEntry e=Entry (Canonical.canonicalVersion e) (Canonical.canonicalContext e)
  (Canonical.canonicalStamp e) (Canonical.canonicalHead e) (Canonical.canonicalBoundary e)
  (Canonical.canonicalPrefix e) (Canonical.canonicalPoint e) (Canonical.canonicalStable e)
  (Canonical.canonicalResult e) (Canonical.canonicalGeneration e) (Canonical.canonicalCoverage e)
  (Canonical.canonicalStopped e) (Canonical.canonicalTail e)
decodeEnvelopeBytes :: B.ByteString -> Either String (Envelope,B.ByteString)
decodeEnvelopeBytes raw=case Canonical.decodeEnvelopePayload raw of
  Just(Canonical.CanonicalEnvelope value checksum,payload)->Right(Envelope(fromCanonicalEntry value)checksum,payload)
  Nothing->do
    envelope@(Envelope value _)<-eitherDecodeStrict' raw
    pure(envelope,BL.toStrict$encode value)
foreign import ccall unsafe "hikaru_log_scan_root" c_root :: IO CInt
foreign import ccall unsafe "hikaru_log_scan_dir_at" c_dirAt :: CInt -> CString -> CInt -> IO CInt
foreign import ccall unsafe "hikaru_log_scan_file_at" c_fileAt :: CInt -> CString -> CInt -> IO CInt
foreign import ccall unsafe "hikaru_log_scan_rename_at" c_renameAt :: CInt -> CString -> CString -> IO CInt
foreign import ccall unsafe "hikaru_log_scan_unlink_at" c_unlinkAt :: CInt -> CString -> IO CInt
foreign import ccall unsafe "hikaru_log_scan_list_open" c_listOpen :: CInt -> IO (Ptr ())
foreign import ccall unsafe "hikaru_log_scan_list_next" c_listNext :: Ptr () -> CString -> IO CInt
foreign import ccall unsafe "hikaru_log_scan_list_close" c_listClose :: Ptr () -> IO CInt
foreign import ccall interruptible "flock" c_flock :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "fsync" c_fsync :: CInt -> IO CInt

openCacheAt :: Fd -> FilePath -> CInt -> IO Fd
openCacheAt (Fd parent) name kind=do
  fd<-Fd <$> withCString name (\p->throwErrnoIfMinus1 "open scan cache" (c_fileAt parent p kind))
  (do s<-getFdStatus fd;uid<-getEffectiveUserID
      unless(isRegularFile s && fileOwner s==uid && linkCount s==1 && fileMode s .&. 0o077==0)
        (ioError$userError"unsafe scan cache file")
      pure fd) `onException` closeFd fd

-- Directory FDs remain pinned through all leaf operations. No lstat/open or
-- check/rename pathname gap can redirect a write through a swapped parent.
openDirectoryAt :: Fd -> FilePath -> Bool -> IO Fd
openDirectoryAt (Fd parent) name create=Fd <$> withCString name (\p->
  throwErrnoIfMinus1 "open scan directory" (c_dirAt parent p(if create then 1 else 0)))

withDirectoryPath :: FilePath -> Bool -> (Fd -> IO a) -> IO a
withDirectoryPath path create action=do
  absolute<-normalise <$> makeAbsolute path
  let parts=filter(/="/")(splitDirectories absolute)
  when(any (`elem` [".",".."]) parts)(ioError$userError"unsafe cache path component")
  bracket(Fd <$> throwErrnoIfMinus1 "open filesystem root" c_root)closeFd $ \root->walk root parts
  where
    walk fd []=action fd
    walk fd [name]=bracket(openDirectoryAt fd name create)closeFd action
    walk fd(name:rest)=bracket(openDirectoryAt fd name False)closeFd(\next->walk next rest)

checkPrivate :: Fd -> IO ()
checkPrivate fd=do
  s<-getFdStatus fd;uid<-getEffectiveUserID
  unless(isDirectory s&&fileOwner s==uid&&fileMode s .&. 0o077==0)(ioError$userError"unsafe scan cache directory")

withDirs :: FilePath -> Bool -> (Fd -> Fd -> IO a) -> IO a
withDirs path create action=
  withDirectoryPath(takeDirectory$takeDirectory path)create $ \root->do
    checkPrivate root
    bracket(openDirectoryAt root(takeFileName$takeDirectory path)create)closeFd $ \directory->do
      checkPrivate directory
      action root directory

readOptionalAt :: Fd -> FilePath -> IO (Maybe B.ByteString)
readOptionalAt fd name=(Just <$> mask (\restore->do
  raw<-openCacheAt fd name 0
  bracket(fdToHandle raw `onException` closeFd raw)hClose (restore . B.hGetContents))) `catch` \err->
    if isDoesNotExistError err then pure Nothing else ioError err

unlinkAt :: Fd -> FilePath -> IO ()
unlinkAt (Fd fd) name=do
  _<-withCString name(\p->throwErrnoIfMinus1 "unlink scan cache"(c_unlinkAt fd p));pure()

atomicAt :: Fd -> FilePath -> B.ByteString -> IO ()
atomicAt directory@(Fd fd) target bytes=mask $ \restore->do
  pid<-getProcessID;unique<-hashUnique <$> newUnique
  let temporary=".commit-"++show pid++"-"++show unique
  raw@(Fd n)<-openCacheAt directory temporary 1
  let remove=unlinkAt directory temporary `catch` \err->
        if isDoesNotExistError err then pure () else ioError err
  (bracket(fdToHandle raw `onException` closeFd raw)hClose $ \output->restore $ do
      B.hPut output bytes;hFlush output
      _<-throwErrnoIfMinus1 "sync scan cache"(c_fsync n)
      pure()) `onException` remove
  (do _<-withCString temporary $ \old->withCString target $ \new->
        throwErrnoIfMinus1 "rename scan cache"(c_renameAt fd old new)
      _<-throwErrnoIfMinus1 "sync scan directory"(c_fsync fd)
      pure()) `onException` remove

generationAt :: Fd -> Fd -> IO Text
generationAt root directory=do
  top<-readOptionalAt root ".epoch"
  local<-readOptionalAt directory ".epoch"
  contextDigest[maybe B.empty id top,maybe B.empty id local]

loadAt :: Fd -> Fd -> FilePath -> IO Loaded
loadAt root directory name=do
  generation<-generationAt root directory
  raw<-readOptionalAt directory name
  token<-contextDigest[generationBytes generation,maybe B.empty id raw]
  entry<-case raw >>= either(const Nothing)Just . decodeEnvelopeBytes of
    Nothing->pure Nothing
    Just(Envelope value expected,payload)->do
      actual<-contextDigest[payload]
      pure(if expected==actual && entryGeneration value==generation then Just value else Nothing)
  pure(Loaded token entry generation)
  where generationBytes=BC.pack.T.unpack

safeName :: Text -> Bool
safeName value=not(T.null value)&&T.length value<=64&&T.all(\c->isAscii c&&(isAlphaNum c||c=='_'))value

conversationDirectory :: ScanEnv -> FilePath -> IO FilePath
conversationDirectory env path=do
  root<-normalise <$> makeAbsolute(scanCacheRoot env)
  absolute<-keyPath . normalise <$> makeAbsolute path
  name<-contextDigest["log-scan-source-v1",BC.pack(show(map ord absolute))]
  pure(root </> T.unpack name)
  where
    -- normalise deliberately retains '..'. The cache namespace uses lexical
    -- absolute names so a maintenance caller can spell ../logs/session.jsonl.
    -- This never changes the path used to open source. A symlink/.. alias may
    -- collide in the namespace, but source identity still forces rebuilding.
    keyPath absolute="/" </> joinPath(reverse$foldl component []$splitDirectories absolute)
    component acc "/"=acc
    component acc "."=acc
    component [] ".."=[]
    component (_:rest) ".."=rest
    component acc name=name:acc

cachePath :: ScanEnv -> OpenedLog -> Consumer input state checkpoint -> IO FilePath
cachePath env source consumer=do
  unless(safeName(consumerName consumer))(ioError$userError"invalid scan channel")
  directory<-conversationDirectory env(sourcePath source)
  pure(directory </> (T.unpack(consumerName consumer)++".json"))

loadEntry :: FilePath -> IO Loaded
loadEntry path = withDirs path False(\root directory->loadAt root directory(takeFileName path))
  `catch` \err->if isDoesNotExistError err then do
    -- Preserve a root invalidation epoch even before this conversation has a
    -- directory. An in-flight first scan must not publish across invalidateAll.
    top<-((withDirectoryPath(takeDirectory$takeDirectory path)False $ \root->checkPrivate root>>readOptionalAt root ".epoch")
      `catch` \e->if isDoesNotExistError e then pure Nothing else ioError e)
    generation<-contextDigest[maybe B.empty id top,B.empty]
    token<-contextDigest[BC.pack$T.unpack generation,B.empty]
    pure(Loaded token Nothing generation)
  else ioError err

samples :: OpenedLog -> Integer -> IO (Text,Text)
samples source size=do
  first<-readRange source 0(min 4096 size) >>= contextDigest.pure
  last_<-readRange source (max 0(size-4096))(min 4096 size) >>= contextDigest.pure
  pure(first,last_)

validEntry :: ScanEnv -> OpenedLog -> Consumer input state checkpoint -> FileStamp -> Entry -> IO Bool
validEntry env source consumer current entry
  | fileSize old<0 || not(validProjection(PDigest(fileIdentity old))) =pure False
  | entryVersion entry/=consumerVersion consumer || entryContext entry/=consumerContext consumer =pure False
  | fileIdentity old/=fileIdentity current || fileSize current<fileSize old =pure False
  | fileSize current==fileSize old && current/=old =pure False
  | entryCoverage entry<0 || entryCoverage entry>fileSize old =pure False
  | resumeByte point<0 || resumeByte point>entryCoverage entry || resumeRow point<0 || resumeRow point>fileSize old+1 =pure False
  | not(validProjection(entryStable entry)&&validProjection(entryResult entry)) =pure False
  | otherwise=do
      (headHash,boundaryHash)<-samples source(entryCoverage entry)
      if headHash/=entryHead entry || boundaryHash/=entryBoundary entry then pure False
      else if scanMode env/=ScanVerify then pure True else case entryPrefix entry of
        Nothing->pure False
        Just expected->(==expected) <$> (readRange source 0(entryCoverage entry)>>=contextDigest.pure)
  where old=entryStamp entry;point=entryPoint entry

publishBytes :: FilePath -> Maybe Text -> B.ByteString -> IO Bool
publishBytes path expected bytes=withDirs path True $ \root directory->do
  let name=takeFileName path
  bracket(openCacheAt directory(name++".lock")2)closeFd $ \(Fd fd)->do
    locked<-c_flock fd 6
    if locked/=0 then do
      err<-getErrno
      if err==eWOULDBLOCK then pure False else throwErrno "lock scan cache"
    else finally (do
      current<-loadAt root directory name
      if Just (loadedToken current)/=expected then pure False else do
        atomicAt directory name bytes
        pure True)
      (throwErrnoIfMinus1_ "unlock scan cache" (c_flock fd 8))

publish :: FilePath -> Maybe Text -> Entry -> IO Bool
publish path expected value=do
  let payload=BL.toStrict$encode value
  checksum<-contextDigest[payload]
  publishBytes path expected(encodeEnvelopePayload payload checksum)

scanConsumer :: ScanEnv -> OpenedLog -> Consumer input state checkpoint -> IO (ScanResult state)
scanConsumer env source consumer=do
  (beforeBytes,beforeMaterialized)<-readCounts source
  current<-sourceStamp source
  regular<-sourceRegular source
  outcome<-if scanMode env==ScanOff || not regular then full False Nothing Nothing current else do
    path<-cachePath env source consumer
    entryState<-loadEntry path
    let location=Just path;loaded=Just entryState
    case loadedEntry entryState of
      Just entry->do
        valid<-validEntry env source consumer current entry
        if not valid then full True location loaded current else do
          let unchanged=current==entryStamp entry
              projection=if unchanged || entryStopped entry && not(entryTail entry) then entryResult entry else entryStable entry
          restored<-case decodeProjection (consumerCodec consumer) projection of
            Nothing -> pure Nothing
            Just checkpoint -> restore (consumerRestore consumer checkpoint)
          case restored of
            Nothing->full True location loaded current
            Just state | unchanged->pure(state,ScanReused)
            Just state-> incremental path loaded current entry state
      _->full True location loaded current
  (afterBytes,afterMaterialized)<-readCounts source
  pure(ScanResult(fst outcome)(snd outcome)(afterBytes-beforeBytes)(afterMaterialized-beforeMaterialized))
  where
    restore (Restored state) = pure state
    restore (Reread span_ next) = readLocated source span_ >>= restore . next
    step state line = do
      input <- consumerInput consumer line
      either (ioError . userError . T.unpack) pure (consumerStep consumer state input)
    project = fmap (encodeProjection (consumerCodec consumer)) . consumerProject consumer
    policy = consumerReadPolicy consumer (scanMode env)
    resume point = foldLocated source policy point step
    full cache location loaded stamp=do
      result<-resume (ResumePoint 0 0 False) (consumerInitial consumer)
      saved<-if cache then commit location loaded stamp result else pure False
      pure(readState result,if not cache then ScanDisabled else if saved then ScanRebuilt else ScanUncached)
    incremental path loaded stamp entry state=do
          result<-if entryStopped entry && not(entryTail entry) then do
            through<-validateStopped source policy (entryCoverage entry)
            pure(ReadResult state state(entryPoint entry)True through False)
          else resume (entryPoint entry) state
          saved<-commit(Just path)loaded stamp result
          pure(readState result,if saved then ScanExtended else ScanUncached)
    commit location loaded stamp result=case(location,project(readPrefixState result),project(readState result))of
      (Just path,Just stable,Just final_)
        | validProjection stable && validProjection final_ ->do
          after<-sourceStamp source
          if after/=stamp then pure False else do
            let coverage=readDecodedThrough result
            (first,last_)<-samples source coverage
            prefix<-if scanMode env==ScanVerify then Just <$> (readRange source 0 coverage>>=contextDigest.pure) else pure Nothing
            finalStamp<-sourceStamp source
            if finalStamp/=stamp then pure False else do
              let generation=maybe "" loadedGeneration loaded
                  e=Entry(consumerVersion consumer)(consumerContext consumer)stamp first last_ prefix(readResume result)stable final_
                       generation coverage(readStopped result)(readTail result)
              publish path (loadedToken <$> loaded) e `catch` \err->
                if isDoesNotExistError err then pure False else ioError err
      _->pure False

invalidateConversation :: ScanEnv -> FilePath -> IO ()
invalidateConversation env path=do
  directory<-conversationDirectory env path
  withDirs(directory </> "unused.json")True $ \_ current->do
    writeEpoch current
    removeEntries current

writeEpoch :: Fd -> IO ()
writeEpoch directory=bracket(openCacheAt directory ".epoch.lock" 2)closeFd $ \(Fd lock)->do
  _<-throwErrnoIfMinus1 "lock scan epoch"(c_flock lock 2)
  (do entropy<-withBinaryFile "/dev/urandom" ReadMode (`B.hGet` 32)
      unless(B.length entropy==32)(ioError$userError"incomplete scan epoch entropy")
      nonce<-contextDigest[entropy]
      atomicAt directory ".epoch" (BC.pack$T.unpack nonce)) `finally`
        throwErrnoIfMinus1_ "unlock scan epoch" (c_flock lock 8)

removeEntries :: Fd -> IO ()
removeEntries directory=do
  names<-listAt directory
  mapM_ (\name->when(T.isSuffixOf ".json"(T.pack name)&&safeName(T.dropEnd 5$T.pack name)) $ do
    -- An already pinned regular leaf is removed by name; never follow a link.
    -- A simultaneous replacement can at most remove another cache leaf.
    bracket(openCacheAt directory name 0)closeFd(const$unlinkAt directory name))names

invalidateAll :: ScanEnv -> IO ()
invalidateAll env=withDirectoryPath(scanCacheRoot env)True $ \root->do
  checkPrivate root
  writeEpoch root
  names<-listAt root
  mapM_ (\name->when(validProjection(PDigest$T.pack name)) $
    bracket(openDirectoryAt root name False)closeFd $ \directory->checkPrivate directory>>removeEntries directory)names

-- macOS /dev/fd/N cannot be traversed as a directory. fdopendir on an owned
-- duplicate preserves the pinned identity without reopening a pathname.
listAt :: Fd -> IO [FilePath]
listAt (Fd fd)=bracket acquire release $ \stream->allocaBytes 1024 (loop stream)
  where
    acquire=do
      stream<-c_listOpen fd
      if stream==nullPtr then do
        _<-throwErrnoIfMinus1 "open scan directory stream" (pure(-1 :: CInt))
        ioError$userError"unreachable directory stream"
      else pure stream
    release stream=do
      _<-throwErrnoIfMinus1 "close scan directory stream"(c_listClose stream);pure()
    loop stream buffer=do
      found<-throwErrnoIfMinus1 "read scan directory stream"(c_listNext stream buffer)
      if found==0 then pure[] else do
        -- Policy accepts only ASCII digest/channel names; bytes outside ASCII
        -- remain unmatched, without locale-dependent filename decoding.
        name<-BC.unpack <$> B.packCString buffer
        rest<-loop stream buffer
        pure(name:rest)
