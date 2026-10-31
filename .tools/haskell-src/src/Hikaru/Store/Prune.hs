{-# LANGUAGE ForeignFunctionInterface #-}
-- | POSIX fd-relative removal. Never chmods a path or follows a replaced
-- directory symlink. Only a path already removed by another cleaner is absent.
module Hikaru.Store.Prune (removeTreeIgnoringErrors) where

import Control.Exception (bracket,bracketOnError)
import Control.Monad (forM_,void,when)
import Data.Bits ((.&.))
import Data.Word (Word64)
import Foreign.C.Error
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.Storable (peek)
import GHC.IO.Encoding (getFileSystemEncoding)
import qualified GHC.Foreign as G
import System.IO.Error (catchIOError,isDoesNotExistError)
import System.Posix.Files (FileStatus,deviceID,fileID,getFdStatus,getSymbolicLinkStatus,isSymbolicLink)
import System.Posix.IO (OpenMode(ReadOnly),OpenFileFlags(..),defaultFileFlags,openFd,openFdAt,closeFd,dup)
import System.Posix.Types (Fd(..))
import System.Directory (removeDirectory)

foreign import ccall unsafe "fdopendir" c_fdopendir :: CInt -> IO (Ptr ())
foreign import ccall unsafe "closedir" c_closedir :: Ptr () -> IO CInt
foreign import ccall unsafe "hikaru_readdir_name" c_readdir :: Ptr () -> IO CString
foreign import ccall unsafe "hikaru_stat_at" c_statAt :: CInt -> CString -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO CInt
foreign import ccall unsafe "hikaru_unlink_at" c_unlinkAt :: CInt -> CString -> CInt -> IO CInt

ignoreMissing :: IO () -> IO ()
ignoreMissing action=catchIOError action $ \err ->
  if isDoesNotExistError err then pure () else ioError err

withPath :: FilePath -> (CString -> IO a) -> IO a
withPath path action=getFileSystemEncoding >>= \encoding->G.withCString encoding path action

data Identity = Identity !Word64 !Word64 !Word64
identity :: FileStatus -> Identity
identity s=Identity(fromIntegral(deviceID s))(fromIntegral(fileID s))0
same :: Identity -> FileStatus -> Bool
same (Identity device inode _) s=device==fromIntegral(deviceID s)&&inode==fromIntegral(fileID s)

statAt :: Fd -> FilePath -> IO Identity
statAt (Fd raw) name=withPath name $ \cstring->alloca $ \device->alloca $ \inode->alloca $ \mode->do
  throwErrnoIfMinus1_ "fstatat" (c_statAt raw cstring device inode mode)
  Identity <$> peek device <*> peek inode <*> peek mode

unlinkAt :: Fd -> FilePath -> Bool -> IO ()
unlinkAt (Fd raw) name directory=withPath name $ \cstring->
  throwErrnoIfMinus1_ "unlinkat" (c_unlinkAt raw cstring(if directory then 1 else 0))

-- fdopendir takes ownership of a duplicate, leaving the traversal FD alive.
-- Materialize names before deleting, matching list(os.scandir(fd)).
names :: Fd -> IO [FilePath]
names fd=bracket acquire (void . c_closedir) $ \stream->do
  encoding<-getFileSystemEncoding
  let loop acc=do
        resetErrno
        ptr<-c_readdir stream
        if ptr==nullPtr then do
          errno<-getErrno
          if errno==eOK then pure(reverse acc) else throwErrno "readdir"
        else do
          name<-G.peekCString encoding ptr
          loop(if name=="."||name==".." then acc else name:acc)
  loop []
  where acquire=bracketOnError (dup fd) closeFd $ \(Fd raw)->throwErrnoIfNull "fdopendir"(c_fdopendir raw)

flags :: OpenFileFlags
flags=defaultFileFlags {cloexec=True,nofollow=True}

walk :: Fd -> IO ()
walk parent=do
  entries<-names parent
  forM_ entries $ \name->ignoreMissing $ do
    before@(Identity _ _ mode)<-statAt parent name
    if mode .&. 0o170000 /= 0o040000 then unlinkAt parent name False else do
      matched<-bracket (openFdAt(Just parent) name ReadOnly flags) closeFd $ \fd->do
        current<-getFdStatus fd
        if same before current then walk fd>>pure True else pure False
      when matched(unlinkAt parent name True)

removeTreeIgnoringErrors :: FilePath -> IO ()
-- The exported name is retained for existing callers. Other IO failures now
-- propagate before the caller replaces its record; symlinks remain untouched.
removeTreeIgnoringErrors path=ignoreMissing $ do
  status<-getSymbolicLinkStatus path
  when (not (isSymbolicLink status)) $ do
    let before=identity status
    matched<-bracket (openFd path ReadOnly flags) closeFd $ \fd->do
      current<-getFdStatus fd
      if same before current then walk fd>>pure True else pure False
    when matched(removeDirectory path)
