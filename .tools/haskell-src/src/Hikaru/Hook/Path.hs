{-# LANGUAGE OverloadedStrings #-}
-- | The hook protocol accepts a named file or an inherited descriptor.
-- Descriptor ownership is explicit; runtime-owned descriptors are not inputs.
module Hikaru.Hook.Path
  ( HookPath(..), optionalPath, pathPresent, pathName, consumePath ) where

import Control.Exception (finally)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import Foreign.C.Error (Errno(..), eBADF)
import GHC.IO.Exception (IOException(..))
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.Files (getFileStatus, getFdStatus)
import System.Posix.IO (closeFd, queryFdOption, FdOption(CloseOnExec))
import System.Posix.Types (Fd(..))

data HookPath = FilePathRef FilePath | DescriptorRef Fd deriving (Eq, Show)

optionalPath :: Value -> Parser (Maybe HookPath)
optionalPath Null = pure Nothing
optionalPath (String text) = pure (if T.null text then Nothing else Just (FilePathRef (T.unpack text)))
optionalPath (Bool False) = pure Nothing
optionalPath (Bool True) = pure (Just (DescriptorRef (Fd 1)))
optionalPath (Number 0) = pure Nothing
optionalPath (Number value) = case toBoundedInteger value :: Maybe CInt of
  Just descriptor -> pure (Just (DescriptorRef (Fd descriptor)))
  Nothing -> fail "transcript descriptor is not an in-range integer"
optionalPath _ = fail "transcript path must name a file or descriptor"

pathPresent :: Bool -> HookPath -> IO Bool
pathPresent _ (FilePathRef path) = (True <$ getFileStatus path) `catchIOError` \err ->
  if isDoesNotExistError err then pure False else ioError err
pathPresent inheritedOnly (DescriptorRef fd) = inspect `catchIOError` \err ->
  if ioe_errno err == Just badDescriptor then pure False else ioError err
  where
    Errno badDescriptor = eBADF
    inspect = do
      _ <- getFdStatus fd
      if inheritedOnly then not <$> queryFdOption fd CloseOnExec else pure True

pathName :: HookPath -> FilePath
pathName (FilePathRef path) = path
pathName (DescriptorRef (Fd number)) = "/dev/fd/" ++ show number

consumePath :: HookPath -> IO a -> IO a
consumePath (FilePathRef _) action = action
consumePath (DescriptorRef fd) action = action `finally` closeFd fd
