{-# LANGUAGE DeriveDataTypeable #-}
-- | Named Python failure boundaries shared by business modules. Async
-- cancellation is never swallowed by a Python-style `except Exception`.
module Hikaru.Compat.Exception
  ( PythonFailure(..), raisePython, fromPython, trySynchronous
  , catchOrdinary, requireEither, failure ) where

import Control.Exception
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

newtype PythonFailure = PythonFailure Text deriving (Show, Typeable)
instance Exception PythonFailure

raisePython :: Text -> IO a
raisePython = throwIO . PythonFailure

fromPython :: Either Text a -> IO a
fromPython = either raisePython pure

trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous = tryJust (\e -> case fromException e :: Maybe SomeAsyncException of
  Just _ -> Nothing
  Nothing -> Just e)

catchOrdinary :: IO a -> (SomeException -> IO a) -> IO a
catchOrdinary action handler = action `catch` \err -> case fromException err :: Maybe SomeAsyncException of
  Just _ -> throwIO err
  Nothing -> handler err

-- Preserve the already-tested hook API while named failures are introduced
-- in the other modules. Hook exception boundaries suppress ordinary faults.
failure :: Text -> IO a
failure = ioError . userError . T.unpack

requireEither :: Either Text a -> IO a
requireEither = either failure pure
