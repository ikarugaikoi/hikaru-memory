{-# LANGUAGE OverloadedStrings #-}
-- | Cache eligibility for consumers which call the host local-time resolver.
-- An unrecognised TZ selection disables this optimisation; it does not change
-- the business parser or raise an extra user-visible error.
module Hikaru.Choubo.Context (consumerContext, pureContext) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)
import System.FilePath ((</>), splitDirectories, isAbsolute)
import System.IO.Error (catchIOError, isDoesNotExistError)
import Hikaru.Compat.Json (PyString, pyStringToChars)
import Hikaru.Choubo.Store (contextDigest)

-- Haskell's escaped String representation is injective over the PyString
-- code points, including lone surrogates (which Show escapes).
identityBytes :: PyString -> BS.ByteString
identityBytes = TE.encodeUtf8 . T.pack . show . pyStringToChars

pureContext :: Text -> PyString -> IO Text
pureContext version identity = contextDigest [TE.encodeUtf8 version, identityBytes identity]

consumerContext :: Text -> PyString -> IO (Maybe Text)
consumerContext version identity = build
  where
    build = do
      tz <- lookupEnv "TZ"
      tzdir <- lookupEnv "TZDIR"
      case selectedZone tz tzdir of
        Nothing -> pure Nothing
        Just path -> (do
          bytes <- BS.readFile path
          if not ("TZif" `BS.isPrefixOf` bytes)
            then pure Nothing
            else Just <$> contextDigest
              [ TE.encodeUtf8 version, identityBytes identity
              , B8.pack (show tz), B8.pack (show tzdir), bytes ]) `catchIOError` \err ->
                if isDoesNotExistError err then pure Nothing else ioError err

    -- Match the supported Darwin IANA-file selection domain. POSIX TZ rule
    -- strings, explicit custom TZDIR and ambiguous selections deliberately
    -- fall back. The resolver itself still receives the original environment.
    selectedZone Nothing Nothing = Just "/etc/localtime"
    selectedZone (Just value) Nothing
      | not (null value)
      , not (isAbsolute value)
      , all (`notElem` [".", ".."]) (splitDirectories value)
      , all (\c -> c `elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_+-/" :: String)) value
      = Just ("/usr/share/zoneinfo" </> value)
    selectedZone _ _ = Nothing
