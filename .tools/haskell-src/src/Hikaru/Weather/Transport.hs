{-# LANGUAGE OverloadedStrings #-}
module Hikaru.Weather.Transport
  ( Transport, TransportFailure(..), httpTransport, httpRequest, replayTransport ) where

import qualified Hikaru.Local.Settings as Local
import Control.Concurrent (threadDelay)
import Control.Exception (Exception(..), catch, throwIO)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Hikaru.Weather.Types (WeatherRequest, requestURL)
import Hikaru.Weather.Json (decodeWeatherJSON)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

data TransportFailure = TimedOut | Unavailable | HttpFailure Int | InvalidReplay String
  deriving (Eq, Show)
instance Exception TransportFailure where
  displayException TimedOut = "TimeoutError: weather request timed out"
  displayException Unavailable = "ConnectionError: weather service unavailable"
  displayException (HttpFailure status) = "HTTPError: weather API returned status " ++ show status
  displayException (InvalidReplay reason) = "ValueError: invalid weather replay: " ++ reason

type Transport = WeatherRequest -> Double -> IO (Either TransportFailure B.ByteString)

-- This conversion is also inspected by the privacy test. The URL is built
-- solely from WeatherRequest, never from a record, session, or Config path.
httpRequest :: WeatherRequest -> Double -> Request
httpRequest request seconds = (setRequestCheckStatus (parseRequest_ (requestURL request)))
  { responseTimeout = responseTimeoutMicro (round (seconds * 1000000))
  , cookieJar = Nothing
  }

httpTransport :: Transport
httpTransport _ _ | not Local.weatherEnabled = pure (Left Unavailable)
httpTransport request seconds = do
  manager <- newManager tlsManagerSettings
  (Right . responseBody <$> httpLbs (httpRequest request seconds) manager) `catch` \err ->
    case err of
      HttpExceptionRequest _ ResponseTimeout -> pure (Left TimedOut)
      HttpExceptionRequest _ ConnectionTimeout -> pure (Left TimedOut)
      HttpExceptionRequest _ (ConnectionFailure _) -> pure (Left Unavailable)
      HttpExceptionRequest _ NoResponseDataReceived -> pure (Left Unavailable)
      HttpExceptionRequest _ ConnectionClosed -> pure (Left Unavailable)
      _ -> throwIO err

-- The existing explicit offline replay setting stays available for command
-- tests. Without that setting, only httpTransport is selected.
data Replay = Replay Double (Either TransportFailure B.ByteString)
instance FromJSON Replay where
  parseJSON = withObject "weather replay" $ \o -> do
    delay <- o .:? "delay_seconds" .!= 0
    mode <- o .: "mode" :: Parser Text
    reply <- case mode of
      "failure" -> pure (Left Unavailable)
      "timeout" -> pure (Left TimedOut)
      "http_error" -> Left . HttpFailure <$> o .: "status"
      "raw" -> Right . B.fromStrict <$> (o .: "body" >>= pure . encodeUtf8)
      _ -> Right . encode <$> (o .: "response" :: Parser Value)
    pure (Replay delay reply)

replayTransport :: FilePath -> Transport
replayTransport path _ _ = do
  bytes <- B.readFile path
  case decodeWeatherJSON bytes of
    Left reason -> pure (Left (InvalidReplay (path ++ ": " ++ reason)))
    Right (Replay delay reply) -> do
      threadDelay (round (delay * 1000000))
      pure reply
