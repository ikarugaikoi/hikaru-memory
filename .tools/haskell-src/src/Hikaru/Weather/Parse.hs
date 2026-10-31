{-# LANGUAGE OverloadedStrings #-}
-- | JSON and stored-line grammars end here; policy never indexes JSON values.
module Hikaru.Weather.Parse (decodeForecast, decodeCache, parseStoredWeather) where

import Control.Monad (forM, guard)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither, JSONPathElement(Key, Index))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as B
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day, fromGregorianValid)
import Data.Void (Void)
import qualified Data.Vector as V
import Hikaru.HouseDay (HouseDay, houseDayNamed)
import Hikaru.Decimal (decimalChar, asciiDecimal)
import Hikaru.Weather.Types
import Hikaru.Weather.Json (decodeWeatherJSON)
import Text.Megaparsec hiding (parse, many, some, (<?>))
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (char)
import Control.Applicative (some)
import Text.Read (readMaybe)

type LineParser = Parsec Void Text

dateText :: Text -> Either String Day
dateText text = do
  (y,m,d) <- either (Left . errorBundlePretty) Right (P.parse (dateParts <* eof) "weather date" text)
  maybe (Left "invalid weather date") Right (fromGregorianValid y m d)
  where
    digits n = T.pack <$> count n (satisfy (\c -> c >= '0' && c <= '9'))
    dateParts :: LineParser (Integer, Int, Int)
    dateParts = do
      y <- digits 4; _ <- char '-'; m <- digits 2; _ <- char '-'; d <- digits 2
      pure (read (T.unpack y), read (T.unpack m), read (T.unpack d))

number :: Value -> Parser Double
number (Number n) = pure (toRealFloat n)
number (Bool b) = pure (if b then 1 else 0)
number (String t) = maybe (fail "invalid weather number") pure (readMaybe (T.unpack (T.strip t)))
number _ = fail "invalid weather number"

weatherCode :: Value -> Parser Text
weatherCode value = do
  code <- case value of
    String t -> maybe (fail "invalid weather code") pure (readMaybe (T.unpack (T.strip t)) :: Maybe Integer)
    _ -> do n <- number value
            if isNaN n || isInfinite n then fail "invalid weather code" else pure (truncate n)
  pure (M.findWithDefault "？" code skyWords)

skyWords :: Map Integer Text
skyWords = M.fromList
  [(0,"快晴"),(1,"晴れ"),(2,"晴れ時々くもり"),(3,"くもり"),(45,"霧"),(48,"霧")
  ,(51,"霧雨"),(53,"霧雨"),(55,"霧雨"),(56,"霧雨"),(57,"霧雨")
  ,(61,"小雨"),(63,"雨"),(65,"大雨"),(66,"雨"),(67,"雨")
  ,(71,"小雪"),(73,"雪"),(75,"大雪"),(77,"雪")
  ,(80,"にわか雨"),(81,"にわか雨"),(82,"激しいにわか雨"),(85,"にわか雪"),(86,"にわか雪")
  ,(95,"雷雨"),(96,"雷雨"),(99,"雷雨")]

newtype Response = Response Forecast
instance FromJSON Response where
  parseJSON = withObject "forecast" $ \o -> do
    daily <- (o .:? "daily" >>= maybe (pure M.empty) parseDaily) <?> Key "daily"
    current <- (o .:? "current" >>= maybe (pure Nothing) parseCurrent) <?> Key "current"
    pure (Response (Forecast current daily))

parseDaily :: Value -> Parser (Map Day Weather)
parseDaily (Bool False) = pure M.empty
parseDaily (Number 0) = pure M.empty
parseDaily (String "") = pure M.empty
parseDaily (Array values) | V.null values = pure M.empty
parseDaily input = (withObject "daily weather" $ \o -> do
  dates <- (o .:? "time" >>= maybe (pure []) times) <?> Key "time"
  rows <- forM (zip [0..] dates) $ \(i,date) -> do
    let at key = do
          values <- o .: key
          (maybe (fail "short daily weather array") pure ((values :: V.Vector Value) V.!? i)
            <?> Index i) <?> Key key
        field key parser value = (parser value <?> Index i) <?> Key key
    code <- at "weather_code"
    high <- at "temperature_2m_max"
    low <- at "temperature_2m_min"
    rain <- at "precipitation_sum"
    if Null `elem` [code,high,low,rain] then pure Nothing else do
      day <- field "time" (\value -> parseJSON value >>= either fail pure . dateText) date
      weather <- Weather <$> field "weather_code" weatherCode code
        <*> field "temperature_2m_max" number high <*> field "temperature_2m_min" number low
        <*> field "precipitation_sum" number rain
      pure (Just (day,weather))
  pure (M.fromList (catMaybes rows))) input
  where
    times (Bool False) = pure []
    times (Number 0) = pure []
    times (String "") = pure []
    times value = parseJSON value :: Parser [Value]

parseCurrent :: Value -> Parser (Maybe CurrentWeather)
parseCurrent (Bool False) = pure Nothing
parseCurrent (Number 0) = pure Nothing
parseCurrent (String "") = pure Nothing
parseCurrent (Array values) | V.null values = pure Nothing
parseCurrent input = (withObject "current weather" $ \o -> do
  code <- o .:? "weather_code"
  temperature <- o .:? "temperature_2m"
  case (code,temperature) of
    (Just c,Just t) -> Just <$> (CurrentWeather <$> (weatherCode c <?> Key "weather_code")
      <*> (number t <?> Key "temperature_2m"))
    _ -> pure Nothing) input

decodeForecast :: B.ByteString -> Either String Forecast
decodeForecast bytes = do Response result <- decodeWeatherJSON bytes; pure result

-- Missing, stale or malformed cache content is rebuilt. I/O failures belong
-- to the caller and never enter this pure content decoder.
data Envelope = Envelope HouseDay Double (Either String Sky)
instance FromJSON Envelope where
  parseJSON = withObject "weather cache" $ \o -> do
    day <- o .: "day" >>= either fail (pure . houseDayNamed) . dateText
    timestamp <- case KM.lookup "t" o of Nothing -> pure 0; Just value -> cachedNumber value
    pure (Envelope day timestamp (parseEither (cachedValues OriginalNumbers) o))

decodeCache :: Double -> HouseDay -> B.ByteString -> Maybe (Either String Sky)
decodeCache epoch day bytes = do
  Envelope storedDay timestamp original <- decode bytes
  guard (cacheIsFresh epoch day timestamp storedDay)
  sky <- either (const Nothing) Just $ do
    -- Cached measurements were already numbers, unlike API numbers which
    -- also accept numeric strings. Validate that distinction before restoring
    -- decimal-zero signs, so a malformed fresh cache becomes a miss.
    _ <- original
    CachedValues value <- decodeWeatherJSON bytes
    pure value
  pure (Right sky)

newtype CachedValues = CachedValues Sky
instance FromJSON CachedValues where
  parseJSON = withObject "cached weather" (fmap CachedValues . cachedValues RestoredZeros)

-- The original cache accepts only JSON numbers/bools; the lexeme-preserving
-- decoder additionally admits its "-0" marker. Keep those policies paired.
data CacheNumbers = OriginalNumbers | RestoredZeros

cachedNumber :: Value -> Parser Double
cachedNumber value@(Number _) = number value
cachedNumber value@(Bool _) = number value
cachedNumber _ = fail "cached measurement is not a number"

-- Old caches could contain a nontext label. Keep their successful rendering
-- and exit status, but use JSON's spelling instead of rebuilding Python repr.
cachedWord :: Value -> Parser Text
cachedWord (String word) = pure word
cachedWord value = pure (TE.decodeUtf8 (B.toStrict (encode value)))

cachedValues :: CacheNumbers -> Object -> Parser Sky
cachedValues policy fields = do
  current <- fields .:? "cur" .!= Null
  daily <- fields .:? "daily" .!= Null
  Sky <$> optionalWeather "cached current weather" 2 currentValue current
      <*> optionalWeather "cached daily weather" 4 dailyValue daily
  where
    measurement = case policy of OriginalNumbers -> cachedNumber; RestoredZeros -> number
    currentValue a = CurrentWeather <$> cachedWord (a V.! 0) <*> measurement (a V.! 1)
    dailyValue a = Weather <$> cachedWord (a V.! 0) <*> measurement (a V.! 1) <*> measurement (a V.! 2) <*> measurement (a V.! 3)
    optionalWeather _ _ _ Null = pure Nothing
    optionalWeather _ _ _ (Bool False) = pure Nothing
    optionalWeather _ _ _ (Number 0) = pure Nothing
    optionalWeather _ _ _ (String "") = pure Nothing
    optionalWeather _ _ _ (String "-0") | RestoredZeros <- policy = pure Nothing
    optionalWeather _ _ _ (Object values) | KM.null values = pure Nothing
    optionalWeather _ _ _ (Array values) | V.null values = pure Nothing
    optionalWeather description size parseValue value = Just <$> withArray description (\a ->
      if V.length a < size then fail ("short " ++ description) else parseValue a) value

-- Prefix matching is deliberate. A matched but invalid date/number fails the
-- whole read; a line outside the grammar is ignored. Last duplicate wins.
parseStoredWeather :: Text -> Either String (Map Day Weather)
parseStoredWeather text = M.fromList . catMaybes <$> traverse line (zip [1 :: Int ..] (T.splitOn "\n" text))
  where
    line (ordinal,input) = case P.parse storedLine "stored weather" input of
      Left _ -> Right Nothing
      Right (date,word,high,low,rain) -> do
        let field name result = either (Left . (("line " ++ show ordinal ++ ", " ++ name ++ ": ") ++)) Right result
        day <- field "date" (dateText date)
        weather <- Weather word <$> field "high" (decimal high) <*> field "low" (decimal low)
          <*> field "rain" (decimal rain)
        pure (Just (day,weather))
    decimal value
      | not (T.any decimalChar value) = Left "invalid stored weather number"
      | otherwise = maybe (Left "invalid stored weather number") Right
          (readMaybe (T.unpack (normalizeDecimal value)))

-- Read also accepts .5 and 5. through normalization; the grammar itself must
-- still admit malformed dot sequences so that those stop instead of vanish.
normalizeDecimal :: Text -> Text
normalizeDecimal raw = prefix <> leading <> trailing
  where
    normalized = T.map asciiDecimal raw
    (prefix,body) = if T.isPrefixOf "-" normalized then ("-",T.drop 1 normalized) else ("",normalized)
    leading = if T.isPrefixOf "." body then "0" else ""
    trailing = body <> if T.isSuffixOf "." body then "0" else ""

storedLine :: LineParser (Text,Text,Text,Text,Text)
storedLine = do
  date <- T.pack <$> ((++) <$> count 4 digit <*> ((:) <$> char '-' <*> ((++) <$> count 2 digit <*> ((:) <$> char '-' <*> count 2 digit))))
  _ <- char '｜'
  word <- T.pack <$> someTill anySingle (try (lookAhead (char '｜' *> measurements)))
  _ <- char '｜'
  (high,low,rain) <- measurements
  pure (date,word,high,low,rain)
  where
    digit = satisfy decimalChar
    magnitude = T.pack <$> some (satisfy (\c -> c == '.' || decimalChar c))
    signed = do sign <- optional (char '-'); value <- magnitude
                pure (maybe "" T.singleton sign <> value)
    measurements = do high <- signed; _ <- char '/'; low <- signed; _ <- char '｜'; rain <- magnitude
                      pure (high,low,rain)
