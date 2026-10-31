{-# LANGUAGE OverloadedStrings #-}
-- | Aeson uses Scientific, which cannot retain the sign of decimal zero.
-- Preserve that one measurement lexeme before the typed Aeson decoder; strings
-- and all other tokens pass through unchanged. JSON validation stays in Aeson.
module Hikaru.Weather.Json (decodeWeatherJSON) where

import Control.Applicative (many, some, (<|>))
import Data.Aeson (FromJSON, eitherDecode)
import Data.Char (isSpace)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, match, satisfy, anySingle, takeWhile1P, eof, optional)
import Text.Megaparsec.Char (char, string)

type Lexer = Parsec Void Text

decodeWeatherJSON :: FromJSON a => B.ByteString -> Either String a
decodeWeatherJSON bytes = do
  input <- either (Left . show) Right (TE.decodeUtf8' (B.toStrict bytes))
  tokens <- either (Left . show) Right (parse (many token <* eof) "weather JSON" input)
  eitherDecode (B.fromStrict (TE.encodeUtf8 (T.concat (preserve tokens))))
  where
    preserve [] = []
    preserve (raw:rest)
      | Right () <- parse negativeZero "zero" raw
      , take 1 (dropWhile (T.all isSpace) rest) /= [":"] = "\"-0\"" : preserve rest
      | otherwise = raw : preserve rest

token :: Lexer Text
token = fst <$> match (quoted <|> numeric <|> (() <$ anySingle))
  where
    quoted = char '"' *> many (escaped <|> satisfy (\c -> c /= '"' && c /= '\\')) *> char '"' *> pure ()
    escaped = char '\\' *> anySingle
    numeric = () <$ takeWhile1P Nothing (\c -> c `elem` ("-+.eE0123456789" :: String))

negativeZero :: Lexer ()
negativeZero = do
  _ <- string "-0"
  fraction <- optional (char '.' *> some (char '0'))
  exponent_ <- optional (satisfy (`elem` ("eE" :: String)) *> optional (satisfy (`elem` ("+-" :: String))) *> some (satisfy (\c -> c >= '0' && c <= '9')))
  case (fraction,exponent_) of
    (Nothing,Nothing) -> fail "integer zero has no sign"
    _ -> eof
