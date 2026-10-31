{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Python JSON values. Aeson Value cannot represent NaN, float/int identity,
-- ordered last-wins dictionaries, or unpaired Unicode surrogate code points.
-- Keep those distinctions in the type, never in magic strings or object keys.
module Hikaru.Compat.Json
  ( Value(..), PyNumber(..), PyString, PyObject
  , decodeJson, emptyObject, objectFromPairs, field, objectPairs, pyTruthy, pyEqual, pyCompare, textOf
  , pyStringFromText, pyStringFromChars, pyStringToText, pyStringToChars
  , pyStringNull, pyStringInfix, pyStringStrip
  , pyStringConcat
  ) where

import qualified Data.Aeson.Decoding.Text as A
import qualified Data.Aeson.Decoding.Tokens as A
import qualified Data.Aeson.Key as Key
import Data.Char (chr, ord)
import Data.Foldable (toList)
import Data.List (isInfixOf, dropWhileEnd)
import qualified Data.Map.Strict as M
import qualified Data.Scientific as Sci
import qualified Data.Sequence as Seq
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Hikaru.Compat.Text (pySpace)

-- Constructors are private so every scalar-only string has the fast Text
-- representation. CodePoints is only for strings containing lone surrogates.
data PyString = ScalarText !Text | CodePoints [Char]
  deriving Show

instance Eq PyString where
  ScalarText x == ScalarText y = x == y
  x == y = pyStringToChars x == pyStringToChars y

instance Ord PyString where
  compare (ScalarText x) (ScalarText y) = compare x y
  compare x y = compare (pyStringToChars x) (pyStringToChars y)

instance IsString PyString where fromString = pyStringFromChars

surrogate :: Char -> Bool
surrogate c = ord c >= 0xd800 && ord c <= 0xdfff

pyStringFromText :: Text -> PyString
pyStringFromText = ScalarText

pyStringFromChars :: [Char] -> PyString
pyStringFromChars chars
  | any surrogate chars = CodePoints chars
  | otherwise = ScalarText (T.pack chars)

pyStringToChars :: PyString -> [Char]
pyStringToChars (ScalarText text) = T.unpack text
pyStringToChars (CodePoints chars) = chars

-- Explicit, checked boundary: Data.Text would silently replace surrogates.
pyStringToText :: PyString -> Either Text Text
pyStringToText (ScalarText text) = Right text
pyStringToText (CodePoints _) = Left "unpaired surrogate cannot be represented as scalar Unicode text"

pyStringNull :: PyString -> Bool
pyStringNull (ScalarText text) = T.null text
pyStringNull (CodePoints chars) = null chars

pyStringInfix :: PyString -> PyString -> Bool
pyStringInfix (ScalarText x) (ScalarText y) = x `T.isInfixOf` y
pyStringInfix x y = pyStringToChars x `isInfixOf` pyStringToChars y

pyStringStrip :: PyString -> PyString
pyStringStrip (ScalarText text) = ScalarText (T.dropAround pySpace text)
pyStringStrip (CodePoints chars) = pyStringFromChars (dropWhileEnd pySpace (dropWhile pySpace chars))

pyStringConcat :: [PyString] -> PyString
pyStringConcat strings = case traverse scalar strings of
  Just texts -> ScalarText (T.concat texts)
  Nothing -> pyStringFromChars (concatMap pyStringToChars strings)
  where
    scalar (ScalarText text) = Just text
    scalar _ = Nothing

data PyNumber = PyInteger !Integer | PyFloat !Double | PyNaN
  deriving Show

-- Preserve first insertion position and last assigned value, as dict does.
-- JSON keys are always strings; this cannot collide with user data.
data PyObject = PyObject !(M.Map PyString Value) !(Seq.Seq PyString)
  deriving Show

data Value
  = Null | Bool !Bool | Number !PyNumber | String !PyString
  | Array !(V.Vector Value) | Object !PyObject
  deriving Show

instance Eq Value where (==) = pyEqual

emptyFields :: PyObject
emptyFields = PyObject M.empty Seq.empty

insertField :: PyString -> Value -> PyObject -> PyObject
insertField key value (PyObject fields order) =
  PyObject (M.insert key value fields)
    (if M.member key fields then order else order Seq.|> key)

objectPairs :: PyObject -> [(PyString, Value)]
objectPairs (PyObject fields order) = [(key, fields M.! key) | key <- toList order]

emptyObject :: Value
emptyObject = Object emptyFields

objectFromPairs :: [(PyString, Value)] -> Value
objectFromPairs = Object . foldl' (\fields (key, value) -> insertField key value fields) emptyFields

field :: Text -> Value -> Value -> Either Text Value
field name fallback (Object (PyObject fields _)) =
  Right (M.findWithDefault fallback (ScalarText name) fields)
field _ _ _ = Left "expected JSON object"

pyTruthy :: Value -> Bool
pyTruthy Null = False
pyTruthy (Bool b) = b
pyTruthy (Number (PyInteger n)) = n /= 0
pyTruthy (Number (PyFloat n)) = n /= 0
pyTruthy (Number PyNaN) = True
pyTruthy (String s) = not (pyStringNull s)
pyTruthy (Array xs) = not (V.null xs)
pyTruthy (Object (PyObject fields _)) = not (M.null fields)

numericBool :: Value -> Value
numericBool (Bool b) = Number (PyInteger (if b then 1 else 0))
numericBool x = x

-- Nothing is IEEE unordered, not EQ: NaN > x and NaN < x are both false.
numberCompare :: PyNumber -> PyNumber -> Maybe Ordering
numberCompare PyNaN _ = Nothing
numberCompare _ PyNaN = Nothing
numberCompare (PyFloat x) _ | isNaN x = Nothing
numberCompare _ (PyFloat y) | isNaN y = Nothing
numberCompare (PyInteger x) (PyInteger y) = Just (compare x y)
numberCompare (PyFloat x) (PyFloat y) = Just (compare x y)
numberCompare (PyInteger x) (PyFloat y)
  | isInfinite y = Just (if y > 0 then LT else GT)
  | otherwise = Just (compare (fromInteger x :: Rational) (toRational y))
numberCompare (PyFloat x) (PyInteger y) = fmap invert (numberCompare (PyInteger y) (PyFloat x))
  where invert LT = GT; invert GT = LT; invert EQ = EQ

-- json.loads returns the decoder's singleton for every NaN token. CPython
-- list/dict equality uses an identity shortcut for their elements, although
-- direct equality on that same NaN is false. Other JSON values need no extra
-- identity representation: equality can distinguish all relevant cases.
containerEqual :: Value -> Value -> Bool
containerEqual (Number PyNaN) (Number PyNaN) = True
containerEqual x y = pyEqual x y

pyEqual :: Value -> Value -> Bool
pyEqual a b = case (numericBool a, numericBool b) of
  (Null, Null) -> True
  (String x, String y) -> x == y
  (Number x, Number y) -> numberCompare x y == Just EQ
  (Array xs, Array ys) -> V.length xs == V.length ys && V.and (V.zipWith containerEqual xs ys)
  (Object (PyObject xs _), Object (PyObject ys _)) -> M.size xs == M.size ys && all
    (\(key,value) -> maybe False (containerEqual value) (M.lookup key ys)) (M.toList xs)
  _ -> False

pyCompare :: Value -> Value -> Either Text (Maybe Ordering)
pyCompare a b = case (numericBool a, numericBool b) of
  (String x, String y) -> Right (Just (compare x y))
  (Number x, Number y) -> Right (numberCompare x y)
  (Array xs, Array ys) -> go (V.toList xs) (V.toList ys)
  _ -> Left "unsupported Python ordering of JSON values"
  where
    go [] [] = Right (Just EQ)
    go [] _ = Right (Just LT)
    go _ [] = Right (Just GT)
    go (x:xs) (y:ys)
      | containerEqual x y = go xs ys
      | otherwise = pyCompare x y

textOf :: Value -> Either Text PyString
textOf (String text) = Right text
textOf (Array blocks) = pyStringConcat <$> traverse blockText (V.toList blocks)
  where
    blockText block@(Object _) = do
      kind <- field "type" Null block
      if kind /= String "text" then pure "" else do
        value <- field "text" (String "") block
        case value of
          String text -> pure text
          _ -> Left "non-string text block"
    blockText _ = pure ""
textOf _ = Right ""

jsonSpace :: Char -> Bool
jsonSpace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

skipSpace :: Text -> Text
skipSpace = T.dropWhile jsonSpace

-- CPython 3.12's default integer conversion limit. Floats are not subject to
-- it, even when their integral part has more than 4300 decimal digits.
integerLimit :: Integer
integerLimit = 10 ^ (4300 :: Int)

decodeJson :: Text -> Either Text Value
decodeJson input
  | "\xfeff" `T.isPrefixOf` input = Left "JSON leading BOM"
  | otherwise = case fastValue (hasHugeExponent input) (A.textToTokens input) of
      Right (value, rest) | T.all jsonSpace rest -> Right value
      _ -> do
        (value, rest) <- slowValue (skipSpace input)
        if T.all jsonSpace rest then Right value else Left "trailing JSON input"

-- Aeson's scanner stores exponent in Int. Route unusually long exponents to
-- the bounded exact scanner before its Int conversion can wrap. This scan is
-- lazy: documents without scientific-number tokens never force it. False
-- positives inside strings only choose the other lossless parser.
hasHugeExponent :: Text -> Bool
hasHugeExponent text = case T.uncons (T.dropWhile (\c -> c /= 'e' && c /= 'E') text) of
  Nothing -> False
  Just (_, rest) ->
    let signed = case T.uncons rest of Just (c,r) | c == '+' || c == '-' -> r; _ -> rest
        (digits, tailText) = T.span asciiDigit signed
    in T.length digits > 6 || hasHugeExponent tailText

fastValue :: Bool -> A.Tokens k String -> Either Text (Value, k)
fastValue huge tokens = case tokens of
  A.TkLit lit rest ->
    let value = case lit of A.LitNull -> Null; A.LitTrue -> Bool True; A.LitFalse -> Bool False
    in Right (value, rest)
  A.TkText text rest -> Right (String (ScalarText text), rest)
  A.TkNumber number rest -> do
    value <- case number of
      A.NumInteger n | abs n < integerLimit -> Right (PyInteger n)
                     | otherwise -> Left "integer exceeds Python digit limit"
      A.NumDecimal n -> floating n
      A.NumScientific n | huge -> Left "use exact exponent scanner"
                        | otherwise -> floating n
    pure (Number value, rest)
  A.TkArrayOpen items -> array [] items
  A.TkRecordOpen fields -> object emptyFields fields
  A.TkErr _ -> Left "JSON scanner rejected input"
  where
    floating n
      -- Scientific does not retain the sign of literal -0.0 / -0e0.
      | Sci.coefficient n == 0 = Left "use signed-zero scanner"
      | otherwise = Right (PyFloat (Sci.toRealFloat n))
    array acc (A.TkItem item) = do
      (value, rest) <- fastValue huge item
      array (value:acc) rest
    array acc (A.TkArrayEnd rest) = Right (Array (V.fromList (reverse acc)), rest)
    array _ (A.TkArrayErr _) = Left "invalid JSON array"
    object !acc (A.TkPair key item) = do
      (value, rest) <- fastValue huge item
      object (insertField (ScalarText (Key.toText key)) value acc) rest
    object acc (A.TkRecordEnd rest) = Right (Object acc, rest)
    object _ (A.TkRecordErr _) = Left "invalid JSON object"

type Parse a = Either Text (a, Text)

slowValue :: Text -> Parse Value
slowValue = slowValueWith stringBody

slowValueWith :: (Text -> Parse PyString) -> Text -> Parse Value
slowValueWith parseString input = case T.uncons input of
  Just ('"', rest) -> do (text, tailText) <- parseString rest; pure (String text, tailText)
  Just ('{', rest) -> objectStart (skipSpace rest)
  Just ('[', rest) -> arrayStart (skipSpace rest)
  Just ('t', _) -> literal "true" (Bool True)
  Just ('f', _) -> literal "false" (Bool False)
  Just ('n', _) -> literal "null" Null
  Just ('N', _) -> literal "NaN" (Number PyNaN)
  Just ('I', _) -> literal "Infinity" (Number (PyFloat (1/0)))
  Just ('-', _) | "-Infinity" `T.isPrefixOf` input -> literal "-Infinity" (Number (PyFloat (-1/0)))
  Just (c, _) | c == '-' || asciiDigit c -> numberValue input
  _ -> Left "invalid JSON value"
  where
    literal prefix value = maybe (Left "invalid JSON literal") (\rest -> Right (value, rest)) (T.stripPrefix prefix input)
    objectStart rest = case T.uncons rest of
      Just ('}', tailText) -> Right (Object emptyFields, tailText)
      _ -> objectNext emptyFields rest
    objectNext !fields rest = do
      afterQuote <- maybe (Left "expected JSON key") Right (T.stripPrefix "\"" rest)
      (key, afterKey) <- parseString afterQuote
      afterColon <- maybe (Left "expected JSON colon") Right (T.stripPrefix ":" (skipSpace afterKey))
      (value, afterValue) <- slowValueWith parseString (skipSpace afterColon)
      let next = insertField key value fields
      case T.uncons (skipSpace afterValue) of
        Just ('}', tailText) -> Right (Object next, tailText)
        Just (',', tailText) -> objectNext next (skipSpace tailText)
        _ -> Left "expected JSON object delimiter"
    arrayStart rest = case T.uncons rest of
      Just (']', tailText) -> Right (Array V.empty, tailText)
      _ -> arrayNext [] rest
    arrayNext acc rest = do
      (value, afterValue) <- slowValueWith parseString rest
      case T.uncons (skipSpace afterValue) of
        Just (']', tailText) -> Right (Array (V.fromList (reverse (value:acc))), tailText)
        Just (',', tailText) -> arrayNext (value:acc) (skipSpace tailText)
        _ -> Left "expected JSON array delimiter"

-- Chunks of ordinary Unicode stay Text; only isolated escaped surrogates
-- create CodePoints. A high/low escape pair becomes one Unicode code point,
-- exactly as json.loads does, and no U+FFFD substitution occurs.
stringBody :: Text -> Parse PyString
stringBody = stringBodyWith (T.span (\c -> c /= '\"' && c /= '\\' && ord c >= 32))

stringBodyWith :: (Text -> (Text, Text)) -> Text -> Parse PyString
stringBodyWith plainSpan = go []
  where
    go acc input =
      let (plain, rest) = plainSpan input
          next = if T.null plain then acc else ScalarText plain : acc
      in case T.uncons rest of
        Just ('"', tailText) -> Right (pyStringConcat (reverse next), tailText)
        Just ('\\', tailText) -> do
          (escaped, afterEscape) <- escape tailText
          go (escaped:next) afterEscape
        _ -> Left "invalid JSON string"
    escape text = case T.uncons text of
      Just ('"', rest) -> Right ("\"", rest)
      Just ('\\', rest) -> Right ("\\", rest)
      Just ('/', rest) -> Right ("/", rest)
      Just ('b', rest) -> Right ("\b", rest)
      Just ('f', rest) -> Right ("\f", rest)
      Just ('n', rest) -> Right ("\n", rest)
      Just ('r', rest) -> Right ("\r", rest)
      Just ('t', rest) -> Right ("\t", rest)
      Just ('u', rest) -> do
        (n, afterHex) <- hex4 rest
        if n >= 0xd800 && n <= 0xdbff then
          case T.stripPrefix "\\u" afterHex of
            Just lowText -> case hex4 lowText of
              Right (low, tailText) | low >= 0xdc00 && low <= 0xdfff ->
                Right (pyStringFromChars [chr (0x10000 + (n-0xd800)*0x400 + low-0xdc00)], tailText)
              _ -> Right (pyStringFromChars [chr n], afterHex)
            Nothing -> Right (pyStringFromChars [chr n], afterHex)
        else Right (pyStringFromChars [chr n], afterHex)
      _ -> Left "invalid JSON escape"
    hex4 text
      | T.length digits /= 4 = Left "incomplete Unicode escape"
      | otherwise = do
          values <- traverse hexDigit (T.unpack digits)
          pure (foldl (\n d -> n*16+d) 0 values, T.drop 4 text)
      where digits = T.take 4 text
    hexDigit c
      | c >= '0' && c <= '9' = Right (ord c - ord '0')
      | c >= 'a' && c <= 'f' = Right (ord c - ord 'a' + 10)
      | c >= 'A' && c <= 'F' = Right (ord c - ord 'A' + 10)
      | otherwise = Left "invalid Unicode escape digit"

asciiDigit :: Char -> Bool
asciiDigit c = c >= '0' && c <= '9'

numberValue :: Text -> Parse Value
numberValue input = do
  let (negative, unsigned) = case T.uncons input of Just ('-',r) -> (True,r); _ -> (False,input)
      (integral, rest) = T.span asciiDigit unsigned
  if T.null integral || (T.length integral > 1 && T.head integral == '0')
    then Left "invalid JSON number" else pure ()
  (fraction, afterFraction, hasFraction) <- case T.uncons rest of
    Just ('.', tailText) ->
      let (digits, remaining) = T.span asciiDigit tailText in
      if T.null digits then Left "missing fraction digits" else Right (digits,remaining,True)
    _ -> Right ("",rest,False)
  let bound = T.length integral + T.length fraction + 10000
  (exponent, remaining, hasExponent) <- case T.uncons afterFraction of
    Just (c, tailText) | c == 'e' || c == 'E' ->
      let (minus, expText) = case T.uncons tailText of
            Just ('-',r) -> (True,r); Just ('+',r) -> (False,r); _ -> (False,tailText)
          (digits, final) = T.span asciiDigit expText
          magnitude = T.foldl' (\n d -> min bound (n*10 + ord d - ord '0')) 0 digits
      in if T.null digits then Left "missing exponent digits"
         else Right (if minus then negate magnitude else magnitude, final, True)
    _ -> Right (0,afterFraction,False)
  let integer = T.foldl' (\n c -> n*10 + fromIntegral (ord c-ord '0')) 0 integral
  if not hasFraction && not hasExponent then
    if T.length integral > 4300 then Left "integer exceeds Python digit limit"
    else Right (Number (PyInteger (if negative then negate integer else integer)), remaining)
  else
    let coefficient = T.foldl' (\n c -> n*10 + fromIntegral (ord c-ord '0')) integer fraction
        magnitude = Sci.toRealFloat (Sci.scientific coefficient (exponent - T.length fraction)) :: Double
        value = if negative then negate magnitude else magnitude
    in Right (Number (PyFloat value), remaining)
