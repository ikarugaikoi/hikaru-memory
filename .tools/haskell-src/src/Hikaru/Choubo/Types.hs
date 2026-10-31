{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- | No conversation text may be serialized in a scan checkpoint.
module Hikaru.Choubo.Types
  ( ScanMode(..), ScanEnv(..), RowSpan(..), LocatedLine(..), Projection(..)
  , validProjection, safeTime, Step(..), ReadPolicy(..)
  , ScanDisposition(..), ScanResult(..), FileStamp(..), ResumePoint(..)
  ) where

import Data.Aeson
import qualified Data.Aeson.Encoding.Internal as Encoding
import qualified Data.ByteString.Builder as Builder
import qualified Data.Aeson.KeyMap as KM
import qualified Control.Applicative
import Data.Aeson.Types (Parser)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import Data.Time
import Data.Word

data ScanMode = ScanOff | ScanAppend | ScanVerify deriving (Eq,Show)
data ScanEnv = ScanEnv { scanCacheRoot :: FilePath, scanMode :: ScanMode } deriving (Eq,Show)
-- rowOrdinal/resumeRow count physical LF bytes, not logical records.
-- ReadPolicy sizes are IO chunk sizes, never line/record length limits.
data RowSpan = RowSpan { rowBegin :: Integer, rowEnd :: Integer, rowOrdinal :: Integer } deriving (Eq,Show)
data LocatedLine = LocatedLine { locatedSpan :: RowSpan, locatedText :: Text } deriving (Eq,Show)
data ReadPolicy = Line64K | ReadAhead8192 | ReadAhead8192DeferredCR deriving (Eq,Show)
data Step s = Continue s | Finish s
data ScanDisposition = ScanDisabled | ScanRebuilt | ScanReused | ScanExtended | ScanUncached deriving (Eq,Show)
data ScanResult s = ScanResult
  { scanState :: s, scanDisposition :: ScanDisposition
  , scanBytesRead :: Integer, scanMaterializedBytes :: Integer }

-- The integer in PEpochBits is the original binary64 timestamp representation.
-- PTime is accepted only for a restricted, parseable ASCII ISO time lexeme.
data Projection = PPosition Integer | PTime Text | PEpochBits Word64
  | PFlag Bool | PDigest Text | PVersion Word32 | PList [Projection]
  | PFields [(Text,Projection)] deriving (Eq,Show)

safeTime :: Text -> Bool
safeTime value
  | canonicalTime value = True
  | otherwise = originalSafeTime value

-- Positive-only fast path for complete canonical scalar ISO text. Everything
-- else, including leap seconds or extended offsets, keeps the original parser.
canonicalTime :: Text -> Bool
canonicalTime (TI.Text array offset size) = (size == 25 || size == 32)
  && at 4 == 45 && at 7 == 45 && at 10 == 84
  && at 13 == 58 && at 16 == 58 && at (zone+3) == 58
  && (at zone == 43 || at zone == 45)
  && digits 0 4 && digits 5 7 && digits 8 10
  && digits 11 13 && digits 14 16 && digits 17 19
  && digits (zone+1) (zone+3) && digits (zone+4) (zone+6)
  && (size/=32 || (at 19==46 && digits 20 26))
  && year>=1 && two 11<24 && two 14<60 && two 17<60
  && two(zone+1)<24 && two(zone+4)<60
  && maybe False (const True) (fromGregorianValid (toInteger year) (two 5) (two 8))
  where
    zone=if size==32 then 26 else 19
    -- The size guard precedes every read; each fixed index/span lies within it.
    -- Every accepted byte is ASCII. A non-ASCII/other-form Text uses the exact
    -- original parser through safeTime; its internal UTF-8 is never rewritten.
    at i=A.unsafeIndex array (offset+i)
    digits !i !end
      | i==end=True
      | otherwise=let b=at i in b>=48 && b<=57 && digits (i+1) end
    two :: Int -> Int
    two i=fromIntegral(at i)*10+fromIntegral(at(i+1))-528
    year=two 0*100+two 2

-- For these ASCII-only domains, byte length equals scalar length on success.
-- Invalid multi-byte characters fail the same domain check; no byte is read
-- outside the supplied valid Text slice. No machine-word/alignment assumption.
validDigest :: Text -> Bool
validDigest (TI.Text array offset size)=size==64 && go 0
  where
    go !i
      | i==size=True
      | otherwise=let b=A.unsafeIndex array(offset+i)
                  in ((b>=48 && b<=57)||(b>=97 && b<=102)) && go(i+1)

validIdentifier :: Text -> Bool
validIdentifier (TI.Text array offset size)=size>0 && size<=64 && go 0
  where
    go !i
      | i==size=True
      | otherwise=let b=A.unsafeIndex array(offset+i)
                  in ((b>=48 && b<=57)||(b>=65 && b<=90)
                    ||(b>=97 && b<=122)||b==95) && go(i+1)

originalSafeTime :: Text -> Bool
originalSafeTime value = T.length value <= 64 && T.length value >= 10
  && T.all (\c -> c `elem` ("0123456789-+:TZ. " :: String)) value
  && (any iso ["%Y-%m-%dT%H:%M:%S%Q%Ez","%Y-%m-%d %H:%M:%S%Q%Ez"]
      || any local ["%Y-%m-%dT%H:%M:%S%QZ","%Y-%m-%dT%H:%M:%S%Q","%Y-%m-%d %H:%M:%S%Q"]
      || maybe False (const True) (parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack value) :: Maybe Day))
  where
    iso fmt = maybe False (const True) (parseTimeM False defaultTimeLocale fmt (T.unpack value) :: Maybe ZonedTime)
    local fmt = maybe False (const True) (parseTimeM False defaultTimeLocale fmt (T.unpack value) :: Maybe LocalTime)

validProjection :: Projection -> Bool
validProjection value = validNode value && case value of
  PList xs -> all validProjection xs
  PFields fields -> all (validProjection.snd) fields
  _ -> True

-- FromJSON has already parsed and validated each child before its parent.
-- Revalidating the whole subtree at every parent repeats timestamp parsing
-- for every enclosing list/field node in large collect metadata documents.
-- Direct callers of validProjection still validate the complete tree.
validNode :: Projection -> Bool
validNode (PPosition n) = n >= 0
validNode (PTime t) = safeTime t
validNode (PEpochBits _) = True
validNode (PFlag _) = True
validNode (PDigest t) = validDigest t
validNode (PVersion _) = True
validNode (PList _) = True
validNode (PFields fields) = let keys=map fst fields in length(nub keys)==length keys
  && all validIdentifier keys

-- Only fixed JSON framing is specialized. Scalars, including field-name Text,
-- retain the exact Aeson encoders; no validity/domain check is bypassed here.
projectionBuilder :: Projection -> Builder.Builder
projectionBuilder value = case value of
  PPosition n -> Builder.string8 "{\"position\":" <> scalarBuilder n <> Builder.char8 '}'
  PTime t -> Builder.string8 "{\"time\":" <> scalarBuilder t <> Builder.char8 '}'
  PEpochBits n -> Builder.string8 "{\"epoch_bits\":" <> scalarBuilder n <> Builder.char8 '}'
  PFlag b -> Builder.string8 "{\"boolean\":" <> scalarBuilder b <> Builder.char8 '}'
  PDigest t -> Builder.string8 "{\"checksum\":" <> scalarBuilder t <> Builder.char8 '}'
  PVersion n -> Builder.string8 "{\"version\":" <> scalarBuilder n <> Builder.char8 '}'
  PList xs -> Builder.string8 "{\"list\":[" <> projectionItems xs <> Builder.string8 "]}"
  PFields xs -> Builder.string8 "{\"fields\":[" <> fieldItems xs <> Builder.string8 "]}"

scalarBuilder :: ToJSON a => a -> Builder.Builder
scalarBuilder = Encoding.fromEncoding . toEncoding
{-# INLINE scalarBuilder #-}

projectionItems :: [Projection] -> Builder.Builder
projectionItems [] = mempty
projectionItems (x:xs) = projectionBuilder x <> rest xs
  where
    rest [] = mempty
    rest (y:ys) = Builder.char8 ',' <> projectionBuilder y <> rest ys

fieldItems :: [(Text,Projection)] -> Builder.Builder
fieldItems [] = mempty
fieldItems (x:xs) = item x <> rest xs
  where
    item (name,value) = Builder.char8 '[' <> scalarBuilder name <> Builder.char8 ','
      <> projectionBuilder value <> Builder.char8 ']'
    rest [] = mempty
    rest (y:ys) = Builder.char8 ',' <> item y <> rest ys

instance ToJSON Projection where
  toEncoding = Encoding.unsafeToEncoding . projectionBuilder
  toJSON value = case value of
    PPosition n -> object ["position" .= n]
    PTime t -> object ["time" .= t]
    PEpochBits n -> object ["epoch_bits" .= n]
    PFlag b -> object ["boolean" .= b]
    PDigest t -> object ["checksum" .= t]
    PVersion n -> object ["version" .= n]
    PList xs -> object ["list" .= xs]
    PFields xs -> object ["fields" .= xs]
instance FromJSON Projection where
  parseJSON = withObject "projection" $ \o -> do
    let original = foldr (<|>) (fail "invalid projection")
          [PPosition <$> o .: "position", PTime <$> o .: "time", PEpochBits <$> o .: "epoch_bits"
          ,PFlag <$> o .: "boolean", PDigest <$> o .: "checksum", PVersion <$> o .: "version"
          ,PList <$> o .: "list", PFields <$> o .: "fields"]
    value <- case KM.keys o of
      [key] -> (case key of
        "position" -> PPosition <$> o .: "position"
        "time" -> PTime <$> o .: "time"
        "epoch_bits" -> PEpochBits <$> o .: "epoch_bits"
        "boolean" -> PFlag <$> o .: "boolean"
        "checksum" -> PDigest <$> o .: "checksum"
        "version" -> PVersion <$> o .: "version"
        "list" -> PList <$> o .: "list"
        "fields" -> PFields <$> o .: "fields"
        _ -> fail "invalid projection") <|> original
      _ -> fail "invalid projection shape"
    if validNode value then pure value else fail "invalid projection domain"
    where (<|>) :: Parser a -> Parser a -> Parser a
          (<|>) = (Control.Applicative.<|>)

data FileStamp = FileStamp
  { fileIdentity :: Text, fileSize :: Integer, modifiedNs :: Integer, changedNs :: Integer }
  deriving (Eq,Show)
instance ToJSON FileStamp where
  toEncoding s=pairs ("ctime_time" .= changedNs s <> "identity_checksum" .= fileIdentity s
    <> "mtime_time" .= modifiedNs s <> "size_position" .= fileSize s)
  toJSON s=object["identity_checksum" .= fileIdentity s,"size_position" .= fileSize s
                 ,"mtime_time" .= modifiedNs s,"ctime_time" .= changedNs s]
instance FromJSON FileStamp where
  parseJSON=withObject "file stamp" $ \o->FileStamp <$> o .: "identity_checksum" <*> o .: "size_position"
     <*> o .: "mtime_time" <*> o .: "ctime_time"

data ResumePoint = ResumePoint
  { resumeByte :: Integer, resumeRow :: Integer, resumeSkipLF :: Bool } deriving (Eq,Show)
instance ToJSON ResumePoint where
  toEncoding p=pairs ("byte_position" .= resumeByte p <> "row_position" .= resumeRow p
    <> "skip_lf_boolean" .= resumeSkipLF p)
  toJSON p=object["byte_position" .= resumeByte p,"row_position" .= resumeRow p,"skip_lf_boolean" .= resumeSkipLF p]
instance FromJSON ResumePoint where
  parseJSON=withObject "resume position" $ \o->ResumePoint <$> o .: "byte_position" <*> o .: "row_position" <*> o .: "skip_lf_boolean"
