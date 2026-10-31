{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Positive-only cache decoder. A miss MUST use the original Aeson decoder.
-- Recognizes only the exact layout emitted by the existing ToEncoding methods;
-- it neither changes the cache format nor replaces any store/checksum checks.
module Hikaru.Choubo.Canonical
  ( CanonicalEntry(..), CanonicalEnvelope(..)
  , decodeProjection, decodeEntry, decodeEnvelope, decodeEnvelopePayload
  ) where

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)
import Hikaru.Choubo.Types

-- Strict intermediate records keep the Store's private constructors private.
data CanonicalEntry = CanonicalEntry
  { canonicalVersion :: !Word32, canonicalContext :: !Text, canonicalStamp :: !FileStamp
  , canonicalHead :: !Text, canonicalBoundary :: !Text, canonicalPrefix :: !(Maybe Text)
  , canonicalPoint :: !ResumePoint, canonicalStable :: !Projection, canonicalResult :: !Projection
  , canonicalGeneration :: !Text, canonicalCoverage :: !Integer
  , canonicalStopped :: !Bool, canonicalTail :: !Bool } deriving (Eq,Show)
data CanonicalEnvelope = CanonicalEnvelope !CanonicalEntry !Text deriving (Eq,Show)

newtype P a = P { runP :: B.ByteString -> Maybe (a,B.ByteString) }
instance Functor P where
  fmap f (P action)=P $ \b->do (a,r)<-action b;pure(f a,r)
instance Applicative P where
  pure a=P $ \b->Just(a,b)
  P f <*> P x=P $ \b->do (g,r)<-f b;(a,s)<-x r;pure(g a,s)
instance Monad P where
  P x >>= f=P $ \b->do (a,r)<-x b;runP(f a)r
instance Alternative P where
  empty=P $ const Nothing
  P a <|> P b=P $ \x->a x <|> b x

literal :: B.ByteString -> P ()
literal expected=P $ \input->((),) <$> B.stripPrefix expected input
ensure :: Bool -> P ()
ensure True=pure()
ensure False=empty
complete :: P a -> B.ByteString -> Maybe a
complete parser input=do (a,rest)<-runP parser input;if B.null rest then Just a else Nothing

-- Keep the exact contiguous source range consumed by a successful parser.
-- Safe ByteString slicing respects any nonzero starting offset. The returned
-- payload is exposed only after the complete envelope parser succeeds at EOF.
-- A successful canonical payload has exactly the existing toEncoding spelling.
capture :: P a -> P (a,B.ByteString)
capture (P parser)=P $ \input->do
  (value,rest)<-parser input
  let consumed=B.length input-B.length rest
  if consumed<0 then Nothing else Just((value,B.take consumed input),rest)

-- No escape decoding or lenient Unicode conversion exists here. Anything
-- outside printable ASCII goes to Aeson, including all JSON escape sequences.
text :: P Text
text=do
  literal "\""
  P $ \input->let (body,rest)=B.break(==34)input in do
    remaining<-B.stripPrefix "\"" rest
    if B.all(\b->b>=32 && b<127 && b/=92)body
      then Just(TE.decodeUtf8 body,remaining) else Nothing

-- Decimal subset of Aeson's Integer domain. Fraction/exponent/-0/leading-zero
-- spellings must fall back, even if the general decoder accepts their value.
integer :: P Integer
integer=P $ \input->do
  let (negative,source)=case B.uncons input of Just(45,rest)->(True,rest);_->(False,input)
      (digits,_)=B.span(\b->b>=48 && b<=57)(B.take 65 source)
      remaining=B.drop(B.length digits)source
  (first,_)<-B.uncons digits
  if B.length digits>64 || (B.length digits>1 && first==48) || (negative && digits=="0") then Nothing else do
    (value,left)<-BC.readInteger digits
    if B.null left then Just(if negative then negate value else value,remaining) else Nothing
bounded :: Integer -> P Integer
bounded maximum_=do n<-integer;ensure(n>=0 && n<=maximum_);pure n
word32 :: P Word32
word32=fromInteger <$> bounded(toInteger(maxBound::Word32))
word64 :: P Word64
word64=fromInteger <$> bounded(toInteger(maxBound::Word64))
boolean :: P Bool
boolean=(literal "true">>pure True)<|>(literal "false">>pure False)
nullableText :: P (Maybe Text)
nullableText=(literal "null">>pure Nothing)<|>(Just <$> text)

-- This is only a fast-path recursion limit; hitting it returns to Aeson and
-- does not reject a document that the existing decoder would accept.
projection :: Int -> P Projection
projection depth=do
  ensure(depth<128)
  literal "{\""
  P $ \input->do
    (first,_)<-B.uncons input
    runP (case first of
      112 -> literal "position\":" *> (PPosition <$> integer) <* literal "}"
      116 -> literal "time\":" *> (PTime <$> text) <* literal "}"
      101 -> literal "epoch_bits\":" *> (PEpochBits <$> word64) <* literal "}"
      98 -> literal "boolean\":" *> (PFlag <$> boolean) <* literal "}"
      99 -> literal "checksum\":" *> (PDigest <$> text) <* literal "}"
      118 -> literal "version\":" *> (PVersion <$> word32) <* literal "}"
      108 -> literal "list\":" *> (PList <$> array(projection(depth+1))) <* literal "}"
      102 -> literal "fields\":" *> (PFields <$> array(field(depth+1))) <* literal "}"
      _ -> empty) input
  where
    field d=do literal "[";key<-text;literal ",";value<-projection d;literal "]";pure(key,value)

array :: P a -> P [a]
array item=do
  literal "["
  (literal "]">>pure[]) <|> (do x<-item;go [x])
  where
    go !backwards=(literal "]">>pure(reverse backwards)) <|> do
      literal ",";x<-item;go(x:backwards)

-- Preserve the complete public domain check, including nested fields and
-- the original safeTime fallback. Lexical recognition is never authorization.
validatedProjection :: P Projection
validatedProjection=do value<-projection 0;ensure(validProjection value);pure value

stamp :: P FileStamp
stamp=do
  literal "{\"ctime_time\":";changed<-integer
  literal ",\"identity_checksum\":";identity<-text
  literal ",\"mtime_time\":";modified<-integer
  literal ",\"size_position\":";size<-integer
  literal "}"
  pure(FileStamp identity size modified changed)
resume :: P ResumePoint
resume=do
  literal "{\"byte_position\":";byte<-integer
  literal ",\"row_position\":";row<-integer
  literal ",\"skip_lf_boolean\":";skip<-boolean
  literal "}"
  pure(ResumePoint byte row skip)
entry :: P CanonicalEntry
entry=do
  literal "{\"boundary_checksum\":";boundary<-text
  literal ",\"context_checksum\":";context<-text
  literal ",\"coverage_position\":";coverage<-integer
  literal ",\"generation_checksum\":";generation<-text
  literal ",\"head_checksum\":";head_<-text
  literal ",\"prefix_checksum\":";prefix<-nullableText
  literal ",\"result\":";result<-validatedProjection
  literal ",\"resume\":";point<-resume
  literal ",\"rule_version\":";version<-word32
  literal ",\"source\":";source<-stamp
  literal ",\"stable\":";stable<-validatedProjection
  literal ",\"stopped_boolean\":";stopped<-boolean
  literal ",\"tail_boolean\":";tail_<-boolean
  literal "}"
  pure(CanonicalEntry version context source head_ boundary prefix point stable result generation coverage stopped tail_)
envelope :: P (CanonicalEnvelope,B.ByteString)
envelope=do
  literal "{\"checksum\":";checksum<-text
  literal ",\"entry\":";(value,payload)<-capture entry
  literal ",\"format_version\":3}"
  pure(CanonicalEnvelope value checksum,payload)
decodeProjection :: B.ByteString -> Maybe Projection
decodeProjection=complete validatedProjection
decodeEntry :: B.ByteString -> Maybe CanonicalEntry
decodeEntry=complete entry
decodeEnvelope :: B.ByteString -> Maybe CanonicalEnvelope
decodeEnvelope=fmap fst . decodeEnvelopePayload
-- Full grammar/domain/EOF success is mandatory before either slice escapes.
decodeEnvelopePayload :: B.ByteString -> Maybe (CanonicalEnvelope,B.ByteString)
decodeEnvelopePayload=complete envelope
