{-# LANGUAGE OverloadedStrings #-}
-- | Failure-only diagnostics for _hk.ts's replace/fromisoformat operation.
-- CPython 3.12.12 checks all lexical fields, constructs the timezone, then
-- checks Gregorian and clock ranges. A successful business parser is never
-- replaced here. Derived ordering: Modules/_datetimemodule.c (PSF license;
-- fixed source and license in .build/cpython-3.12.12, audit in docs).
module Hikaru.Compat.DateTimeDiagnostic (timestampDiagnostic,fromIsoformatDiagnostic) where
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (fromGregorianValid,toGregorian)
import Data.Time.Calendar.WeekDate (fromWeekDate,toWeekDate)
import Hikaru.Compat.Diagnostic
import Hikaru.Compat.Json
import Hikaru.Compat.Render (pyRepr)
import Hikaru.Compat.Time (clockParts)
import Hikaru.Compat.ValueDiagnostic (missingAttribute)

timestampDiagnostic :: Text -> Value -> Diagnostic
timestampDiagnostic = diagnose True

-- datetime.fromisoformat itself has no preceding global replace operation.
fromIsoformatDiagnostic :: Text -> Value -> Diagnostic
fromIsoformatDiagnostic = diagnose False

diagnose :: Bool -> Text -> Value -> Diagnostic
diagnose _ reason _
  | reason `elem` ["datetime conversion outside year range", "hikaru day outside datetime year range"] =
      namedDiagnostic "OverflowError"
diagnose replace _ (String raw) = case lexical replace raw of
  Left () -> invalid
  Right (year,month,day,hour,minute,second,offset)
    | abs offset >= 86400000000 -> Diagnostic "ValueError" $ pyStringConcat
        ["offset must be a timedelta strictly between -timedelta(hours=24) and timedelta(hours=24), not ",delta offset,"."]
    | year<1 || year>9999 || fromGregorianValid year (fromInteger month) (fromInteger day)==Nothing ->
        dateDiagnostic year month day
    | hour>23 -> valueError "hour must be in 0..23"
    | minute>59 -> valueError "minute must be in 0..59"
    | second>59 -> valueError "second must be in 0..59"
    | otherwise -> invalid
  where
    invalid = valueError $ pyStringConcat ["Invalid isoformat string: ",pyRepr(String $ pyStringFromChars $ (if replace then replaceZ else id) $ pyStringToChars raw)]
diagnose True _ value = missingAttribute "replace" value
diagnose False _ _ = Diagnostic "TypeError" "fromisoformat: argument must be str"

valueError :: PyString -> Diagnostic
valueError = Diagnostic "ValueError"

replaceZ :: String -> String
replaceZ []=[]
replaceZ ('Z':xs)="+00:00"++replaceZ xs
replaceZ (x:xs)=x:replaceZ xs

-- CPython sanitizes at most one potential separator before strict UTF-8.
sanitize :: Bool -> PyString -> Either () Text
sanitize replace raw = do
  let chars=(if replace then replaceZ else id)(pyStringToChars raw)
      surrogate c=ord c>=0xd800 && ord c<=0xdfff
      candidates=[n|n<-[7,8,10],n<length chars,surrogate(chars!!n)]
      clean=case candidates of
        n:_->take n chars++"T"++drop(n+1)chars
        []->chars
  if length chars<7 || any surrogate clean then Left() else Right(T.pack clean)

decimal :: Int -> Int -> Text -> Either () Integer
decimal start count text =
  let value=T.take count(T.drop start text)
  in if T.length value/=count || T.any(\c->c<'0'||c>'9')value then Left()
     else Right(T.foldl'(\n c->n*10+toInteger(ord c-ord '0'))0 value)

separator :: Text -> Either () Int
separator text
  | size<7=Left()
  | size==7=Right 7
  | at 4=='-' =
      if at 5/='W' then Right 10
      else if size>8 && at 8=='-'
           then if size==9 then Left() else if size>10 && digit(at 10) then Right 8 else Right 10
           else Right 8
  | at 4=='W' =
      let end=7+T.length(T.takeWhile digit(T.drop 7 text))
      in Right(if end<9 then end else if even end then 7 else 8)
  | otherwise=Right 8
  where size=T.length text;at=T.index text;digit c=c>='0'&&c<='9'

dateFields :: Int -> Text -> Either () (Integer,Integer,Integer)
dateFields end text = do
  year<-decimal 0 4 text
  let separated=T.take 1(T.drop 4 text)=="-"
      start=if separated then 5 else 4
  if T.take 1(T.drop start text)=="W" then do
    week<-decimal(start+1)2 text
    let afterWeek=start+3
    day<-if afterWeek<end then do
      if separated && T.take 1(T.drop afterWeek text)/="-" then Left() else pure()
      decimal(if separated then afterWeek+1 else afterWeek)1 text
      else Right 1
    let result=fromWeekDate year(fromInteger week)(fromInteger day)
    if year<1 || year>9999 || week<1 || week>53 || day<1 || day>7
       || toWeekDate result/=(year,fromInteger week,fromInteger day) then Left()
      else let(y,m,d)=toGregorian result in Right(y,toInteger m,toInteger d)
  else do
    month<-decimal start 2 text
    if separated && T.take 1(T.drop 7 text)/="-" then Left() else pure()
    day<-decimal(if separated then 8 else 6)2 text
    pure(year,month,day)

lexical :: Bool -> PyString -> Either () (Integer,Integer,Integer,Int,Int,Int,Integer)
lexical replace raw = do
  text<-sanitize replace raw
  end<-separator text
  (year,month,day)<-dateFields end text
  if T.length text<=end then pure(year,month,day,0,0,0,0) else do
    let (clock,zone)=T.break(\c->c=='+'||c=='-'||c=='Z')(T.drop(end+1)text)
        simplify=either(const$Left())Right
        following=if T.null zone then 0 else if T.take 1 zone=="Z" then 90 else 43
    ((hour,minute,second,_),trailing)<-simplify(clockParts following clock)
    if T.null zone then
      if trailing then Left() else pure(year,month,day,hour,minute,second,0)
    else if T.take 1 zone=="Z" then
      if T.length zone==1 || T.take 1(T.drop 1 zone)=="\0" then pure(year,month,day,hour,minute,second,0)
      else Left()
    else do
      ((zh,zm,zs,zf),zoneTrailing)<-simplify(clockParts 0(T.drop 1 zone))
      if zoneTrailing then Left() else pure()
      let seconds=toInteger(zh*3600+zm*60+zs)
          offset=if seconds==0 then 0 else seconds*1000000+toInteger zf
      pure(year,month,day,hour,minute,second,if T.take 1 zone=="-" then negate offset else offset)

delta :: Integer -> PyString
delta micros =
  let (days,remainder)=micros `divMod` 86400000000
      (seconds,micro)=remainder `divMod` 1000000
      parts=["days="++show days|days/=0]++["seconds="++show seconds|seconds/=0]++["microseconds="++show micro|micro/=0]
  in pyStringFromChars("datetime.timedelta("++(if null parts then "0" else T.unpack(T.intercalate ", "$map T.pack parts))++")")
