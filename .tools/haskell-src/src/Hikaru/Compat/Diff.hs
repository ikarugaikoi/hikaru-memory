{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | CPython 3.12.12 difflib for immutable lists of code-point strings.
-- Port of Lib/difflib.py: SequenceMatcher and unified_diff, not general LCS.
-- PSF license and source attribution: docs/CPython-LICENSE.txt and
-- audit/cpython-diff-source.json. No Python calls, foreign code or IO.
module Hikaru.Compat.Diff
  ( SequenceMatcher, Match(..), Tag(..), Opcode(..), DiffError(..)
  , sequenceMatcher, setSeq2, matchingBlocks, opcodes, groupedOpcodes
  , findLongestMatch, bJunk, bPopular
  , UnifiedOptions(..), defaultUnifiedOptions, unifiedDiff, formatRangeUnified, splitLF
  ) where

import qualified Data.IntMap.Strict as IM
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Vector as V
import Hikaru.Compat.Json (PyString, pyStringFromChars, pyStringToChars, pyStringConcat, pyStringNull)

data Match = Match { matchA :: !Integer, matchB :: !Integer, matchSize :: !Integer }
  deriving (Eq,Ord,Show)
data Tag = Replace | Delete | Insert | Equal deriving (Eq,Ord,Show)
data Opcode = Opcode
  { opcodeTag :: !Tag, aStart :: !Integer, aEnd :: !Integer, bStart :: !Integer, bEnd :: !Integer }
  deriving (Eq,Show)
data DiffError = IndexError deriving (Eq,Show)

data SequenceMatcher = SequenceMatcher
  { seqA :: !(V.Vector PyString), seqB :: !(V.Vector PyString)
  , junkPredicate :: !(Maybe (PyString -> Bool)), autoJunk :: !Bool
  , indices :: !(M.Map PyString [Int])
  , bJunk :: !(S.Set PyString), bPopular :: !(S.Set PyString)
  , cachedMatches :: Maybe [Match], cachedOpcodes :: Maybe [Opcode] }

sequenceMatcher :: Maybe (PyString -> Bool) -> Bool -> [PyString] -> [PyString] -> SequenceMatcher
sequenceMatcher junk automatic a b = setSeq2 b $
  SequenceMatcher (V.fromList a) V.empty junk automatic M.empty S.empty S.empty Nothing Nothing


setSeq2 :: [PyString] -> SequenceMatcher -> SequenceMatcher
setSeq2 b matcher = matcher
  {seqB=vector,indices=filtered,bJunk=junk,bPopular=popular,
   cachedMatches=Nothing,cachedOpcodes=Nothing}
  where
    vector = V.fromList b
    (reverseIndices,reverseOrder) = foldl' insert (M.empty,[]) (zip [0..] b)
    insert (current,keys) (i,value) = (M.insertWith (++) value [i] current,if M.member value current then keys else value:keys)
    order = reverse reverseOrder
    table = M.map reverse reverseIndices
    junk = S.fromList [value | value <- order,maybe False ($ value) (junkPredicate matcher)]
    withoutJunk = M.withoutKeys table junk
    cutoff = length b `div` 100 + 1
    popular = if autoJunk matcher && length b >= 200
      then M.keysSet $ M.filter ((>cutoff) . length) withoutJunk else S.empty
    filtered = M.withoutKeys withoutJunk popular


size :: V.Vector a -> Integer
size = toInteger . V.length

at :: V.Vector a -> Integer -> Either DiffError a
at vector index
  | normalized < 0 || normalized >= size vector = Left IndexError
  | otherwise = Right $ vector V.! fromInteger normalized
  where normalized = if index < 0 then size vector+index else index

findLongestMatch :: SequenceMatcher -> Integer -> Maybe Integer -> Integer -> Maybe Integer -> Either DiffError Match
findLongestMatch matcher alo ahi_ blo bhi_ = do
  initial <- search alo IM.empty (Match alo blo 0)
  left <- extendLeft False initial
  right <- extendRight False left
  junkLeft <- extendLeft True right
  extendRight True junkLeft
  where
    a = seqA matcher; b = seqB matcher
    ahi = fromMaybe (size a) ahi_; bhi = fromMaybe (size b) bhi_
    search !i !previous !best
      | i >= ahi = Right best
      | otherwise = do
          value <- at a i
          let positions = takeWhile ((<bhi) . toInteger) $ dropWhile ((<blo) . toInteger) $ M.findWithDefault [] value $ indices matcher
              step (!current,!longest) j =
                let k = IM.findWithDefault 0 (j-1) previous+1
                    longer = if k > matchSize longest then Match (i-k+1) (toInteger j-k+1) k else longest
                in (IM.insert j k current,longer)
              (next,best') = foldl' step (IM.empty,best) positions
          search (i+1) next best'
    extendLeft wantJunk current@(Match i j k)
      | i <= alo || j <= blo = Right current
      | otherwise = do
          bv <- at b (j-1)
          if S.member bv (bJunk matcher) /= wantJunk then Right current else do
            av <- at a (i-1)
            if av==bv then extendLeft wantJunk (Match (i-1) (j-1) (k+1)) else Right current
    extendRight wantJunk current@(Match i j k)
      | i+k >= ahi || j+k >= bhi = Right current
      | otherwise = do
          bv <- at b (j+k)
          if S.member bv (bJunk matcher) /= wantJunk then Right current else do
            av <- at a (i+k)
            if av==bv then extendRight wantJunk (Match i j (k+1)) else Right current

matchingBlocks :: SequenceMatcher -> ([Match],SequenceMatcher)
matchingBlocks matcher = case cachedMatches matcher of
  Just result -> (result,matcher)
  Nothing -> (result,matcher {cachedMatches=Just result})
    where
      la = size $ seqA matcher; lb = size $ seqB matcher
      result = collapse (sort $ find [(0,la,0,lb)] []) ++ [Match la lb 0]
      find [] acc = acc
      find ((alo,ahi,blo,bhi):pending) acc = case findLongestMatch matcher alo (Just ahi) blo (Just bhi) of
        Left _ -> error "SequenceMatcher internal bounds invariant"
        Right m@(Match i j k)
          | k==0 -> find pending acc
          | otherwise -> let left = [(alo,i,blo,j) | alo<i && blo<j]
                             right = [(i+k,ahi,j+k,bhi) | i+k<ahi && j+k<bhi]
                         in find (right++left++pending) (m:acc)
      collapse [] = []
      collapse (first:rest) = go first rest
        where
          go current [] = [current]
          go current@(Match i j k) (next@(Match x y n):tail_)
            | i+k==x && j+k==y = go (Match i j (k+n)) tail_
            | otherwise = current:go next tail_

opcodes :: SequenceMatcher -> ([Opcode],SequenceMatcher)
opcodes matcher = case cachedOpcodes matcher of
  Just result -> (result,matcher)
  Nothing -> let (blocks,next) = matchingBlocks matcher
                 result = build 0 0 blocks
             in (result,next {cachedOpcodes=Just result})
  where
    build _ _ [] = []
    build i j (Match ai bj n:rest) =
      let tag | i<ai && j<bj = Just Replace
              | i<ai = Just Delete
              | j<bj = Just Insert
              | otherwise = Nothing
      in [Opcode t i ai j bj | Just t <- [tag]] ++ [Opcode Equal ai (ai+n) bj (bj+n) | n/=0] ++ build (ai+n) (bj+n) rest

-- Grouped opcodes are consumed once by unifiedDiff; no mutated matcher escapes.
groupedOpcodes :: Integer -> SequenceMatcher -> [[Opcode]]
groupedOpcodes n matcher = group [] trimmed
  where
    (original,_) = opcodes matcher
    initial = if null original then [Opcode Equal 0 1 0 1] else original
    leading (c:cs) | opcodeTag c==Equal = c {aStart=max (aStart c) (aEnd c-n),bStart=max (bStart c) (bEnd c-n)}:cs
    leading cs = cs
    trailing cs = case reverse cs of
      c:rest | opcodeTag c==Equal -> reverse $ c {aEnd=min (aEnd c) (aStart c+n),bEnd=min (bEnd c) (bStart c+n)}:rest
      _ -> cs
    trimmed = trailing $ leading initial
    group current [] = case reverse current of
      [] -> []
      [one] | opcodeTag one==Equal -> []
      final -> [final]
    group current (c:rest)
      | opcodeTag c==Equal && aEnd c-aStart c > n+n =
          let prefix = c {aEnd=min (aEnd c) (aStart c+n),bEnd=min (bEnd c) (bStart c+n)}
              suffix = c {aStart=max (aStart c) (aEnd c-n),bStart=max (bStart c) (bEnd c-n)}
          in reverse (prefix:current):group [suffix] rest
      | otherwise = group (c:current) rest


data UnifiedOptions = UnifiedOptions
  { fromFile :: !PyString, toFile :: !PyString, fromFileDate :: !PyString, toFileDate :: !PyString
  , contextLines :: !Integer, lineTerm :: !PyString }
  deriving Show

defaultUnifiedOptions :: UnifiedOptions
defaultUnifiedOptions = UnifiedOptions "" "" "" "" 3 "\n"

formatRangeUnified :: Integer -> Integer -> PyString
formatRangeUnified start stop = pyStringFromChars $
  if amount==1 then show (start+1) else show (if amount==0 then start else start+1)++","++show amount
  where amount = stop-start

slice :: V.Vector a -> Integer -> Integer -> [a]
slice vector start end = V.toList $ V.slice (fromInteger lo) (fromInteger $ max 0 $ hi-lo) vector
  where
    length_ = size vector
    normalize x = max 0 $ min length_ $ if x<0 then length_+x else x
    lo = normalize start; hi = normalize end

unifiedDiff :: UnifiedOptions -> [PyString] -> [PyString] -> [PyString]
unifiedDiff options a b = case groups of
  [] -> []
  _ -> [join ["--- ",fromFile options,date $ fromFileDate options,lineTerm options],
        join ["+++ ",toFile options,date $ toFileDate options,lineTerm options]] ++ concatMap renderGroup groups
  where
    matcher = sequenceMatcher Nothing True a b
    groups = groupedOpcodes (contextLines options) matcher
    join = pyStringConcat
    date text = if pyStringNull text then "" else join ["\t",text]
    renderGroup [] = error "difflib group invariant"
    renderGroup group@(first:_) =
      let last_ = foldl' (\_ x -> x) first group
      in join ["@@ -",formatRangeUnified (aStart first) (aEnd last_)," +",
               formatRangeUnified (bStart first) (bEnd last_)," @@",lineTerm options] : concatMap emit group
    emit c = case opcodeTag c of
      Equal -> map (\line -> join [" ",line]) $ slice (seqA matcher) (aStart c) (aEnd c)
      other -> (if other==Replace || other==Delete then map (\line -> join ["-",line]) $ slice (seqA matcher) (aStart c) (aEnd c) else [])
               ++ (if other==Replace || other==Insert then map (\line -> join ["+",line]) $ slice (seqB matcher) (bStart c) (bEnd c) else [])

-- str.split("\n"), including the trailing empty element and empty input.
splitLF :: PyString -> [PyString]
splitLF = map pyStringFromChars . split . pyStringToChars
  where split input = case break (=='\n') input of
          (line,[]) -> [line]
          (line,_:rest) -> line:split rest
