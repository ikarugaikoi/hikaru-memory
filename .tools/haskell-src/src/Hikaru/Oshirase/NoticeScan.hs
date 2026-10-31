{-# LANGUAGE OverloadedStrings #-}
-- | Notice observes tools, not conversation messages. In particular it does
-- not filter sidechains, projects, meta rows, or non-human origins.
module Hikaru.Oshirase.NoticeScan
  ( ReadState(..), ReadCheckpoint, readConsumer, markerConsumer
  , initialRead, stepRead, restoreRead, projectRead, markerStep, lastComplete, unpagedReading
  ) where

import Control.Monad (foldM)
import Data.Aeson hiding (object)
import Data.Aeson.Types (Parser)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Vector as V
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import Hikaru.Hook.Input
import Hikaru.Oshirase.NoticePending
import Hikaru.Hook.ReadCommand (ReadExtent(..), readExtents, pageIndex)
import Hikaru.Choubo.JsonLine (conversationText)
import Hikaru.Choubo.Consumer
import Hikaru.Choubo.ConsumerProjection
import Hikaru.Choubo.Types

data ReadEvent = Used Integer ToolUseId [ReadExtent] | Returned ToolUseId Bool (Maybe Text) (Maybe Integer) Bool
data ReadState = ReadState
  { readPending :: Pending (RowSpan, Integer, [ReadExtent])
  , readSeen :: Maybe Double
  , readWhole :: Maybe Double
  , readPages :: Map.Map Integer Double
  } deriving (Eq, Show)

initialRead :: ReadState
initialRead = ReadState emptyPending Nothing Nothing Map.empty

parseEvents :: Value -> Parser [ReadEvent]
parseEvents = withObject "notice row" $ \object -> do
  message <- object .:! "message" .!= mempty
  content <- message .:? "content" .!= Null
  case content of
    Array blocks -> do
      kind <- object .:? "type" :: Parser (Maybe Text)
      case kind of
        Just "assistant" -> catMaybes <$> traverse use (zip [0..] (V.toList blocks))
        Just "user" -> do
          stamp <- optionalText object "timestamp"
          catMaybes <$> traverse (result stamp) (V.toList blocks)
        _ -> pure []
    _ -> pure []
  where
    use (ordinal, Object block) = do
      kind <- block .:? "type" :: Parser (Maybe Text)
      name <- block .:? "name" :: Parser (Maybe Text)
      if kind /= Just "tool_use" || name /= Just "Bash" then pure Nothing else do
        ident <- optionalText block "id"
        input <- block .:? "input" .!= mempty
        command <- input .:? "command" .!= ""
        let extents = readExtents command
        pure (if null extents then Nothing else (\i -> Used ordinal i extents) <$> (ident >>= toolUseId))
    use _ = pure Nothing
    result stamp (Object block) = do
      kind <- block .:? "type" :: Parser (Maybe Text)
      if kind /= Just "tool_result" then pure Nothing else do
        ident <- optionalText block "tool_use_id"
        failed <- flag block "is_error"
        output <- block .:? "content" .!= Null >>= contentText
        let total = footerTotal output
            unavailable = MP.parseMaybe outOfRange (T.strip output) == Just ()
        pure ((\i -> Returned i failed stamp total unavailable) <$> (ident >>= toolUseId))
    result _ _ = pure Nothing

mayUse :: Text -> Bool
mayUse line = "tool_use" `T.isInfixOf` line && any (`T.isInfixOf` line)
  ["wake.py", "seal.py", "mezame.py", "fuutou.py", ".tools/bin/mezame", ".tools/bin/fuutou", ".tools/bin/hikaru-core", ".丘の上.md.b64"]

events :: Text -> Either Text [ReadEvent]
events raw = case decodeRow parseEvents (conversationText raw) of
  Left BadJSON -> Left "invalid JSON conversation row"
  Left (BadRow reason) -> Left (T.pack reason)
  Right result -> Right result

-- The least recent of the required pages is the freshness of a complete
-- reading. A later reread of just one page cannot refresh the other pages.
lastComplete :: Int -> ReadState -> Maybe Double
lastComplete total state = case coverage total state of
  Nothing -> readSeen state
  value -> value

-- A whole-file read covers every page regardless of the current layout.
-- With no page reads, no sealed content needs to be opened merely to count it.
unpagedReading :: ReadState -> Maybe (Maybe Double)
unpagedReading state
  | Map.null (readPages state) = Just (readWhole state)
  | Just whole <- readWhole state, all (<= whole) (readPages state) = Just (Just whole)
  | otherwise = Nothing

coverage :: Int -> ReadState -> Maybe Double
coverage total state = max (readWhole state) $ do
  values <- traverse (`Map.lookup` readPages state) [1 .. toInteger total]
  case values of [] -> Nothing; value:rest -> Just (minimum (value:rest))

-- Page totals in old tool output preserve evidence of a completed reading
-- even if an edit later increases the number of pages. No output is stored.
footerTotal :: Text -> Maybe Integer
footerTotal text = case reverse (filter (not . T.null) (T.lines (T.stripEnd text))) of
  suffix:_ | "［ページ " `T.isPrefixOf` suffix -> MP.parseMaybe footer suffix
  _ | not (T.null text) -> Just 1
    | otherwise -> Nothing
  where
    footer :: MP.Parsec Void Text Integer
    footer = MC.string "［ページ " *> (ML.decimal :: MP.Parsec Void Text Integer)
      *> MC.char '/' *> ML.decimal <* MC.char '。'
      <* MP.takeWhileP Nothing (/= '］') <* MC.char '］'

outOfRange :: MP.Parsec Void Text ()
outOfRange = () <$ (MC.string "ページは 1〜" *> (ML.decimal :: MP.Parsec Void Text Integer) *> MC.string " です。")

stepRead :: ReadState -> LocatedLine -> Either Text ReadState
stepRead state line
  | not (mayUse raw || ("tool_result" `T.isInfixOf` raw && pendingRawHit raw (readPending state))) = Right state
  | otherwise = events raw >>= foldM advance state
  where
    raw = locatedText line
    advance old (Used ordinal ident extents) = Right old
      {readPending = pendingInsert ident (locatedSpan line, ordinal, extents) (readPending old)}
    advance old (Returned ident failed stamp hintedTotal unavailable) =
      let (found, pending) = pendingTake ident (readPending old)
          updated = old {readPending = pending}
      in case (found, failed, stamp) of
        (Just (_,_,extents), False, Just value) -> do
          instant <- realToFrac . utcTimeToPOSIXSeconds <$> timestamp value
          let readOne current WholeOka = current {readWhole = max (readWhole current) (Just instant)}
              readOne current (OkaPage _) | unavailable = current
              readOne current (OkaPage page) = current
                {readPages = Map.insertWith max (pageIndex page) instant (readPages current)}
              finished = L.foldl' readOne updated extents
              historical = case hintedTotal of
                Just count_ | count_ > 0 && count_ <= toInteger (maxBound :: Int) -> coverage (fromInteger count_) finished
                _ -> Nothing
          pure finished {readSeen = maximum [readSeen old, readWhole finished, historical]}
        _ -> Right updated

-- This type cannot contain a pending ID, a command, or tool output. Only the
-- pure restore recipe can turn its witnesses back into an in-memory ReadState.
data ReadCheckpoint = ReadCheckpoint
  (Maybe Double) (Maybe Double) [(Integer, Double)] [(RowSpan, Integer)]

readConsumer :: Text -> Consumer LocatedLine ReadState ReadCheckpoint
readConsumer context = Consumer
  { consumerName = "notice_last_read", consumerVersion = 9, consumerContext = context
  , consumerReadPolicy = const ReadAhead8192, consumerInput = pure
  , consumerInitial = initialRead
  , consumerStep = \state line -> Continue <$> stepRead state line
  , consumerProject = projectRead, consumerRestore = restoreRead
  , consumerCodec = ProjectionCodec encodeRead decodeRead }

projectRead :: ReadState -> Maybe ReadCheckpoint
projectRead state = Just (ReadCheckpoint (readSeen state) (readWhole state)
  (Map.toAscList (readPages state))
  [(span_, ordinal) | (_, (span_, ordinal, _)) <- pendingEntries (readPending state)])

encodeRead :: ReadCheckpoint -> Projection
encodeRead (ReadCheckpoint seen whole pages positions) = PFields
  [("seen", timeProjection seen), ("whole", timeProjection whole)
  ,("pages", PList [PFields [("page", PPosition index), ("time", PEpochBits (castDoubleToWord64 stamp))]
                  | (index, stamp) <- pages])
  ,("pending", PList [PFields [("row", spanProjection span_), ("block", PPosition ordinal)]
                    | (span_, ordinal) <- positions])]

-- Only numeric page positions and times enter the ledger. Commands and IDs
-- are recovered from the original tool-use row, just as before.
timeProjection :: Maybe Double -> Projection
timeProjection = PList . maybe [] (pure . PEpochBits . castDoubleToWord64)

restoreTime :: Projection -> Maybe (Maybe Double)
restoreTime (PList []) = Just Nothing
restoreTime (PList [value]) = Just <$> restoreEpoch value
restoreTime _ = Nothing

restoreEpoch :: Projection -> Maybe Double
restoreEpoch (PEpochBits bits)
  | let time = castWord64ToDouble bits, not (isNaN time || isInfinite time) = Just time
restoreEpoch _ = Nothing

decodeRead :: Projection -> Maybe ReadCheckpoint
decodeRead projection = do
  fields <- exactFields ["seen", "whole", "pages", "pending"] projection
  seen <- lookup "seen" fields >>= restoreTime
  whole <- lookup "whole" fields >>= restoreTime
  pages <- case lookup "pages" fields of
    Just (PList xs) -> traverse pageTime xs
    _ -> Nothing
  positions <- case lookup "pending" fields of
    Just (PList xs) -> traverse witness xs
    _ -> Nothing
  pure (ReadCheckpoint seen whole pages positions)
  where
    pageTime value = do
      fields <- exactFields ["page", "time"] value
      index <- positionField "page" fields
      time <- lookup "time" fields >>= restoreEpoch
      if index > 0 then Just (index,time) else Nothing
    witness value = do
      fields <- exactFields ["row", "block"] value
      (,) <$> (lookup "row" fields >>= restoreSpan) <*> positionField "block" fields

restoreRead :: ReadCheckpoint -> Restore ReadState
restoreRead (ReadCheckpoint seen whole pages positions) = go initialRead positions
  where
    go state [] = Restored (Just state {readSeen = seen, readWhole = whole, readPages = Map.fromList pages})
    go state ((span_, ordinal):rest) = Reread span_ $ \line ->
      case recover state span_ ordinal line of
        Nothing -> Restored Nothing
        Just next -> go next rest
    recover state span_ ordinal line = do
        parsed <- either (const Nothing) Just (events (locatedText line))
        case [(ident,extents) | Used index ident extents <- parsed, index == ordinal, mayUse (locatedText line)] of
          [(ident,extents)] -> Just state {readPending = pendingInsert ident (span_, ordinal, extents) (readPending state)}
          _ -> Nothing

markerConsumer :: Text -> Text -> Consumer Text Bool Bool
markerConsumer marker context = Consumer
  { consumerName = "notice_marker", consumerVersion = 5, consumerContext = context
  , consumerReadPolicy = const ReadAhead8192, consumerInput = pure . locatedText
  , consumerInitial = False
  , consumerStep = \given line -> Continue <$> markerStep marker given line
  , consumerProject = Just, consumerRestore = Restored . Just
  , consumerCodec = ProjectionCodec PFlag (\value -> case value of PFlag given -> Just given; _ -> Nothing) }

markerStep :: Text -> Bool -> Text -> Either Text Bool
markerStep marker given raw = do
  reset <- if "compact_boundary" `T.isInfixOf` raw || "isCompactSummary" `T.isInfixOf` raw
    then case decodeRow parseCompact (conversationText raw) of
      Left BadJSON -> Left "invalid JSON conversation row"
      Left (BadRow reason) -> Left (T.pack reason)
      Right value -> Right value
    else Right False
  pure (if reset then False else marker `T.isInfixOf` raw || given)
  where
    parseCompact = withObject "compact row" $ \object -> do
      subtype <- object .:? "subtype" :: Parser (Maybe Text)
      if subtype == Just "compact_boundary" then pure True else flag object "isCompactSummary"
