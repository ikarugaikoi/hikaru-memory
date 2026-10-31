-- | One fold for both a fresh scan and a restored prefix. Input validation is
-- the IO boundary (wake also consults the host timezone); decisions, projection
-- and the restoration recipe are pure. Neither fold nor restore sees ScanMode,
-- a cache path, a file handle or whether this is a full/incremental pass.
module Hikaru.Choubo.Consumer
  ( Consumer(..), ProjectionCodec(..), Restore(..) ) where

import Data.Text (Text)
import Data.Word (Word32)
import Hikaru.Choubo.Types

data Consumer input state checkpoint = Consumer
  { consumerName :: Text
  , consumerVersion :: Word32
  , consumerContext :: Text
  , consumerReadPolicy :: ScanMode -> ReadPolicy
  , consumerInput :: LocatedLine -> IO input
  , consumerInitial :: state
  , consumerStep :: state -> input -> Either Text (Step state)
  , consumerProject :: state -> Maybe checkpoint
  , consumerRestore :: checkpoint -> Restore state
  , consumerCodec :: ProjectionCodec checkpoint
  }

-- There is deliberately no ToJSON state constraint. Only this small, explicit
-- checkpoint can reach the existing Projection whitelist and store checks.
data ProjectionCodec checkpoint = ProjectionCodec
  { encodeProjection :: checkpoint -> Projection
  , decodeProjection :: Projection -> Maybe checkpoint
  }

-- A pending tool ID must be recovered from its source row, never serialized.
-- The store interprets these requests on the already validated OpenedLog.
-- Nothing retains the existing "unrestorable -> full scan" semantics; source
-- read failures remain IO failures, not cache misses.
data Restore state
  = Restored (Maybe state)
  | Reread RowSpan (LocatedLine -> Restore state)
