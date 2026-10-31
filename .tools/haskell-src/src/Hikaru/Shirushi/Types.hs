{-# LANGUAGE OverloadedStrings #-}
-- | Values after the input boundary. All times in mark are local JST.
module Hikaru.Shirushi.Types
  ( Message(..), Speaker(..), MarkError(..), Result
  , Turn, turns, turnSpeaker, turnMessages, turnIndices
  , Shape(..), MarkKind(..), Mark(..), markMessage, markKind
  , Quote(..), Repetition(..), hikaruDay
  ) where

import Control.Exception (Exception(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (LocalTime)
import Hikaru.Conversation.Types (Speaker(..))
import Hikaru.HouseDay (HouseDay, houseDayFromJST)

newtype MarkError = MarkError Text deriving (Eq, Show)
instance Exception MarkError where
  displayException (MarkError detail) = T.unpack detail
type Result a = Either MarkError a

data Message = Message
  { messageTime :: !LocalTime
  , messageSpeaker :: !Speaker
  , messageText :: !Text
  , messageSession :: !(Maybe Text)
  , messageSequence :: !(Maybe Integer)
  } deriving (Eq, Show)

-- A turn always has a first message, and every message has the same speaker.
-- The constructor and record updates are private; grouping is its only entry.
newtype Turn = Turn (NonEmpty (Int, Message)) deriving (Eq, Show)

turns :: NonEmpty Message -> NonEmpty Turn
turns = fmap Turn . NE.groupBy1 sameSpeaker . NE.zip (0 :| [1..])
  where sameSpeaker (_, a) (_, b) = messageSpeaker a == messageSpeaker b

turnSpeaker :: Turn -> Speaker
turnSpeaker (Turn entries) = messageSpeaker (snd (NE.head entries))
turnMessages :: Turn -> NonEmpty Message
turnMessages (Turn entries) = fmap snd entries
turnIndices :: Turn -> [Int]
turnIndices (Turn entries) = map fst (NE.toList entries)

data Shape = Shape
  { shapeLen :: !Int, shapeEndQ :: !Bool, shapeNQ :: !Int, shapeParenN :: !Int
  , shapeParenRatio :: !Double, shapeList :: !Int, shapeBold :: !Int, shapeHeading :: !Int
  , shapeMixed :: !Bool, shapeFirst :: !Text
  } deriving (Eq, Show)

data MarkKind
  = Waiting | Apology | Prompting | Gambling | NotHere | Body | ThirdPerson
  | Declaration | Backstage | Correction | Hesitation | Pleasure | Short
  | QuoteLanguage | Repeated | Crossing | Pictures | Alternative | Stacked
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Evidence keeps its numbers/times until rendering. Only dictionary hits are
-- already words. Quotes and samples retain the actual message, even at ties.
data Mark
  = WordMark !Message !MarkKind !Text
  | QuoteMark !Message !Text !LocalTime
  | RepeatMark !Message !(NonEmpty Repetition)
  | CrossMark !Message !Integer !Int
  | PictureMark !Message !LocalTime !Int
  | ChoiceMark !Message !Text
  | StackMark !Message !Int
  deriving (Eq, Show)

markMessage :: Mark -> Message
markMessage mark = case mark of
  WordMark m _ _ -> m
  QuoteMark m _ _ -> m
  RepeatMark m _ -> m
  CrossMark m _ _ -> m
  PictureMark m _ _ -> m
  ChoiceMark m _ -> m
  StackMark m _ -> m

markKind :: Mark -> MarkKind
markKind mark = case mark of
  WordMark _ kind _ -> kind
  QuoteMark {} -> QuoteLanguage
  RepeatMark {} -> Repeated
  CrossMark {} -> Crossing
  PictureMark {} -> Pictures
  ChoiceMark {} -> Alternative
  StackMark {} -> Stacked

data Quote = Quote
  { quoteText :: !Text, quoteHit :: !(Maybe Message), quoteSameLanguage :: !Bool }
  deriving (Eq, Show)

data Repetition = Repetition
  { repetitionText :: !Text, repetitionCount :: !Int, repetitionDays :: !Int }
  deriving (Eq, Show)

hikaruDay :: LocalTime -> Result HouseDay
hikaruDay = maybe (Left (MarkError "house day outside 1..9999")) Right . houseDayFromJST
