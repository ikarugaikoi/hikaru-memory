{-# LANGUAGE OverloadedStrings #-}
-- | とばりの合言葉の状態。
--
-- 始まりの合言葉でとばりが下り、どの終わりの合言葉でも上がる。
-- 終わりの一言そのものも記録しない。どの道具も、この
-- 一つの状態の移り方で見分ける。前は collect 側（'Hikaru.Conversation.Parse' の
-- markHidden）と、ログの索引側（Choubo.Hidden）に、同じ移り方が二つ書いてあった
-- （2026-10-22、重构清单の第 5 步で一つにした）。
--
-- 外から渡す除外の範囲（.タイムマシン/.exclude）は、ここでは扱わない。collect が
-- 会話を一つ読み終えてから、時刻で重ねる。
module Hikaru.Conversation.Hidden
  ( startPhrase, endPhrase, phrasePairs
  , HiddenState(..), HiddenClass(..), initialHidden, classifyHidden, classifyUtterance, classifyText
  , advanceHidden, hiddenByPhrase, phraseOpen
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Hikaru.Compat.Json (pyStringToText)
import Hikaru.Conversation.Tobari (startPhrase, endPhrase, phrasePairs, punctuation)
import Hikaru.Conversation.Input (stripArchive)
import Hikaru.Conversation.Types (Message(..))
import qualified Hikaru.Conversation.Line as Line

normalizePhrase :: Text -> Text
normalizePhrase = T.filter (not . (`T.elem` punctuation)) . stripArchive

starts, ends :: [Text]
starts = map (normalizePhrase . fst) phrasePairs
ends = map (normalizePhrase . snd) phrasePairs

data HiddenState = HiddenState
  { hiddenClosed :: !Bool
  , hiddenTail :: !Bool
  } deriving (Eq, Show)

-- These are classifications, not a replacement for the source message.
-- They may be projected as booleans into the log scan state.  No text or
-- arbitrary JSON value is retained by this type.
data HiddenClass = HiddenClass
  { hiddenTyped :: !Bool
  , hiddenStarts :: !Bool
  , hiddenEnds :: !Bool
  } deriving (Eq, Show)

initialHidden :: HiddenState
initialHidden = HiddenState True False

-- | 合言葉として数えるのは、ユーザーが自分で打った一言だけ（貼り付けや、
-- 道具の出力の中の同じ文字列は数えない）。
classifyHidden :: Message -> HiddenClass
classifyHidden message = case pyStringToText (messageText message) of
  Right body -> classifyText typed body
  -- A lone surrogate cannot be part of any phrase. Keep the legacy Message
  -- boundary here; the classifier and all new callers use Text.
  Left _ -> HiddenClass typed False False
  where typed = messageTyped message

classifyUtterance :: Line.Utterance -> HiddenClass
classifyUtterance message = classifyText (Line.utteranceTyped message) (Line.utteranceText message)

classifyText :: Bool -> Text -> HiddenClass
classifyText typed body = HiddenClass typed
  (typed && normalized `elem` starts) (typed && normalized `elem` ends)
  where normalized = normalizePhrase body

advanceHidden :: HiddenState -> HiddenClass -> HiddenState
advanceHidden state classification
  | not (hiddenTyped classification) = state
  | hiddenStarts classification = HiddenState False False
  | hiddenEnds classification = HiddenState True True
  | otherwise = state {hiddenTail = False}

-- | とばりの中か、終わりの一言そのものか。
hiddenByPhrase :: HiddenState -> Bool
hiddenByPhrase state = not (hiddenClosed state) || hiddenTail state

-- | とばりが、まだ下りているか。
phraseOpen :: HiddenState -> Bool
phraseOpen = not . hiddenClosed
