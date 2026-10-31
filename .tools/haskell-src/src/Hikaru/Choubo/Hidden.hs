-- | The body-free part of _hk.mark_hidden.  Callers still fully parse each
-- accepted message before classifying it.  External exclusion ranges are
-- deliberately absent: collect evaluates them only after parsing a session.
-- The state machine itself lives in "Hikaru.Conversation.Hidden", shared with
-- collect's reader; this module keeps the log-scan consumers' import path.
module Hikaru.Choubo.Hidden
  ( HiddenState(..), HiddenClass(..), initialHidden, classifyHidden
  , advanceHidden, hiddenByPhrase, phraseOpen
  ) where

import Hikaru.Conversation.Hidden
