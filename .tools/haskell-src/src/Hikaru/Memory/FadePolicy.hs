-- | Shared vocabulary and thresholds for selection and the displayed guide.
module Hikaru.Memory.FadePolicy
  ( Step(..), fadingSteps, fromLayer, toLayer, limit, ageDays, keepMark, keepMarks ) where
import Data.List (intercalate)
import Hikaru.Memory.Strobe (Layer(..), Keep(..), keepName)

data Step = RecentToMiddle | MiddleToOld deriving (Eq, Show)
fadingSteps :: [Step]
fadingSteps = [RecentToMiddle, MiddleToOld]
fromLayer, toLayer :: Step -> Layer
fromLayer RecentToMiddle = Konogoro
fromLayer MiddleToOld = SukoshiMae
toLayer RecentToMiddle = SukoshiMae
toLayer MiddleToOld = Mukashi
limit :: Step -> Int
limit RecentToMiddle = 30000
limit MiddleToOld = 15000
ageDays :: Integer
ageDays = 31
keepMark :: Keep -> String
keepMark k = "（" ++ keepName k ++ "）"
keepMarks :: String -> String
keepMarks separator = intercalate separator (map keepMark [minBound .. maxBound])
