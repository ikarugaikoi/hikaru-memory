module Hikaru.Compat.Set (PySet, emptySet, insertValue, setItems) where
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Hikaru.Compat.Hash
import Hikaru.Compat.Json
newtype PySet = PySet (M.Map Scalar Value)
emptySet :: HashContext -> PySet
emptySet _ = PySet M.empty
setItems :: PySet -> [Value]
setItems (PySet values) = M.elems values
insertValue :: Value -> PySet -> Either Text PySet
insertValue value (PySet values) = do
 key <- scalarKey value
 pure (PySet (M.insertWith (\_ old -> old) key value values))
