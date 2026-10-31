-- | UTF-8 output for native Text at the command boundary.
module Hikaru.Output (outputText) where
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import System.IO (stdout)

outputText :: Text -> IO ()
outputText = B.hPut stdout . TE.encodeUtf8
