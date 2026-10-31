-- SHA-256 uses the already-required crypton implementation.
module Hikaru.Compat.SHA256 (sha256) where
import Crypto.Hash (Digest,SHA256,hash)
import qualified Data.ByteString as B
sha256 :: B.ByteString -> IO String
sha256 input = pure (show (hash input :: Digest SHA256))
