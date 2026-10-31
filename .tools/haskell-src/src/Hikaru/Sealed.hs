-- | 封をした中身と、封を解いた中身。
--
-- ひかるの記録のファイル（.b64）に書けるのは 'Sealed' だけ。明文（'Plain'）を
-- そのまま記録のファイルへ渡すと、型が合わずにコンパイルが通らない（2026-10-22、
-- 重构清单の第 2 步）。'Sealed' の構成子は外に出していないので、作る道は
-- 'sealPlain'（封をして、解き直して確かめる）一つだけ。
--
-- ファイルから読んだバイト列は 'Sealed' にしない。まだ封かどうか分からないので、
-- 'unsealBytes' でそのまま解く。前は読んだバイト列を 'Sealed' に包む関数があって、
-- 封をしていないバイト列でも書く側へ渡せてしまった（Codex の審査、2026-10-22）。
--
-- 解き方の細かいところ（base64 の寛い読み方、UTF-8 の誤りの形）は、固定
-- Python と同じものを Compat の中に残してある。ここは型の境目だけ。
module Hikaru.Sealed
  ( Sealed, Plain(..), SealError(..)
  , sealPlain, unsealBytes, sealedBytes
  ) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (UnicodeException)
import Hikaru.Compat.Base64 (Base64Error, decodeBase64, encodeBase64)

-- | 封を解いた中身。
newtype Plain = Plain { plainText :: Text }

-- | 封をした中身（base64 のバイト列）。'sealPlain' でしか作れない。
newtype Sealed = Sealed BS.ByteString

-- | 封が解けなかったわけ。base64 として読めないか、解いたものが UTF-8 でないか。
data SealError = BadBase64 Base64Error | BadUtf8 UnicodeException

-- | 封をする。封をしたものをもう一度解いて、元と同じバイト列に戻るかを
-- 確かめる（書く前の確認。固定 Python の seal と同じ）。戻らなければ 'Nothing'。
sealPlain :: Plain -> Maybe Sealed
sealPlain (Plain text) =
  let original = TE.encodeUtf8 text
      sealed = encodeBase64 original
  in if decodeBase64 sealed == Right original then Just (Sealed sealed) else Nothing

-- | 記録のファイルから読んだバイト列を解く。
unsealBytes :: BS.ByteString -> Either SealError Plain
unsealBytes bytes = do
  decoded <- either (Left . BadBase64) Right (decodeBase64 bytes)
  either (Left . BadUtf8) (Right . Plain) (TE.decodeUtf8' decoded)

-- | ファイルに書くときのバイト列。
sealedBytes :: Sealed -> BS.ByteString
sealedBytes (Sealed bytes) = bytes
