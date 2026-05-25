module Hummingbird.Codebase.Hash where

import Data.Array.Byte
import Data.Binary
import Data.ByteArray qualified as ByteArray
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Hashable
import Data.Text qualified as Text
import Crypto.Hash
import Prelude
import Prettyprinter
import Unsafe.Coerce (unsafeCoerce)

newtype Hash = Hash (Crypto.Hash.Digest SHA3_512)
  deriving stock
    ( Eq
    , Ord
    , Show
    )
  deriving newtype (ByteArray.ByteArrayAccess)

instance Binary Hash where
  get = do
    bs <- get @ByteString
    case digestFromByteString bs of
      Just digest -> pure (Hash digest)
      Nothing     -> fail "Hummingbird.Codebase.Hash.get: could not convert bytestring to hash"
  
  put (Hash digest) =
    put @ByteString $ ByteArray.convert digest

instance Hashable Hash where
  salt `hashWithSalt` Hash digest =
    hashWithSalt
      salt
      (unsafeCoerce @_ @ByteArray digest)
    -- 'unsafeCoerce' is a necessary evil to make this function work. Digest and
    -- ByteArray are representationally equal under the hood; crypton hides
    -- the equality from us to protect the integrity of the individual hashes.

instance Pretty Hash where
  pretty (Hash a) =
    angles $ pretty (Text.pack $ take 8 $ show a) <> "..."
