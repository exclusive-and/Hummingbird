module Hummingbird.Codebase.Hash where

import Control.Monad
import Control.Monad.Fail
import Data.Binary
import Data.ByteArray qualified as ByteArray
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Crypto.Hash
import Prelude
import Prettyprinter

newtype Hash = Hash (Crypto.Hash.Digest SHA3_512)
  deriving stock
    ( Eq
    , Generic
    , Ord
    , Show
    )

instance Binary Hash where
  get = do
    bs <- get @ByteString
    case digestFromByteString bs of
      Just digest -> pure (Hash digest)
      Nothing     -> fail "Hummingbird.Codebase.Hash.get: could not convert bytestring to hash"
  
  put (Hash digest) =
    put @ByteString $ ByteArray.convert digest

instance Pretty Hash where
  pretty (Hash a) =
    angles $ pretty (Text.pack $ take 8 $ show a) <> "..."
