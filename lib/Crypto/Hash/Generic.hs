module Crypto.Hash.Generic where

import Data.Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Prelude
import Prettyprinter

import Crypto.Hash qualified
import Crypto.Hash
  ( Digest
  , HashAlgorithm
  )

-- | Get cryptographic hashes of generic datatypes via their 'Binary' instance.
hashWith ::
  (HashAlgorithm algo, Binary a)
  => algo
  -> a
  -> Digest algo

hashWith algo =
  Crypto.Hash.hashWith algo . ByteString.Lazy.toStrict . encode
