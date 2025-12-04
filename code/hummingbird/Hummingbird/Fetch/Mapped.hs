module Hummingbird.Fetch.Mapped
(
  -- * Fetching from (hash-)maps
  Query (GetMap, Lookup),
  rule,
)
where

import Num
import Text qualified

import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap

import Hummingbird.Fetch (fetch)
import Hummingbird.Fetch qualified as Fetch
import Hummingbird.Prelude

-- |
data Query k v a where

  -- |
  GetMap :: Query k v (HashMap k v)

  -- |
  Lookup :: k -> Query k v (Maybe v)

deriving instance (Show k, Show v) => Show (Query k v a)
deriving instance (Eq k, Eq v) => Eq (Query k v a)

instance (Hashable k, Hashable v) => Hashable (Query k v a) where
  hashWithSalt salt query =
    case query of
      GetMap ->
        hashWithSalt salt (0 :: Int)
      Lookup key ->
        hashWithSalt salt (1 :: Int, key)

rule ::
  (Hashable k)
  => (forall b. Query k v b -> q b)
  -> Query k v a
  -> Fetch.Task q (HashMap k v)
  -> Fetch.Task q a
rule inject query getMap =
  case query of
    GetMap ->
      getMap
    Lookup key ->
      HashMap.lookup key <$> fetch (inject GetMap)
