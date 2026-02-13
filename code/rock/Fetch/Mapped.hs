module Fetch.Mapped where

import Num
import Prelude

import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap

import Fetch (fetch, Task)

-- |
data Query k v a where

  -- |
  GetMap :: Query k v (HashMap k v)

  -- |
  Lookup :: k -> Query k v (Maybe v)

instance (Hashable k, Hashable v) => Hashable (Query k v a) where
  hashWithSalt salt query =
    case query of
      GetMap ->
        hashWithSalt salt (0 :: Int)
      Lookup key ->
        hashWithSalt salt (1 :: Int, key)

deriving instance (Eq k, Eq v) => Eq (Query k v a)
deriving instance (Show k, Show v) => Show (Query k v a)

rule ::
  (Hashable k, Monad m)
  => (forall b. Query k v b -> q b)
  -> Query k v a
  -> Task q m (HashMap k v)
  -> Task q m a
rule inject query getMap =
  case query of
    GetMap ->
      getMap
    Lookup key ->
      HashMap.lookup key <$> fetch (inject GetMap)
