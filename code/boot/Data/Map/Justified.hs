module Data.Map.Justified
(
  Map,
  Key,
  member,
  lookup,
  withMap,
) where

import Data.Map qualified
import GHC.Generics
import Prelude hiding (lookup)

newtype Map s k v = Map {
    getOrdMap :: Data.Map.Map k v
  }
  deriving (Generic, Eq, Ord, Show)
  deriving newtype (
      Monoid
    , Semigroup
    , Functor
    , Foldable
    )
  deriving (Traversable)

type role Map phantom nominal representational

newtype Key s k = Key {
    getOrdKey :: k
  }
  deriving (Generic, Eq, Ord, Show)

type role Key phantom representational

member :: (Ord k) => k -> Map s k v -> Maybe (Key s k)
member k (Map ordMap) =
  Key k <$ Data.Map.lookup k ordMap

lookup :: (Ord k) => Key s k -> Map s k v -> v
lookup (Key k) (Map ordMap) =
  case Data.Map.lookup k ordMap of
    Just v  -> v
    Nothing -> error "Data.Map.Justified has been subverted!"

withMap :: Data.Map.Map k v -> (Map s k v -> r) -> r
withMap ordMap cont = cont (Map ordMap)
