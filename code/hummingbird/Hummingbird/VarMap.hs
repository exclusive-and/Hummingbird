module Hummingbird.VarMap
(
  VarMap,
  empty,
  insert,
  insertWith,
  lookup,
  fromList,
  toList,
) where

import Prelude hiding (
    empty,
    lookup,
    toList,
  )

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Map (Map)
import Data.Map qualified as Map

import Hummingbird.Var

-- |
newtype VarMap a = VarMap {
    unVarMap :: Map Var a
  }
  deriving (Eq, Ord, Show)
  deriving stock (Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid)

-- |
empty :: VarMap a
empty = VarMap Map.empty

-- |
fromList :: [(Var, a)] -> VarMap a
fromList = VarMap . Map.fromList

-- |
toList :: VarMap a -> [(Var, a)]
toList = Map.toList . unVarMap

-- |
insert :: Var -> a -> VarMap a -> VarMap a
insert var a =VarMap . Map.insert var a . unVarMap

-- |
insertWith :: (a -> a -> a) -> Var -> a -> VarMap a -> VarMap a
insertWith f var a = VarMap . Map.insertWith f var a . unVarMap

-- |
lookup :: Var -> VarMap a -> Maybe a
lookup var = Map.lookup var . unVarMap
