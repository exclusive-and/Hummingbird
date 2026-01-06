{-# Language DeriveTraversable #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Hummingbird.Var
(
  -- * Variables
  Var (Prim, Var),

  -- * Surface names
  Name,
  InScope,

  -- * Variable lookup
  VarMap,
  empty,
  insert,
  insertWith,
  lookup,
  fromList,
  toList,
)
where

import Num
import Text qualified

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Map (Map)
import Data.Map qualified as Map
import Prettyprinter (Pretty (pretty))
import Prettyprinter qualified as Pretty

import Hummingbird.Builtin
import Hummingbird.Name (Name)
import Hummingbird.Prelude hiding (empty, lookup, toList)

-- |

data Var
  = Var Name Int
  | Prim Builtin
  deriving (Show)

instance Pretty Var where
  pretty = \case
    Var name i ->
      pretty name <> pretty '_' <> pretty i
    Prim builtin ->
      pretty builtin <> pretty '#'

instance Eq Var where
  Var _ i == Var _ j = i == j
  Prim  x == Prim  y = x == y
  _       == _       = False

instance Ord Var where
  compare (Prim  x) (Prim  y) = compare x y
  compare (Prim  _) (Var _ _) = LT
  compare (Var _ _) (Prim  _) = GT
  compare (Var _ i) (Var _ j) = compare i j

-- |
type InScope = Map Name Var

-- |
newtype VarMap a = VarMap
  {
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
