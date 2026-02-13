{-# Language DeriveAnyClass #-}

module Hummingbird.Var where

import Data.Hashable (Hashable)
import Prettyprinter (Pretty (pretty))
import Prettyprinter qualified as Pretty

import Hummingbird.Builtin (Builtin)
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Prelude

-- |
data Var
  = Var Name Int
  | Prim Builtin
  deriving (Generic, Show)

instance Hashable Var

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
