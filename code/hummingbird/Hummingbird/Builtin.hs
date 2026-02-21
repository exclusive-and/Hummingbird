module Hummingbird.Builtin where

import Birds.Prelude

import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as Map

import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name

data Builtin
  = Cat
  | Apply
  | Dip
  | Swap
  | Dup
  | Drop
  | K
  | Cake
  | Placeholder
  deriving (Generic, Eq, Ord, Show)

instance Hashable Builtin

instance Pretty Builtin where
  pretty = \case
    Cat -> pretty @Text "cat"
    Apply -> pretty @Text "apply"
    Dip -> pretty @Text "dip"
    Swap -> pretty @Text "swap"
    Dup -> pretty @Text "dup"
    Drop -> pretty @Text "drop"
    K -> pretty @Text "k"
    Cake -> pretty @Text "cake"
    Placeholder -> pretty @Text "placeholder"

builtins :: Map Name Builtin
builtins = Map.fromList [
    (Name.Name "cat", Cat)
  , (Name.Name "apply", Apply)
  , (Name.Name "dip", Dip)
  , (Name.Name "swap", Swap)
  , (Name.Name "dup", Dup)
  , (Name.Name "drop", Drop)
  , (Name.Name "k", K)
  , (Name.Name "cake", Cake)
  , (Name.Name "placeholder", Placeholder)
  ]
