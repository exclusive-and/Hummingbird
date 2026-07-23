module Hummingbird.Var where

import Prelude
import Prettyprinter

import Data.ContentAddress
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Hummingbird.Builtin as Builtin
import Hummingbird.Codebase.Hash
import Hummingbird.Codebase.Id
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name

data VarInfo
  = NoInfo
  | Hole
  | KnownPrim !Builtin
  | Local !Int
  | Hashed !Id
  deriving
    ( Eq
    , Generic
    , Ord
    , Show
    )

-- |
data Var = Var
  { name  :: !Name
  , uniq  :: !Int
  , info  :: !VarInfo
  }
  deriving (Show)

instance Eq Var where
  (==) = (==) `on` uniq

instance Ord Var where
  compare = compare `on` uniq

instance Hashable Var where
  hash = uniq
  hashWithSalt = defaultHashWithSalt

instance Pretty Var where
  pretty Var{..} =
    case info of
      NoInfo          -> pretty name <> brackets (pretty uniq)
      Hole            -> "hole:" <> pretty uniq
      KnownPrim prim  -> "built-in:" <> pretty prim
      Local x         -> "stable:" <> pretty x
      Hashed hash     -> "hash:" <> pretty name <> brackets (pretty hash)

type InScope = Set Var
type FreeVars = Set Var

mkVarNoInfo :: Int -> Name -> Var
mkVarNoInfo uniq name = Var name uniq NoInfo

mkVarHashed :: Int -> Name -> Id -> Var
mkVarHashed uniq name hash = Var name uniq (Hashed hash)

mkVarBuiltin :: Int -> Name -> Builtin -> Var
mkVarBuiltin uniq name prim = Var name uniq (KnownPrim prim)

builtinVars :: [Var]
builtinVars =
  [ mkVarBuiltin 1 (Name.Name "cat") Cat
  , mkVarBuiltin 2 (Name.Name "apply") Apply
  , mkVarBuiltin 3 (Name.Name "dip") Dip
  , mkVarBuiltin 4 (Name.Name "swap") Swap
  , mkVarBuiltin 5 (Name.Name "dup") Dup
  , mkVarBuiltin 6 (Name.Name "drop") Drop
  , mkVarBuiltin 7 (Name.Name "k") K
  , mkVarBuiltin 8 (Name.Name "cake") Cake
  , mkVarBuiltin 0 (Name.Name "placeholder") Placeholder
  ]

isBuiltin :: Var -> Bool
isBuiltin Var{info} = case info of
  KnownPrim _ -> True
  _           -> False
