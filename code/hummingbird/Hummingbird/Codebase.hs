{- HLINT ignore "Use newtype instead of data" -}
{-# Language NamedFieldPuns #-}
{-# Language RecordWildCards #-}

module Hummingbird.Codebase where

import Data.Map (Map)
import Data.Map qualified as Map

import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Prelude
import Hummingbird.Rename (RnMap)
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var (Var)

-- |
data Codebase cdb = Codebase {
    cdbModules :: Map Name.Module (Surface.Module Var, RnMap)
  }

-- |
empty :: Codebase cdb
empty = Codebase {
    cdbModules = Map.empty
  }

-- |
lookupModule ::
  Name.Module
  -> Codebase cdb
  -> Maybe (Surface.Module Var, RnMap)
lookupModule name Codebase{cdbModules} = Map.lookup name cdbModules

-- |
insertModule ::
  Name.Module
  -> Surface.Module Var
  -> RnMap
  -> Codebase cdb
  -> Codebase cdb'
insertModule name mod rnMap codebase =
  codebase {
    cdbModules = Map.insert name (mod, rnMap) (cdbModules codebase)
  }

-- |
clone :: Codebase cdb -> Codebase cdb'
clone Codebase{..} = Codebase{..}
