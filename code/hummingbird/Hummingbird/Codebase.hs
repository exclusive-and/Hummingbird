module Hummingbird.Codebase where

import Data.Binary
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Justified qualified as Justified
import Prelude
import Prettyprinter

import Crypto.Hash (SHA3_512 (..))
import Crypto.Hash qualified
import Crypto.Hash.Generic qualified

import Hummingbird.Elaboration.Rename (RnMap)
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var (Var)

-- |
data Codebase = Codebase {
    cdbModules :: IORef (Map Name.Module (Surface.Module Var, RnMap))
  }

-- |
init :: IO Codebase
init = do
  cdbModules <- newIORef Map.empty
  pure Codebase{..}

-- |
lookupModule ::
  Name.Module
  -> Codebase
  -> IO (Maybe (Surface.Module Var, RnMap))
lookupModule name Codebase{cdbModules} =
  Map.lookup name <$> readIORef cdbModules

-- |
addModule ::
  Name.Module
  -> Surface.Module Var
  -> RnMap
  -> Codebase
  -> IO ()
addModule name mod rnMap Codebase{cdbModules} =
  atomicModifyIORef cdbModules
    (\modMap -> (Map.insert name (mod, rnMap) modMap, ()))
