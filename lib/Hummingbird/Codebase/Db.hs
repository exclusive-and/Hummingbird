module Hummingbird.Codebase.Db where

import Control.Concurrent qualified
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Primitive
import Data.IORef
import Data.IORef.Extra (atomicModifyIORef_)
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Justified qualified as Justified
import Data.Primitive.MutVar
import Data.Primitive.MutVar.Extra (atomicModifyMutVar_)
import Data.Set (Set)
import Data.Set qualified as Set
import Prelude
import Prettyprinter

import Hummingbird.Codebase.Hash
import Hummingbird.Codebase.Id
import Hummingbird.Codebase.Patch
import Hummingbird.Error
import Hummingbird.Name as Name
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var (Var)

-- |
data Codebase =
  Codebase
    { cdbEnv      :: MutVar RealWorld CodebaseEnv
    , cdbModules  :: MutVar RealWorld (Map Name.Module (Surface.Module Var, Map Name Var))
    }

-- |
init :: (MonadPrim RealWorld m) => m Codebase
init = do
  cdbEnv <- newMutVar emptyEnv
  cdbModules <- newMutVar Map.empty
  pure Codebase
    { cdbEnv
    , cdbModules
    }

lookupName ::
  (MonadPrim RealWorld m)
  => Codebase
  -> Name
  -> m (Maybe (Hash, Surface.Term Hash))
lookupName Codebase{cdbEnv} name = do
  CodebaseEnv
    { cdbNameEnv
    , cdbTermEnv
    } <- readMutVar cdbEnv
  pure do
    hash <- Map.lookup name cdbNameEnv
    term <- Map.lookup hash cdbTermEnv
    Just (hash, term)

getNameEnv :: (MonadPrim RealWorld m) => Codebase -> m (Map Name Hash)
getNameEnv Codebase{cdbEnv} =
  cdbNameEnv <$> readMutVar cdbEnv

insertTerms ::
  (MonadPrim RealWorld m)
  => Codebase
  -> Map Name Hash
  -> Map Hash (Surface.Term Hash)
  -> m ()
insertTerms Codebase{cdbEnv} names terms =
  atomicModifyMutVar_ cdbEnv (insertTermsEnv names terms)

applyChecked ::
  (MonadPrim RealWorld m)
  => Codebase
  -> CodePatch Typechecked
  -> m ()
applyChecked cdb (AddCheckedTerms _ names ok terms _) =
  insertTerms
    cdb
    (Map.filter (`Set.member` ok) names)
    (Map.restrictKeys terms ok)

-- |
lookupModule ::
  (MonadPrim RealWorld m)
  => Name.Module
  -> Codebase
  -> m (Maybe (Surface.Module Var, Map Name Var))
lookupModule name Codebase{cdbModules} =
  Map.lookup name <$> readMutVar cdbModules

-- |
addModule ::
  (MonadPrim RealWorld m)
  => Name.Module
  -> Surface.Module Var
  -> Map Name Var
  -> Codebase
  -> m ()
addModule name mod rnMap Codebase{cdbModules} =
  atomicModifyMutVar_ cdbModules (Map.insert name (mod, rnMap))

data CodebaseEnv =
  CodebaseEnv
    { cdbNameEnv :: Map Name Hash
    , cdbTermEnv :: Map Hash (Surface.Term Hash)
    }

emptyEnv :: CodebaseEnv
emptyEnv =
  CodebaseEnv
    { cdbNameEnv = Map.empty
    , cdbTermEnv = Map.empty
    }

insertTermsEnv ::
  Map Name Hash
  -> Map Hash (Surface.Term Hash)
  -> CodebaseEnv
  -> CodebaseEnv
insertTermsEnv names terms env =
  env
    { cdbNameEnv = cdbNameEnv env <> names
    , cdbTermEnv = cdbTermEnv env <> terms
    }
