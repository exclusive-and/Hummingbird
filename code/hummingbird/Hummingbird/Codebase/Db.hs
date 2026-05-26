module Hummingbird.Codebase.Db where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Data.Binary
import Data.ByteString (ByteString)
import Data.IORef
import Data.IORef.Extra (atomicModifyIORef_)
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Justified qualified as Justified
import Data.Set (Set)
import Data.Set qualified as Set
import Prelude
import Prettyprinter

import Hummingbird.Codebase.Hash as Codebase
import Hummingbird.Codebase.Patch as Codebase
import Hummingbird.Error as Error
import Hummingbird.Name as Name
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var (Var)

-- |
data Codebase =
  Codebase
    { cdbEnv      :: IORef CodebaseEnv
    , cdbModules  :: IORef (Map Name.Module (Surface.Module Var, Map Name Var))
    }

-- |
init :: IO Codebase
init = do
  cdbEnv <- newIORef emptyEnv
  cdbModules <- newIORef Map.empty
  pure Codebase
    { cdbEnv
    , cdbModules
    }

lookupName ::
  Codebase
  -> Name
  -> IO (Maybe (Hash, Surface.Term Hash))
lookupName Codebase{cdbEnv} name = do
  CodebaseEnv
    { cdbNameEnv
    , cdbTermEnv
    } <- readIORef cdbEnv
  pure do
    hash <- Map.lookup name cdbNameEnv
    term <- Map.lookup hash cdbTermEnv
    Just (hash, term)

getNameEnv :: Codebase -> IO (Map Name Hash)
getNameEnv Codebase{cdbEnv} =
  cdbNameEnv <$> readIORef cdbEnv

insertTerms ::
  Codebase
  -> Map Name Hash
  -> Map Hash (Surface.Term Hash)
  -> IO ()
insertTerms Codebase{cdbEnv} names terms =
  atomicModifyIORef_ cdbEnv (insertTermsEnv names terms)

applyChecked :: Codebase -> CodePatch Typechecked -> IO ()
applyChecked cdb (AddCheckedTerms _ names ok terms _) =
  insertTerms
    cdb
    (Map.filter (`Set.member` ok) names)
    (Map.restrictKeys terms ok)

-- |
lookupModule ::
  (MonadIO m)
  => Name.Module
  -> Codebase
  -> m (Maybe (Surface.Module Var, Map Name Var))
lookupModule name Codebase{cdbModules} =
  liftIO do
    Map.lookup name <$> readIORef cdbModules

-- |
addModule ::
  (MonadIO m)
  => Name.Module
  -> Surface.Module Var
  -> Map Name Var
  -> Codebase
  -> m ()
addModule name mod rnMap Codebase{cdbModules} =
  liftIO do
    atomicModifyIORef_ cdbModules (Map.insert name (mod, rnMap))

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
