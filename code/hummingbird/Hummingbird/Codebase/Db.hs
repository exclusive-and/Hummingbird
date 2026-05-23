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
data Codebase = Codebase {
    cdbNameMap :: IORef (Map Name Hash)
  , cdbTermMap :: IORef (Map Hash (Surface.Term Hash))
  , cdbModules :: IORef (Map Name.Module (Surface.Module Var, Map Name Var))
  }

-- |
init :: (MonadIO m) => m Codebase
init =
  liftIO do
    cdbNameMap <- newIORef Map.empty
    cdbTermMap <- newIORef Map.empty
    cdbModules <- newIORef Map.empty
    pure Codebase{..}

getNameMap :: (MonadIO m) => Codebase -> m (Map Name Hash)
getNameMap Codebase{cdbNameMap} =
  liftIO do
    readIORef cdbNameMap

insertTerms ::
  (MonadIO m)
  => Codebase
  -> Map Name Hash
  -> Map Hash (Surface.Term Hash)
  -> m ()
insertTerms Codebase{..} names terms =
  liftIO do
    atomicModifyIORef_ cdbNameMap (<> names)
    atomicModifyIORef_ cdbTermMap (<> terms)

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
