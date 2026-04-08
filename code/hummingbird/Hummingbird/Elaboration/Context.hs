{-# Language OverloadedRecordDot #-}

module Hummingbird.Elaboration.Context where

import Data.Binary
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Prelude
import Prettyprinter

import Crypto.Hash (SHA3_512 (..))
import Crypto.Hash qualified
import Crypto.Hash.Generic qualified

import Hummingbird.Builtin (Builtin)
import Hummingbird.Elaboration.Meta
import Hummingbird.Elaboration.Monad
import Hummingbird.Literal (Literal)
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name

type Hashed = Crypto.Hash.Digest SHA3_512

data Context = Context {
    metas :: IORef (IntMap ElabType)
  , freshMetaId :: IORef Int
  , typeEnv :: IORef (Map Hashed ElabType)
  }

newMeta :: Context -> M MetaId
newMeta context = liftIO $
  atomicModifyIORef context.freshMetaId (\x -> (x + 1, MetaId x))

lookupMeta :: Context -> MetaId -> M ElabType
lookupMeta context (MetaId metaId) = do
    metas <- liftIO $ readIORef context.metas
    case IntMap.lookup metaId metas of
      Just elabType -> pure elabType
      Nothing       -> pure (Meta $ MetaId metaId)

data ElabTerm
  = Lit Literal
  | Compose [ElabTerm]
  | Prim Builtin
  | Word Name
  | Quote ElabTerm
  deriving (Binary, Generic, Show)

data ElabType
  = Meta MetaId
  | TyCon Name.Constructor
  | TyVar Name
  deriving (Binary, Generic, Show)
