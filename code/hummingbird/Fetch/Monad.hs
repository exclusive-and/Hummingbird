{-# Language PolyKinds #-}
{-# Language UndecidableInstances #-}

module Fetch.Monad where

import Prelude

import Control.Monad
import Control.Monad.Chronicle (
    ChronicleT,
    MonadChronicle (..),
  )
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.RWS.Lazy qualified as Lazy
import Control.Monad.RWS.Strict qualified as Strict
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.State.Strict qualified as Strict
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Lazy qualified as Lazy
import Control.Monad.Writer.Strict qualified as Strict

-- | Monads that can 'fetch' answers to queries from a query type @q@.
class (Monad m) => MonadFetch q m | m -> q where
  fetch :: q a -> m a
  fetch = lift . fetch

  default fetch ::
    (MonadTrans t, MonadFetch q n, m ~ t n)
    => q a
    -> m a

instance (MonadFetch q m, Semigroup c) => MonadFetch q (ChronicleT c m)
instance (MonadFetch q m) => MonadFetch q (ContT r m)
instance (MonadFetch q m) => MonadFetch q (ExceptT e m)
instance (MonadFetch q m) => MonadFetch q (IdentityT m)
instance (MonadFetch q m) => MonadFetch q (MaybeT m)
instance (MonadFetch q m) => MonadFetch q (ReaderT r m)
instance (MonadFetch q m, Monoid w) => MonadFetch q (Strict.RWST r w s m)
instance (MonadFetch q m, Monoid w) => MonadFetch q (Lazy.RWST r w s m)
instance (MonadFetch q m) => MonadFetch q (Strict.StateT s m)
instance (MonadFetch q m) => MonadFetch q (Lazy.StateT s m)
instance (MonadFetch q m, Monoid w) => MonadFetch q (Strict.WriterT w m)
instance (MonadFetch q m, Monoid w) => MonadFetch q (Lazy.WriterT w m)
