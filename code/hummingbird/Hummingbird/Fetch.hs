{-# Language PolyKinds #-}
{-# Language UndecidableInstances #-}

module Hummingbird.Fetch where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Chronicle
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.RWS.Lazy qualified
import Control.Monad.RWS.Strict qualified
import Control.Monad.State.Lazy qualified
import Control.Monad.State.Strict qualified
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Lazy qualified
import Control.Monad.Writer.Strict qualified
import Data.Bifunctor
import Data.Constraint.Extras
import Data.Dependent.HashMap (DHashMap)
import Data.Dependent.HashMap qualified as DHashMap
import Data.Dependent.Sum
import Data.Foldable
import Data.Functor.Const
import Data.GADT.Compare
import Data.Hashable
import Data.IORef
import Data.IORef.Extra (atomicModifyIORef_)
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Some
import Data.Typeable
import Prelude
import Prettyprinter
import Prettyprinter.Render.Text

import Crypto.Hash (SHA3_512)
import Crypto.Hash qualified

-- | Inference rules for questions in @q@.
type Rules m q = GenRules m q q

-- | Inference rules for @p@-questions that can depend on @q@-questions.
newtype GenRules m p q =
  GenRules (forall (a :: Type). p a -> Task q m a)

-- | Computes an answer @a@. May ask itself @q@-questions to compute the answer.
newtype Task q m a = Task {
    unTask :: ReaderT (Fetch q m) m a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadFail
    , MonadFix
    , MonadIO
    , MonadMask
    , MonadThrow
    )

deriving newtype instance (MonadChronicle c m) => MonadChronicle c (Task q m)

newtype Fetch q m = Fetch (forall a. q a -> m a)

runTask :: Rules m q -> Task q m a -> m a
runTask (GenRules rules) (Task task) =
  runReaderT task $ Fetch $ runTask (GenRules rules) . rules

-- | Monads that can 'fetch' answers to queries from a query type @q@.
class (Monad m) => MonadFetch q m | m -> q where
  fetch :: q a -> m a
  fetch = lift . fetch

  default fetch ::
    (MonadTrans t, MonadFetch q n, m ~ t n)
    => q a
    -> m a

instance (Monad m) => MonadFetch q (Task q m) where
  fetch key =
    Task $ asks (\(Fetch fetch_) -> fetch_ key) >>= lift

instance (MonadFetch q m, Semigroup c) => MonadFetch q (ChronicleT c m)
instance (MonadFetch q m) => MonadFetch q (ContT r m)
instance (MonadFetch q m) => MonadFetch q (ExceptT e m)
instance (MonadFetch q m) => MonadFetch q (IdentityT m)
instance (MonadFetch q m) => MonadFetch q (MaybeT m)
instance (MonadFetch q m) => MonadFetch q (ReaderT r m)

instance
  (MonadFetch q m, Monoid w)
  => MonadFetch q (Control.Monad.RWS.Lazy.RWST r w s m)

instance
  (MonadFetch q m, Monoid w)
  => MonadFetch q (Control.Monad.RWS.Strict.RWST r w s m)

instance
  (MonadFetch q m)
  => MonadFetch q (Control.Monad.State.Lazy.StateT s m)

instance
  (MonadFetch q m)
  => MonadFetch q (Control.Monad.State.Strict.StateT s m)

instance
  (MonadFetch q m, Monoid w)
  => MonadFetch q (Control.Monad.Writer.Lazy.WriterT w m)

instance
  (MonadFetch q m, Monoid w)
  => MonadFetch q (Control.Monad.Writer.Strict.WriterT w m)

memoise ::
  forall f g m.
  (GEq f, Hashable (Some f), MonadIO m)
  => IORef (DHashMap f MVar)
  -> GenRules m f g
  -> GenRules m f g
memoise memoCtxVar (GenRules rules) =
  GenRules \(key :: f a) ->
    do
    memoCtx <- liftIO $ readIORef memoCtxVar
    case DHashMap.lookup key memoCtx of
      Just memoVar -> liftIO do
        readMVar memoVar
      Nothing -> join $ liftIO do
        memoVar <- newEmptyMVar
        atomicModifyIORef memoCtxVar \memoCtx ->
          case DHashMap.alterLookup (Just . fromMaybe memoVar) key memoCtx of
            (Just memoVar', _) -> (memoCtx, liftIO $ readMVar memoVar')
            (Nothing, memoCtx') ->
              ( memoCtx'
              , do
                value <- rules key
                liftIO $ putMVar memoVar value
                pure value
              )

-- | Hoist @p@-question 'Task's into @q@-question 'Task's via a natural transformation on
-- questions.
hoistQuery ::
  (forall b. p b -> Task q m b)
  -> Task p m a
  -> Task q m a
hoistQuery f (Task task) =
  Task $ ReaderT \fetch_ ->
    runReaderT task $ Fetch \key ->
      runReaderT (unTask $ f key) fetch_

-- |
data Writer w f a where
  Writer :: f a -> Writer w f (a , w)

-- |
writer ::
  (Monad m)
  => (forall a. p a -> w -> Task q m ())
  -> GenRules m (Writer w p) q
  -> GenRules m p q
writer write (GenRules rules) =
  GenRules \key -> do
    (result, w) <- rules $ Writer key
    write key w
    pure result
