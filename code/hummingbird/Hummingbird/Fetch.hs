{-# Language PolyKinds #-}
{-# Language UndecidableInstances #-}

module Hummingbird.Fetch where

import Prelude

import Control.Monad
import Control.Monad.Accum
import Control.Monad.Catch
import Control.Monad.Chronicle
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.RWS.Strict qualified
import Control.Monad.State
import Control.Monad.State.Strict qualified
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Writer (WriterT)
import Control.Monad.Writer.Strict qualified
import Data.Dependent.HashMap (DHashMap)
import Data.Dependent.HashMap qualified as DHashMap
import Data.GADT.Compare
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Primitive.MutVar
import Data.Primitive.MVar as MVar
import Data.Some

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
instance (MonadFetch q m, Monoid w) => MonadFetch q (RWST r w s m)
instance (MonadFetch q m) => MonadFetch q (StateT s m)
instance (MonadFetch q m, Monoid w) => MonadFetch q (WriterT w m)

instance
  (MonadFetch q m, Monoid w)
  => MonadFetch q (Control.Monad.RWS.Strict.RWST r w s m)

instance
  (MonadFetch q m)
  => MonadFetch q (Control.Monad.State.Strict.StateT s m)

instance
  (MonadFetch q m, Monoid w)
  => MonadFetch q (Control.Monad.Writer.Strict.WriterT w m)

newtype Fetch q m = Fetch (forall a. q a -> m a)

-- | Computes an answer @a@. May ask itself @q@-questions to compute the answer.
newtype Task q m a = Task {
    unTask :: ReaderT (Fetch q m) m a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadCont
    , MonadFail
    , MonadFix
    , MonadIO
    , MonadMask
    , MonadThrow
    , PrimMonad
    )

instance (Monad m) => MonadFetch q (Task q m) where
  fetch key = Task $
    asks (\(Fetch fetch_) -> fetch_ key) >>= lift

instance MonadTrans (Task q) where
  lift = Task . lift

instance (MonadReader r m) => MonadReader r (Task q m) where
  ask = lift ask
  local f (Task t) = Task $ mapReaderT (local f) t

deriving newtype instance (MonadAccum w m) => MonadAccum w (Task q m)
deriving newtype instance (MonadChronicle c m) => MonadChronicle c (Task q m)
deriving newtype instance (MonadError e m) => MonadError e (Task q m)
deriving newtype instance (MonadRWS r w s m) => MonadRWS r w s (Task q m)
deriving newtype instance (MonadState s m) => MonadState s (Task q m)
deriving newtype instance (MonadWriter w m) => MonadWriter w (Task q m)

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

-- | Inference rules for questions in @q@.
type Rules m q = GenRules m q q

-- | Inference rules for @p@-questions that can depend on @q@-questions.
newtype GenRules m p q = GenRules (forall a. p a -> Task q m a)

runTask :: Rules m q -> Task q m a -> m a
runTask (GenRules rules) (Task task) =
  runReaderT task $ Fetch $ runTask (GenRules rules) . rules

memoise ::
  (GEq p, Hashable (Some p))
  => (MonadPrim s m)
  => MutVar s (DHashMap p (MVar s))
  -> GenRules m p q
  -> GenRules m p q

memoise memoCtxVar (GenRules rules) =
  GenRules \key ->
    do
    memoCtx <- readMutVar memoCtxVar
    case key `DHashMap.lookup` memoCtx of
      Just memoVar -> readMVar memoVar
      Nothing -> join do
        freshMemoVar <- newEmptyMVar
        let
          launch = do
            value <- rules key
            putMVar freshMemoVar value
            pure value
        let
          recall Nothing =
            (launch, Just freshMemoVar)
          recall (Just memoVar) =
            (readMVar memoVar, Just memoVar)
        atomicModifyMutVar memoCtxVar $ swap . DHashMap.alterF recall key
