{-# Language DataKinds #-}
{-# Language FunctionalDependencies #-}
{-# Language PolyKinds #-}
{-# Language UndecidableInstances #-}

module Fetch
(
  -- * Fetch answers to questions
  MonadFetch (fetch),

  -- *
  Task (Task, unTask),
  runTask,
  hoistQuery,

  -- ** Inference rules
  Rules,
  GenRules,

  -- ** Task kinds
  TaskKind (Input, NonInput),
  input,
  noError,
  nonInput,
)
where

import Num
import Prelude hiding (
    Num (..),
    Integral (..),
    Fractional (..),
  )

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
import Data.Bifunctor
import Data.Foldable
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.These

newtype Fetch q m = Fetch (forall a. q a -> m a)

-- | Computes an answer @a@.
-- May ask itself @q@-questions to compute the desired answer.
newtype Task q m a = Task {
    unTask :: ReaderT (Fetch q m) m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

-- | Monads that can 'fetch' answers to queries from a query type @q@.
class (Monad m) => MonadFetch q m | m -> q where
  fetch :: q a -> m a
  fetch = lift . fetch

  default fetch ::
    (MonadTrans t, MonadFetch q n, m ~ t n)
    => q a
    -> m a

instance (Monad m) => MonadFetch q (Task q m) where
  fetch key = Task $
    asks (\(Fetch fetch_) -> fetch_ key) >>= lift

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

-- | Inference rules for questions in @q@.
type Rules q = GenRules q q

-- | Inference rules for @p@-questions that can depend on @q@-questions.
type GenRules p q = forall a. p a -> Task q IO a

runTask :: Rules q -> Task q IO a -> IO a
runTask rules (Task task) =
  runReaderT task $ Fetch $ runTask rules . rules

-- |  Hoist @p@-question 'Task's into @q@-question 'Task's via a natural transformation on
-- questions.
hoistQuery ::
  (forall b. p b -> Task q m b)
  -> Task p m a
  -> Task q m a
hoistQuery f (Task task) =
  Task $ ReaderT $ \fetch_ ->
    runReaderT task $ Fetch $ \key ->
      runReaderT (unTask $ f key) fetch_

data Chronicle c f a where
  Chronicle :: f a -> Chronicle c f (These c a)

data TaskKind
  = Input
  | NonInput

input :: (Functor m) => m a -> m ((Either e a, TaskKind))
input = fmap ((, Input) . Right)

noError :: (Functor m) => m a -> m ((Either e a, TaskKind))
noError = fmap ((, NonInput) . Right)

nonInput :: (Functor m) => m (Either e a) -> m ((Either e a, TaskKind))
nonInput = fmap (, NonInput)
