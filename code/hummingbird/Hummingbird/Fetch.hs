{-# language UndecidableInstances #-}

module Hummingbird.Fetch
(
  -- * Fetch answers to questions
  Fetch (Fetch),

  -- ** Inference rules
  Rules,
  GenRules,

  -- ** Fetching from a knowledge-base
  MonadFetch (fetch),
  Task (Task, unTask),
  TaskKind (Input, NonInput),
  runTask,
)
where

import Num
import Text qualified

import Control.Monad
import Control.Monad.Chronicle
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

import Hummingbird.Prelude

newtype Fetch q = Fetch (forall a. q a -> IO a)

-- |  Inference rules for questions in @q@.
type Rules q = GenRules q q

-- |  Inference rules for @p@-questions that can depend on @q@-questions.
type GenRules p q = forall a. p a -> Task q a

-- |  Monads that can 'fetch' the answer to @q@-questions.
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
instance (MonadFetch q m) => MonadFetch q (Strict.StateT s m)
instance (MonadFetch q m) => MonadFetch q (Lazy.StateT s m)

instance (MonadFetch q m, Monoid w) => MonadFetch q (Strict.RWST r w s m)
instance (MonadFetch q m, Monoid w) => MonadFetch q (Lazy.RWST r w s m)
instance (MonadFetch q m, Monoid w) => MonadFetch q (Strict.WriterT w m)
instance (MonadFetch q m, Monoid w) => MonadFetch q (Lazy.WriterT w m)

-- |  Computes an answer @a@.
-- May ask itself @q@-questions to compute the desired answer.
newtype Task q a = Task {
    unTask :: ReaderT (Fetch q) IO a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFix
    )

data TaskKind
  = Input
  | NonInput

runTask :: Rules q -> Task q a -> IO a
runTask rules (Task task) =
  runReaderT task $ Fetch $ runTask rules . rules

instance MonadFetch q (Task q) where
  fetch key = Task $ do
    io <- asks (\(Fetch fetch_) -> fetch_ key)
    liftIO io

-- |  Hoist @p@-question 'Task's into @q@-question 'Task's via a natural transformation on
-- questions.
hoistQuery ::
  (forall b. p b -> Task q b)
  -> Task p a
  -> Task q a
hoistQuery f (Task task) =
  Task $ ReaderT $ \fetch_ ->
    runReaderT task $ Fetch $ \key ->
      runReaderT (unTask $ f key) fetch_
