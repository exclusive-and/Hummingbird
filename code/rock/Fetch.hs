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

  -- * Query transformers

  -- ** Writer
  Writer (Writer),
  writer,

  -- ** Except
  Except (Except),
  handle,
) where

import Prelude

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Bifunctor
import Data.Foldable
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.These

import Fetch.Monad

newtype Fetch q m = Fetch (forall a. q a -> m a)

-- | Computes an answer @a@.
-- May ask itself @q@-questions to compute the desired answer.
newtype Task q m a = Task {
    unTask :: ReaderT (Fetch q m) m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance (Monad m) => MonadFetch q (Task q m) where
  fetch key = Task $
    asks (\(Fetch fetch_) -> fetch_ key) >>= lift

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

-- |
data Writer w f a where
  Writer :: f a -> Writer w f (a , w)

-- |
writer ::
  (forall a. p a -> w -> Task q IO ())
  -> GenRules (Writer w p) q
  -> GenRules p q
writer write rules key = do
  (result, w) <- rules $ Writer key
  write key w
  pure result

-- |
data Except e f a where
  Except :: f a -> Except e f (Either e a)

-- |
handle ::
  (forall a. p a -> e -> Task q IO a)
  -> GenRules (Except e p) q
  -> GenRules p q
handle handler rules key = do
  result <- rules $ Except key
  case result of
    Left err -> handler key err
    Right okay -> pure okay
