module Data.Graph.Internal where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import Data.Map (Map)
import Data.Map qualified as Map
import Prelude

-- | Identify a value with an 'Int'. Identifications made in the same context
--   are unique up to equality.
--
-- Whenever I identify a value I haven't seen before, I inform my caller of that fact by
-- 'tell'ing @(the value, its identification)@.
identify :: (Ord a) => a -> Identify a Int
identify x = do
  (freshId, idMap) <- get
  case Map.lookup x idMap of
    Just oldId -> pure oldId
    Nothing -> do
      tell [(x, freshId)]
      lift $ put (freshId + 1, Map.insert x freshId idMap)
      pure freshId

-- | The context monad for 'identify'.
type Identify a = WriterT [(a, Int)] (State (Int, Map a Int))

-- | Transitively identify values and their successors.
identifyBfs :: forall a. (Ord a) => (a -> [a]) -> [a] -> Map Int (a, [Int])
identifyBfs f start =
  let
    visit :: (a, Int) -> Identify a (Map Int (a, [Int]))
    visit (x, v) = do
      v  <- identify x
      ws <- traverse identify (f x)
      pure $ Map.singleton v (x, ws)
    
    initial = execWriterT $ traverse identify start
  in
    evalState (initial >>= bfsM visit) (0, mempty)

-- | Breadth-first work strategy.
--
-- This strategy performs the current worklist through to completion in one sweep. At the same
-- time, it collects the work to do on the next pass.
--
-- The strategy finishes when the next worklist is empty.

bfsM :: (Monoid a, Monad m) => (k -> WriterT [k] m a) -> [k] -> m a
bfsM f = go mempty
  where
    go acc [] = pure acc
    go acc xs = do
      (acc', ys) <- runWriterT (mconcat <$> mapM f xs)
      go (acc <> acc') ys
