module Data.Primitive.MutVar.Extra where

import Prelude

import Control.Monad.Primitive
import Data.Primitive.MutVar

-- | Variant of 'atomicModifyMutVar' that doesn't return anything.
atomicModifyMutVar_ :: (MonadPrim s m) => MutVar s a -> (a -> a) -> m ()
atomicModifyMutVar_ mutvar f =
  atomicModifyMutVar mutvar (\x -> (f x, ()))
