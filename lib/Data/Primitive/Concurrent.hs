module Data.Primitive.Concurrent
(
  -- * The real world
  RealWorld,

  -- * Concurrency primitives
  Control.Concurrent.ThreadId,
  getThreadId,
) where

import Prelude

import Control.Concurrent qualified
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Primitive

-- | Variant of 'Control.Concurrent.myThreadId'.
getThreadId :: (MonadPrim RealWorld m) => m Control.Concurrent.ThreadId
getThreadId = 
  ioToPrim Control.Concurrent.myThreadId

{-# INLINEABLE getThreadId #-}
{-# SPECIALISE getThreadId :: IO Control.Concurrent.ThreadId #-}
