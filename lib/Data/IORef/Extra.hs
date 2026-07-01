module Data.IORef.Extra where

import Data.IORef
import GHC.IO
import Prelude

-- | Variant of 'atomicModifyIORef' that doesn't return anything.
atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f =
  atomicModifyIORef ref (\x -> (f x, ()))

