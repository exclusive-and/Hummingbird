module Data.Primitive.MVar.Extra where

import Prelude

import Control.Monad.Catch
import Control.Monad.Primitive
import Data.Primitive.MVar

modifyMVar_ :: (MonadMask m, MonadPrim s m) => MVar s a -> (a -> m a) -> m ()
modifyMVar_ var f =
  mask \restore -> do
    takeMVar var
      >>= \a -> restore (f a) `onException` putMVar var a
      >>= putMVar var

modifyMVar :: (MonadMask m, MonadPrim s m) => MVar s a -> (a -> m (a, b)) -> m b
modifyMVar var f =
  mask \restore -> do
    takeMVar var
      >>= \a -> restore (f a >>= evalPrim) `onException` putMVar var a
      >>= \(a', b) -> putMVar var a' >> pure b
