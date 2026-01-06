module Fetch.Chronicle where

import Num
import Prelude hiding (
    Num (..),
    Integral (..),
    Fractional (..),
  )

import Control.Monad.Chronicle (ChronicleT)
import Control.Monad.Chronicle qualified
import Control.Monad.Trans
import Data.These

import Fetch (fetch, Task, GenRules)
import Fetch qualified

data Chronicle c f a where
  Chronicle :: f a -> Chronicle c f (These c a)

chronicle ::
  (Semigroup c)
  => (forall a. p a -> These c a -> Task q IO a)
  -> GenRules (Chronicle c p) q
  -> GenRules p q
chronicle chron_ rules key = do
  rules (Chronicle key) >>= chron_ key
  