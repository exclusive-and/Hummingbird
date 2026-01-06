module Fetch.Except where

import Num
import Prelude hiding (
    Num (..),
    Integral (..),
    Fractional (..),
  )

import Fetch (fetch, Task, GenRules)

data Except e f a where
  Except :: f a -> Except e f (Either e a)

handle ::
  (forall a. p a -> e -> Task q IO a)
  -> GenRules (Except e p) q
  -> GenRules p q
handle handler rules key = do
  result <- rules $ Except key
  case result of
    Left err -> handler key err
    Right okay -> pure okay
