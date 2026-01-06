module Fetch.Writer
(
  Writer (Writer),
  writer,  
)
where

import Num
import Prelude hiding (
    Num (..),
    Integral (..),
    Fractional (..),
  )

import Fetch

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
