module Hummingbird.Fetch.Writer
(
  Writer (Writer),
  writer,  
)
where

import Num
import Text qualified

import Hummingbird.Fetch (fetch)
import Hummingbird.Fetch qualified as Fetch
import Hummingbird.Prelude

-- |
data Writer w f a where
  Writer :: f a -> Writer w f (a , w)

-- |
writer ::
  (forall a. p a -> w -> Fetch.Task q ())
  -> Fetch.GenRules (Writer w p) q
  -> Fetch.GenRules p q
writer write rules key = do
  (result, w) <- rules $ Writer key
  write key w
  pure result
