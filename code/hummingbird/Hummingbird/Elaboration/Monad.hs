module Hummingbird.Elaboration.Monad where

import Control.Monad
import Prelude

import Hummingbird.Fetch as Fetch
import Hummingbird.Query (Query)

newtype M a = M (Task Query IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)
