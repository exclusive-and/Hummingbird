module Hummingbird.Elaboration.Monad where

import Control.Monad
import Prelude

import Fetch (Task)

import Hummingbird.Query (Query)

newtype M a = M (Task Query IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)
