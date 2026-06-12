module Hummingbird.Codebase.Id where

import Data.Binary
import Data.Hashable
import Data.Kind
import Data.Text qualified as Text
import Data.Typeable
import Prelude
import Prettyprinter

import Hummingbird.Codebase.Hash

data Id = Id !Hash !Int
  deriving
    ( Eq
    , Generic
    , Ord
    , Show
    , Typeable
    )
  deriving anyclass (Binary, Hashable)

instance Pretty Id where
  pretty (Id a x) = pretty a <> "@" <> pretty x
