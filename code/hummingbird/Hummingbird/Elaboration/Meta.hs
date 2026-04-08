module Hummingbird.Elaboration.Meta where

import Data.Binary
import Data.Hashable
import Data.IntMap qualified as IntMap
import Prelude
import Prettyprinter

newtype MetaId = MetaId Int
  deriving stock
    ( Eq
    , Generic
    , Ord
    , Show
    )
  deriving newtype (Binary, Hashable)

instance Pretty MetaId where
  pretty (MetaId metaId) = "Meta" <+> pretty metaId
