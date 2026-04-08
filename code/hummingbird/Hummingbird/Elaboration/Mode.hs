module Hummingbird.Elaboration.Mode where

import Data.Binary
import Prelude
import Prettyprinter

import Hummingbird.Error qualified as Error
import Hummingbird.Elaboration.Meta
import Hummingbird.Elaboration.Monad
import Hummingbird.Name (Name)
import Hummingbird.Surface qualified as Surface

newtype Checked x = Checked x

data Inferred x = Inferred x (Surface.Type Name)

-- |
data Mode mode where
  Check :: Surface.Type Name -> Mode Checked
  Infer :: Mode Inferred

