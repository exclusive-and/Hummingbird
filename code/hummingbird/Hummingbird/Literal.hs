module Hummingbird.Literal where

import Data.Char
import Prettyprinter qualified as Pretty

import Hummingbird.Prelude

-- |
data Literal
  = Int !Integer
  | Char !Char
  | String !Text
  deriving (Show)

instance Pretty Literal where
  pretty = \case
    Int int -> pretty int
    Char char -> pretty char
    String string -> pretty string
