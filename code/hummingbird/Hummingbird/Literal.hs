module Hummingbird.Literal where

import Birds.Prelude

import Data.Char
import Prettyprinter qualified as Pretty

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
