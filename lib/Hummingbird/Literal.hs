module Hummingbird.Literal where

import Data.Binary
import Data.Char
import Data.Hashable
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude
import Prettyprinter

-- |
data Literal
  = Int !Integer
  | Char !Char
  | String !Text
  deriving (Eq, Generic, Show)

instance Binary Literal
instance Hashable Literal

instance FromInteger Literal where
  fromInteger = Int

instance IsString Literal where
  fromString = String . fromString @Text

instance Pretty Literal where
  pretty = \case
    Int int -> pretty int
    Char char -> pretty char
    String string -> pretty string
