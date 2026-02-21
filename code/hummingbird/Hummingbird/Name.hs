module Hummingbird.Name where

import Data.Hashable
import Data.Text qualified as Text

import Hummingbird.Prelude

data Module
  = Repl
  | Module Text
  deriving (Generic, Eq, Ord, Show)

instance Hashable Module

instance IsString Module where
  fromString = Module . fromString @Text

instance Pretty Module where
  pretty = \case
    Repl -> "[REPL]"
    Module moduleName -> pretty moduleName

newtype Name = Name Text
  deriving stock (Generic, Eq, Ord, Show)
  deriving newtype (Pretty)

instance Hashable Name

deriving newtype instance IsString Name

newtype Constructor = Constructor Text
  deriving stock (Generic, Eq, Ord, Show)
  deriving newtype (Pretty)

instance Hashable Constructor

deriving newtype instance IsString Constructor

data Qualified = Qualified {
    moduleName :: !Module,
    unqualName :: !Name
  }
  deriving (Generic, Eq, Ord, Show)

instance Hashable Qualified

instance IsString Qualified where
  fromString str =
    let
      text = fromString @Text str
      (modDot, name) = Text.breakOnEnd "." text
    in
      case Text.stripSuffix "." modDot of
        Nothing ->
          Qualified (Module mempty) (Name name)
        Just moduleName ->
          Qualified (Module moduleName) (Name name)

instance Pretty Qualified where
  pretty = \case
    Qualified Repl name ->
      pretty Repl <> "." <> pretty name
    Qualified (Module moduleName) name ->
      if Text.null moduleName then
        pretty name
      else
        pretty moduleName <> "." <> pretty name
