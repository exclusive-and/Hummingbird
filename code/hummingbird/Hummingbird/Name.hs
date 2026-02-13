{-# Language OverloadedStrings #-}

module Hummingbird.Name where

import Data.Hashable
import Data.String (IsString (fromString))
import Data.Text qualified as Text

import Hummingbird.Prelude

newtype Module = Module Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, IsString, Pretty)

newtype Name = Name Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, IsString, Pretty)

newtype Constructor = Constructor Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, IsString, Pretty)

data Qualified = Qualified {
    moduleName :: !Module,
    unqualName :: !Name
  }
  deriving (Generic, Eq, Ord, Show)

instance IsString Qualified where
  fromString s = let
    t = fromString @Text s
    (modDot, name) = Text.breakOnEnd "." t
    in
      case Text.stripSuffix "." modDot of
        Nothing ->
          Qualified (Module mempty) (Name t)
        Just moduleName ->
          Qualified (Module moduleName) (Name name)

instance Pretty Qualified where
  pretty (Qualified (Module moduleName) name) =
    if Text.null moduleName then
      pretty name
    else
      pretty moduleName <> "." <> pretty name
