module Hummingbird.Name where

import Data.Binary
import Data.Hashable
import Data.String
import Data.Text qualified as Text
import Prelude
import Prettyprinter

-- |
newtype Constructor = Constructor Name
  deriving stock
    ( Eq
    , Generic
    , Ord
    , Show
    )
  deriving newtype (Binary, Hashable, Pretty)

-- |
data Module = Repl | Module Text
  deriving stock
    ( Eq
    , Generic
    , Ord
    , Show
    )
  deriving anyclass (Hashable)

-- |
newtype Name = Name Text
  deriving stock
    ( Eq
    , Generic
    , Ord
    , Show
    )
  deriving newtype (Binary, Hashable, Pretty)

-- |
data Qualified = Qualified {
    moduleName :: !Module
  , unqualName :: !Name
  }
  deriving stock
    ( Eq
    , Generic
    , Ord
    , Show
    )
  deriving anyclass (Hashable)

-- |
newtype Surface = Surface Text
  deriving stock
    ( Eq
    , Generic
    , Ord
    , Show
    )
  deriving newtype (Hashable, Pretty)

instance Pretty Module where
  pretty = \case
    Repl -> "[REPL]"
    Module moduleName -> pretty moduleName

instance Pretty Qualified where
  pretty = \case
    Qualified Repl name ->
      pretty Repl <> "." <> pretty name
    Qualified (Module moduleName) name ->
      if Text.null moduleName then
        pretty name
      else
        pretty moduleName <> "." <> pretty name

deriving newtype instance IsString Constructor
deriving newtype instance IsString Name
deriving newtype instance IsString Surface

instance IsString Module where
  fromString = Module . fromString @Text

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
