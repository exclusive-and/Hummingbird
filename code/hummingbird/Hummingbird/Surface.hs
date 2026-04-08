module Hummingbird.Surface where

import Data.Binary
import Data.Hashable
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude
import Prettyprinter

import Hummingbird.Literal (Literal)
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name

data Module word = Module {
    name :: !Name.Module,
    decls :: [Declaration word]
  }

data Declaration word
  = Fun word (Term word)
  | Sig word (Type word)

data Term word
  = Lit Literal
  | Word word
  | Lambda word (Term word)
  | Match [Alt word]
  | Quoted (Term word)
  | Concat [Term word]
  deriving (Eq, Generic, Show)

instance Monoid (Term word) where
  mconcat = Concat

instance Semigroup (Term word) where
  (<>) a b = mconcat [a, b]

data Alt word
  = LitAlt Literal (Term word)
  | ConsAlt word (Term word)
  deriving (Eq, Generic, Show)

data Type word
  deriving (Eq, Generic, Show)

instance (Binary word) => Binary (Term word)
instance (Binary word) => Binary (Alt word)
instance (Binary word) => Binary (Type word)

instance (Hashable word) => Hashable (Term word)
instance (Hashable word) => Hashable (Alt word)
instance (Hashable word) => Hashable (Type word)

instance FromInteger (Term word) where
  fromInteger = Lit . fromInteger @Literal

instance IsString (Term word) where
  fromString = Lit . fromString @Literal

instance (Pretty word) => Pretty (Term word) where
  pretty = \case
    Lit literal -> pretty literal
    Word word -> pretty word
    Lambda bndr body ->
      hsep [
          "\\"
        , pretty bndr
        , "->"
        , pretty body
        ]
    Match alts ->
      hang 2 $ vcat ("match" : map pretty alts)
    Quoted quoted -> brackets $ pretty quoted
    Concat terms -> hsep $ map pretty terms

instance (Pretty word) => Pretty (Alt word) where
  pretty = \case
    LitAlt pat rhs ->
      hsep [pretty pat, "->", pretty rhs]
    ConsAlt pat rhs ->
      hsep [pretty pat, "->", pretty rhs]

instance (Pretty word) => Pretty (Type word) where
  pretty = pretty

instance (Pretty word) => Pretty (Declaration word) where
  pretty = \case
    Fun name body ->
      pretty name <+> "=" <+> pretty body
    Sig name ty ->
      pretty name <+> ":" <+> pretty ty

instance (Pretty word) => Pretty (Module word) where
  pretty (Module name decls) =
    vsep [
        pretty name <+> "module"
      , vsep $ map pretty decls
      ]
