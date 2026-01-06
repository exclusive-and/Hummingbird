{-# Language OverloadedStrings #-}

module Hummingbird.Surface where

import Num
import Text qualified

import Prettyprinter (Pretty (pretty))
import Prettyprinter qualified as Pretty

import Hummingbird.Literal (Literal)
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Prelude

data Module word = Module {
    name :: !Name.Module,
    decls :: [Declaration word]
  }

instance (Pretty word) => Pretty (Module word) where
  pretty (Module name decls) =
    Pretty.vsep [
        Pretty.hsep [pretty name, "module"]
      , Pretty.vsep $ map pretty decls
      ]

data Declaration word
  = Fun word (Term word)
  | Sig word (Type word)

instance (Pretty word) => Pretty (Declaration word) where
  pretty = \case
    Fun name body -> Pretty.hsep [pretty name, "=", pretty body]
    Sig name ty -> Pretty.hsep [pretty name, ":", pretty ty]

data Term word
  = Lit Literal
  | Word word
  | Lambda word (Term word)
  | Match [Alt word]
  | Quoted (Term word)
  | Concat [Term word]
  deriving (Show)

instance Semigroup (Term word) where
  (<>) a b = mconcat [a, b]

instance Monoid (Term word) where
  mconcat = Concat

instance (Pretty word) => Pretty (Term word) where
  pretty = \case
    Lit literal -> pretty literal
    Word word -> pretty word
    Lambda bndr body ->
      Pretty.hsep [
          "\\"
        , pretty bndr
        , "->"
        , pretty body
        ]
    Match alts ->
      Pretty.hang 2 $
      Pretty.vcat ("match" : map pretty alts)
    Quoted quoted -> Pretty.brackets $ pretty quoted
    Concat terms -> Pretty.hsep $ map pretty terms

data Alt word
  = LitAlt Literal (Term word)
  | ConsAlt word (Term word)
  deriving (Show)

instance (Pretty word) => Pretty (Alt word) where
  pretty = \case
    LitAlt pat rhs ->
      Pretty.hsep [pretty pat, "->", pretty rhs]
    ConsAlt pat rhs ->
      Pretty.hsep [pretty pat, "->", pretty rhs]

data Type word

instance (Pretty word) => Pretty (Type word) where
  pretty = pretty
