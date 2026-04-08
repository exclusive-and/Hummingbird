module Hummingbird.Tacit
(
  Term (..),
  Alt (LitAlt, ConsAlt),
) where

import Prelude

import Prettyprinter qualified as Pretty

import Hummingbird.Literal (Literal)
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name

data Term word
  = Lit Literal
  | Word word
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
