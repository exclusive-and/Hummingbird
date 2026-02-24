module Birds.Prelude
(
  module Birds.Prelude.Num,
  module Data.String,
  module Data.Text,
  module GHC.Generics,
  module Prelude,
  module Prettyprinter,
) where

import Birds.Prelude.Num (
  Integer,
  Int,
  Natural,
  Word,
  FromInteger (fromInteger),
  Rational,
  Float,
  Double,
  FromRational (fromRational),
  Additive ((+)),
  Sum (Sum, getSum),
  Subtractive ((-), negate, abs, signum),
  Multiplicative ((*)),
  Product (Product, getProduct),
  Integral (div, mod, quot, rem, divMod, quotRem),
  Ring,
  Distributive,
  Fractional ((/), recip),
  Field,
  )

import Data.String (IsString (fromString))
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (
  Num (
    (+),
    (-),
    (*),
    negate,
    abs,
    signum,
    fromInteger),
  Integral (div, mod, quot, rem, divMod, quotRem),
  Fractional ((/), recip, fromRational),
  String,
  )
import Prettyprinter (Pretty (pretty))
