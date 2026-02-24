module Birds.Prelude
(
  -- * Numbers

  -- ** Rings and integers
  Ring,
  Int,
  Integer,
  FromInteger (fromInteger),
  Word,
  Natural,

  -- ** Addition
  Additive ((+)),
  Sum (Sum, getSum),

  -- ** Subtraction
  Subtractive ((-), negate, abs, signum),

  -- ** Multiplication
  Multiplicative ((*)),
  Product (Product, getProduct),
  -- ** Division
  Fractional ((/), recip),
  -- *** Integer quotient and remainders
  Integral (div, mod, quot, rem, divMod, quotRem),

  -- ** Fields and rationals
  Field,
  Rational,
  FromRational (fromRational),
  Float,
  Double,

  module Data.String,
  module Data.Text,
  module GHC.Generics,
  module Prelude,
  module Prettyprinter,
) where

import Birds.Prelude.Num

import Data.String (IsString (fromString))
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (
  Int,
  Word,
  Integer,
  Natural,
  Num (
    (+),
    (-),
    (*),
    negate,
    abs,
    signum,
    fromInteger),
  Integral (div, mod, quot, rem, divMod, quotRem),
  Float,
  Double,
  Rational,
  Fractional ((/), recip, fromRational),
  String,
  )
import Prettyprinter (Pretty (pretty))
