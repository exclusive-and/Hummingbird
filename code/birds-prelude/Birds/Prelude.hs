module Birds.Prelude
(
  -- * Boot imports
  module Birds.Num,

  -- * Hackage imports
  module Data.String,
  module Data.Text,
  module GHC.Generics,
  module Prelude,
  module Prettyprinter,
) where

import Num as Birds.Num (
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
  Subtractive ((-), negate, abs, signum),
  Multiplicative ((*)),
  Ring,
  Distributive,
  Integral (div, mod, quot, rem, divMod, quotRem),
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
