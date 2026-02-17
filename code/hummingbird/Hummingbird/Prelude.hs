module Hummingbird.Prelude
(
  -- * Boot imports
  module Num,

  -- * Hackage imports
  module Generics,
  module Prelude,
  module Prettyprinter,
  module Text,
) where

import Num (
  Integer,
  Int,
  Natural,
  Word,
  Rational,
  Float,
  Double,
  Additive ((+)),
  Subtractive ((-), negate, abs, signum),
  Multiplicative ((*)),
  Ring,
  Distributive,
  Integral (div, mod, quot, rem, divMod, quotRem),
  FromInteger (fromInteger),
  Fractional ((/), recip),
  Field,
  FromRational (fromRational),
  )

import Data.Text as Text (Text)
import GHC.Generics as Generics (Generic)
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
