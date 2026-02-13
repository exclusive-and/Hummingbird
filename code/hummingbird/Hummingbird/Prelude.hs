module Hummingbird.Prelude
(
  -- * Boot imports
  module Num,
  module Prelude,

  -- * Hackage imports
  module Data.Text,
  module GHC.Generics,
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
    Subtractive ((-), negate),
    Multiplicative ((*)),
    FromInteger (fromInteger),
    Integral (div, mod, divMod, quot, rem, quotRem),
    Fractional ((/), recip),
  )

import Prelude hiding (
    Num (..),
    Integral (..),
    Fractional (..),
    String,
  )

import Data.Text (Text)
import GHC.Generics (Generic)
