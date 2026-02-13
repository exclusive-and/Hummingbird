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
  Subtractive ((-), negate),
  Multiplicative ((*)),
  FromInteger (fromInteger),
  Integral (div, mod, divMod, quot, rem, quotRem),
  Fractional ((/), recip),
  )

import Data.Text as Text (Text)
import GHC.Generics as Generics (Generic)
import Prelude hiding (
  Num (..),
  Integral (..),
  Fractional (..),
  String,
  )
import Prettyprinter (Pretty (pretty))
