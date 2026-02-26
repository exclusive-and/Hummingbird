module Birds.Prelude
(
  -- * Numbers

  -- ** Integers
  Ring,
  Int,
  Integer,
  FromInteger (fromInteger),
  Distributive,
  Word,
  Natural,

  -- ** Rational numbers
  Field,
  Rational,
  FromRational (fromRational),
  Float,
  Double,

  -- ** Addition
  Additive ((+)),
  sum,
  Sum (Sum, getSum),
  
  -- ** Subtraction
  Subtractive
    ( (-)
    , negate
    , abs
    , signum ),
  
  -- ** Multiplication
  Multiplicative ((*)),
  product,
  Product (Product, getProduct),

  -- ** Division
  Fractional ((/), recip),
  
  -- *** Integer quotient and remainders
  Integral
    ( div
    , mod
    , divMod
    , quot
    , rem
    , quotRem ),

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
  Integer,
  Word,
  Natural,
  Rational,
  Float,
  Double,
  Num (..),
  sum,
  product,
  Integral (..),
  Fractional (..),
  String,
  )
import Prettyprinter (Pretty (pretty))
