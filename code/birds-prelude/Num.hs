module Num
(
  -- * Built-in numeric types
  Integer,
  FromInteger (fromInteger),
  Int,
  Natural,
  Word,
  Rational,
  FromRational (fromRational),
  Float,
  Double,

  -- * Integer arithmetic
  Additive ((+), zero),
  Sum (Sum, getSum),
  Subtractive ((-), negate, abs, signum),
  Multiplicative ((*), one),
  Product (Product, getProduct),
  Integral (div, mod, quot, rem, divMod, quotRem),
  Ring,
  Distributive,

  -- * Fractional arithmetic
  Fractional ((/), recip),
  Field,

  -- * Comparison and ordering
  Bounded (minBound, maxBound),
  Eq ((==), (/=)),
  Ordering (LT, EQ, GT),
  Ord (compare, (<), (<=), (>=), (>), max, min),
) where

import Prelude qualified
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
  )

import Data.Coerce (coerce)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

infixl 6 +, -
infixl 7 *
infixl 7 `div`, `mod`

-- | Internal helper type for deriving arithmetic instances from Prelude.
newtype NumHelper a = NumHelper a

-- | Addition
class Additive a where
  (+) :: a -> a -> a

  -- | Additive identity: does nothing when added to @x@.
  zero :: a

instance (Prelude.Num a) => Additive (NumHelper a) where
  (+) = coerce @(a -> a -> a) (Prelude.+)
  {-# INLINE (+) #-}

  zero = NumHelper 0
  {-# INLINE zero #-}

deriving via (NumHelper Integer ) instance (Additive Integer)
deriving via (NumHelper Int     ) instance (Additive Int)
deriving via (NumHelper Int8    ) instance (Additive Int8)
deriving via (NumHelper Int16   ) instance (Additive Int16)
deriving via (NumHelper Int32   ) instance (Additive Int32)
deriving via (NumHelper Int64   ) instance (Additive Int64)
deriving via (NumHelper Natural ) instance (Additive Natural)
deriving via (NumHelper Word    ) instance (Additive Word)
deriving via (NumHelper Word8   ) instance (Additive Word8)
deriving via (NumHelper Word16  ) instance (Additive Word16)
deriving via (NumHelper Word32  ) instance (Additive Word32)
deriving via (NumHelper Word64  ) instance (Additive Word64)
deriving via (NumHelper Rational) instance (Additive Rational)
deriving via (NumHelper Float   ) instance (Additive Float)
deriving via (NumHelper Double  ) instance (Additive Double)

-- |
newtype Sum a = Sum { getSum :: a }
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance Functor Sum where
  fmap = coerce

instance Applicative Sum where
  pure = Sum
  (<*>) = coerce

instance (Additive a) => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance (Additive a) => Monoid (Sum a) where
  mempty = Sum zero

deriving instance (Additive a) => Additive (Sum a)
deriving instance (Subtractive a) => Subtractive (Sum a)
deriving instance (Multiplicative a) => Multiplicative (Sum a)

-- | Subtraction
class (Additive a) => Subtractive a where
  (-) :: a -> a -> a

  -- | Negation of a number.
  negate :: a -> a

  -- | Absolute value.
  abs :: a -> a

  -- | Representative for the sign of a number.
  -- We expect that:
  --
  -- > abs x * signum x == x
  --
  -- For real numbers, 'signum' should be @-1@ (negative), @0@ (zero), or @+1@ (positive).
  signum :: a -> a

instance (Prelude.Num a) => Subtractive (NumHelper a) where
  (-) = coerce @(a -> a -> a) (Prelude.-)
  {-# INLINE (-) #-}

  negate = coerce @(a -> a) Prelude.negate
  {-# INLINE negate #-}

  abs = coerce @(a -> a) Prelude.abs
  {-# INLINE abs #-}

  signum = coerce @(a -> a) Prelude.signum
  {-# INLINE signum #-}

deriving via (NumHelper Integer ) instance (Subtractive Integer)
deriving via (NumHelper Int     ) instance (Subtractive Int)
deriving via (NumHelper Int8    ) instance (Subtractive Int8)
deriving via (NumHelper Int16   ) instance (Subtractive Int16)
deriving via (NumHelper Int32   ) instance (Subtractive Int32)
deriving via (NumHelper Int64   ) instance (Subtractive Int64)
deriving via (NumHelper Rational) instance (Subtractive Rational)
deriving via (NumHelper Float   ) instance (Subtractive Float)
deriving via (NumHelper Double  ) instance (Subtractive Double)

-- | Multiplication
class Multiplicative a where
  (*) :: a -> a -> a

  -- | Multiplicative identity: does nothing when multiplied by @x@.
  one :: a

instance (Prelude.Num a) => Multiplicative (NumHelper a) where
  (*) = coerce @(a -> a -> a) (Prelude.*)
  {-# INLINE (*) #-}

  one = NumHelper 1
  {-# INLINE one #-}

deriving via (NumHelper Integer ) instance (Multiplicative Integer)
deriving via (NumHelper Int     ) instance (Multiplicative Int)
deriving via (NumHelper Int8    ) instance (Multiplicative Int8)
deriving via (NumHelper Int16   ) instance (Multiplicative Int16)
deriving via (NumHelper Int32   ) instance (Multiplicative Int32)
deriving via (NumHelper Int64   ) instance (Multiplicative Int64)
deriving via (NumHelper Natural ) instance (Multiplicative Natural)
deriving via (NumHelper Word    ) instance (Multiplicative Word)
deriving via (NumHelper Word8   ) instance (Multiplicative Word8)
deriving via (NumHelper Word16  ) instance (Multiplicative Word16)
deriving via (NumHelper Word32  ) instance (Multiplicative Word32)
deriving via (NumHelper Word64  ) instance (Multiplicative Word64)
deriving via (NumHelper Rational) instance (Multiplicative Rational)
deriving via (NumHelper Float   ) instance (Multiplicative Float)
deriving via (NumHelper Double  ) instance (Multiplicative Double)

-- |
newtype Product a = Product { getProduct :: a }
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance Functor Product where
  fmap = coerce

instance Applicative Product where
  pure = Product
  (<*>) = coerce

instance (Multiplicative a) => Semigroup (Product a) where
  Product a <> Product b = Product (a * b)

instance (Multiplicative a) => Monoid (Product a) where
  mempty = Product one

deriving instance (Additive a) => Additive (Product a)
deriving instance (Subtractive a) => Subtractive (Product a)
deriving instance (Multiplicative a) => Multiplicative (Product a)

-- | Integral numbers
class (Additive a, Multiplicative a) => Integral a where
  {-# MINIMAL divMod, quotRem #-}

  -- | Integer division truncated downward.
  div :: a -> a -> a
  div a b = fst (divMod a b)

  -- | Integer modulus.
  mod :: a -> a -> a
  mod a b = snd (divMod a b)

  -- | Combined 'div' and 'mod'.
  divMod :: a -> a -> (a, a)

  -- | Integer quotient truncated toward zero.
  quot :: a -> a -> a
  quot a b = fst (quotRem a b)

  -- | Integer remainder truncated toward zero.
  rem :: a -> a -> a
  rem a b = snd (quotRem a b)

  -- | Combined 'quot' and 'rem'.
  quotRem :: a -> a -> (a, a)

instance (Prelude.Integral a) => Integral (NumHelper a) where
  divMod = coerce @(a -> a -> (a, a)) Prelude.divMod
  {-# INLINE divMod #-}

  quotRem = coerce @(a -> a -> (a, a)) Prelude.quotRem
  {-# INLINE quotRem #-}

deriving via (NumHelper Integer ) instance (Integral Integer)
deriving via (NumHelper Int     ) instance (Integral Int)
deriving via (NumHelper Int8    ) instance (Integral Int8)
deriving via (NumHelper Int16   ) instance (Integral Int16)
deriving via (NumHelper Int32   ) instance (Integral Int32)
deriving via (NumHelper Int64   ) instance (Integral Int64)
deriving via (NumHelper Natural ) instance (Integral Natural)
deriving via (NumHelper Word    ) instance (Integral Word)
deriving via (NumHelper Word8   ) instance (Integral Word8)
deriving via (NumHelper Word16  ) instance (Integral Word16)
deriving via (NumHelper Word32  ) instance (Integral Word32)
deriving via (NumHelper Word64  ) instance (Integral Word64)

-- |
type Distributive a = (Additive a, Multiplicative a)

-- | Rings (addition, subtraction, and multiplication).
type Ring a = (Additive a, Subtractive a, Multiplicative a)

-- | Convert integer literals into other number types.
class FromInteger a where
  fromInteger :: Integer -> a

instance (Prelude.Num a) => FromInteger (NumHelper a) where
  fromInteger = coerce @(Integer -> a) Prelude.fromInteger
  {-# INLINE fromInteger #-}

deriving via (NumHelper Integer ) instance (FromInteger Integer)
deriving via (NumHelper Int     ) instance (FromInteger Int)
deriving via (NumHelper Int8    ) instance (FromInteger Int8)
deriving via (NumHelper Int16   ) instance (FromInteger Int16)
deriving via (NumHelper Int32   ) instance (FromInteger Int32)
deriving via (NumHelper Int64   ) instance (FromInteger Int64)
deriving via (NumHelper Natural ) instance (FromInteger Natural)
deriving via (NumHelper Word    ) instance (FromInteger Word)
deriving via (NumHelper Word8   ) instance (FromInteger Word8)
deriving via (NumHelper Word16  ) instance (FromInteger Word16)
deriving via (NumHelper Word32  ) instance (FromInteger Word32)
deriving via (NumHelper Word64  ) instance (FromInteger Word64)
deriving via (NumHelper Rational) instance (FromInteger Rational)
deriving via (NumHelper Float   ) instance (FromInteger Float)
deriving via (NumHelper Double  ) instance (FromInteger Double)

-- | Division
class (Multiplicative a) => Fractional a where
  (/) :: a -> a -> a

  -- | The reciprocal of a number. Also called its multiplicative inverse. Equal to @1 / x@.
  recip :: a -> a

instance (Prelude.Fractional a) => Fractional (NumHelper a) where
  (/) = coerce @(a -> a -> a) (Prelude./)
  {-# INLINE (/) #-}

  recip = coerce @(a -> a) Prelude.recip
  {-# INLINE recip #-}

deriving via (NumHelper Rational) instance (Fractional Rational)
deriving via (NumHelper Float   ) instance (Fractional Float)
deriving via (NumHelper Double  ) instance (Fractional Double)

-- | Fields (addition, subtraction, multiplication, and division/reciprocals).
type Field a = (Ring a, Fractional a)

-- | Convert rational literals into other number types.
class FromRational a where
  fromRational :: Rational -> a

instance (Prelude.Fractional a) => FromRational (NumHelper a) where
  fromRational = coerce @(Rational -> a) Prelude.fromRational
  {-# INLINE fromRational #-}

deriving via (NumHelper Rational) instance (FromRational Rational)
deriving via (NumHelper Float   ) instance (FromRational Float)
deriving via (NumHelper Double  ) instance (FromRational Double)
