module Hummingbird.Prelude
(
  -- * Boot imports
  module Num,
  module Prelude,
  module Text,

  -- * Hackage imports
  module GHC.Generics,
)
where

-- boot imports
import Num ()
import Prelude hiding (
    Num (..),
    Integral (..),
    Fractional (..),
    String,
  )
import Text (Text)

-- hackage imports
import GHC.Generics (Generic)
