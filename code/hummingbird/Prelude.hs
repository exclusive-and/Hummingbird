module Prelude
(
  -- * Getting started

  -- ** Numbers
  module Prelude.Num,

  -- ** Logic
  module Data.Bool,
  module Data.Eq,
  module Data.Ord,

  -- ** Text
  module Data.Char,
  module Data.String,
  module Data.Text,
  module Prettyprinter,
  module Text.Read,
  module Text.Show,

  -- ** Lists
  module Data.List,
  
  -- ** Maybe
  module Data.Maybe,

  -- ** Either
  module Data.Either,

  -- ** I/O
  module GHC.IO,
  module System.IO,

  -- ** Function combinators
  module Data.Function,
  module Data.Tuple,

  -- * Monoids and semigroups
  module Data.Monoid,
  module Data.Semigroup,

  -- * Functors and monads

  -- ** Functors
  module Control.Applicative,
  module Data.Bifunctor,
  module Data.Functor,
  module Data.Functor.Const,
  module Data.Functor.Identity,

  -- ** Folds and traversals
  module Data.Bifoldable,
  module Data.Bitraversable,
  module Data.Foldable,
  module Data.Traversable,

  -- ** Monads
  module Control.Monad,
  module Control.Monad.Fix,

  -- *** Advanced I/O
  module Control.Monad.IO.Class,

  -- * Advanced type system nuts and bolts
  module Data.Type.Equality,
  module GHC.Err,
  module GHC.Generics,
) where

import Prelude.Num

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Bool (
    Bool (..)
  , not
  , (||)
  , (&&)
  , otherwise
  )
import Data.Char (Char (..))
import Data.Either (Either (..))
import Data.Eq (Eq (..))
import Data.Foldable hiding (sum, product)
import Data.Function
import Data.Functor
import Data.Functor.Const
import Data.Functor.Identity
import Data.Kind
import Data.List (
    (++)
  , drop
  , filter
  , map
  , reverse
  , span
  , take
  , unzip3
  , zip
  , zip3
  , zipWith
  , zipWith3
  )
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..))
import Data.Ord (
    Ord (..)
  , Ordering (..)
  )
import Data.Semigroup (Semigroup (..))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Traversable
import Data.Tuple
import Data.Type.Equality (type (~))
import GHC.Err (
    error
  , undefined
  )
import GHC.Generics (Generic)
import GHC.IO (IO)
import Prettyprinter (Pretty (pretty))
import System.IO (
    getChar
  , getLine
  , getContents
  , print
  , FilePath
  , readFile
  , writeFile
  , appendFile
  )
import Text.Read (
    Read (..)
  , read
  , reads
  )
import Text.Show (Show (..), shows)
