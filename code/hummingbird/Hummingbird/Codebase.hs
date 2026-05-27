module Hummingbird.Codebase
(
  Hash,
  CryptoHashable
    ( cryptoHash
    ),
  Codebase,
  CodePatch
    ( AddDecls
    , AddExprs
    , AddHashedTerms
    , AddCheckedTerms
    ),
  Stage
    ( Parsed
    , Renamed
    , Typechecked
    ),
) where

import Hummingbird.Codebase.Db (Codebase)
import Hummingbird.Codebase.Hash
  ( Hash
  , CryptoHashable (..)
  )
import Hummingbird.Codebase.Patch (CodePatch (..), Stage (..))
