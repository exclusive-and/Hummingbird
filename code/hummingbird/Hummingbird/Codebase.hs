module Hummingbird.Codebase
(
  Hash,
  ContentAddress
    ( contentHash
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
import Hummingbird.Codebase.Patch
