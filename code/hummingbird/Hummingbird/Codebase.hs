module Hummingbird.Codebase
(
  Hash,
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
import Hummingbird.Codebase.Hash (Hash)
import Hummingbird.Codebase.Patch (CodePatch (..), Stage (..))
