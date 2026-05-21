module Hummingbird.Codebase
(
  Codebase,
  CodePatch
    ( AddDecls
    , AddExprs
    , AddHashedTerms
    , AddCheckedTerms
    ),
  Hash,
  KStage
    ( Parsed
    , Hashed
    , Typechecked
    ),
) where

import Hummingbird.Codebase.Db (Codebase)
import Hummingbird.Codebase.Hash (Hash)
import Hummingbird.Codebase.Patch (CodePatch (..))
import Hummingbird.Codebase.Stages (KStage (..))
