module Hummingbird.Elaboration where

import Data.Binary
import Data.Map (Map)
import Data.Map qualified as Map
import Prelude
import Prettyprinter

import Crypto.Hash (SHA3_512 (..))
import Crypto.Hash qualified
import Crypto.Hash.Generic qualified

import Hummingbird.Builtin (Builtin (..))
import Hummingbird.Elaboration.Context
import Hummingbird.Elaboration.Meta
import Hummingbird.Elaboration.Mode
import Hummingbird.Elaboration.Monad
import Hummingbird.Error qualified as Error
import Hummingbird.Literal (Literal)
import Hummingbird.Literal qualified as Literal
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Query (Query)
import Hummingbird.Query qualified as Query
import Hummingbird.Surface qualified as Surface
import Hummingbird.Surface.Located (Located)
import Hummingbird.Surface.Located qualified as Located

-- | Get the cryptographic SHA3-512 hash of a term.
hashTerm :: Surface.Term Name -> Crypto.Hash.Digest SHA3_512
hashTerm =
  Crypto.Hash.Generic.hashWith SHA3_512

data ElabInsn
  = CheckTerms [ElabTerm] [ElabType] [ElabType]

data ElabCtxt
  = TermCtxt !Located.Span !(Surface.Term Name)

-- |
elaborate ::
  Context
  -> Mode mode
  -> Surface.Term Name
  -> M (mode (Surface.Type Name))
elaborate context mode term0 = go term0
  where
    go = go

checkCompose ::
  Context
  -> [Surface.Term Name]
  -> [ElabType]
  -> [ElabType]
  -> M (ElabTerm, [ElabType], [ElabType])
checkCompose context terms0 expectedArgs expectedResults = undefined
