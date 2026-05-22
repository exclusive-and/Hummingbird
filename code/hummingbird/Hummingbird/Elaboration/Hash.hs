module Hummingbird.Elaboration.Hash where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Chronicle
import Control.Monad.State
import Control.Monad.Trans
import Crypto.Hash
import Crypto.Hash.Generic
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Semigroup
import Data.Set (Set)
import Data.Set qualified as Set
import Data.These
import Data.Traversable
import Prelude
import Prettyprinter

import Hummingbird.Builtin as Builtin
import Hummingbird.Codebase as Codebase
import Hummingbird.Codebase.Hash qualified as Codebase
import Hummingbird.Error as Error
import Hummingbird.Fetch as Fetch
import Hummingbird.Name as Name
import Hummingbird.Query (Query)
import Hummingbird.Surface as Surface

renameDeclTask ::
  Map Name Hash
  -> Surface.Declaration Name
  -> Task Query IO (CodePatch Renamed)

renameDeclTask env (Surface.Fun name expr) =
  undefined
renameDeclTask env (Surface.Sig{}) =
  pure $ AddHashedTerms [] Map.empty Set.empty Map.empty Map.empty

renameExprTask ::
  Map Name Hash
  -> Surface.Term Name
  -> Task Query IO (CodePatch Renamed)

renameExprTask env expr = do
  case snd $ runHashingM (renameTerm expr) env of
    This errors -> do
      pure $ AddHashedTerms errors Map.empty Set.empty Map.empty Map.empty
    That renamed -> do
      let hashed = hashTerm renamed
      pure $ AddHashedTerms
        []
        Map.empty
        (Set.singleton hashed)
        (Map.singleton hashed renamed)
        Map.empty
    These errors renamed -> do
      let hashed = hashTerm renamed
      pure $ AddHashedTerms
        errors
        Map.empty
        (Set.singleton hashed)
        (Map.singleton hashed renamed)
        Map.empty

hashTerm :: Surface.Term Hash -> Hash
hashTerm = Codebase.Hash . Crypto.Hash.Generic.hashWith SHA3_512

newtype HashingM a = HashingM (ChronicleT [Error] (State (Map Name Hash)) a)
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadChronicle [Error]
    , MonadState (Map Name Hash)
    )

runHashingM :: HashingM a -> Map Name Hash -> (Map Name Hash, These [Error] a)
runHashingM (HashingM m) = swap . runState (runChronicleT m)

renameTerm :: Surface.Term Name -> HashingM (Surface.Term Hash)
renameTerm (Surface.Word name) = do
  inScope <- get
  case Map.lookup name inScope of
    Just hash -> pure (Word hash)
    Nothing ->
      case Map.lookup name builtins of
        Just builtin  -> pure (Prim builtin)
        Nothing       -> confess [Elaboration $ NotInScope name]
renameTerm (Surface.Lit lit) =
  pure (Surface.Lit lit)
renameTerm (Surface.Lambda bndr body) =
  error "Hummingbird.Elaboration.Hash.renameTerm: no support for lambdas yet"
renameTerm (Surface.Match alts) =
  Surface.Match <$> mapM renameAlt alts
renameTerm (Surface.Quoted quoted) =
  Surface.Quoted <$> renameTerm quoted
renameTerm (Surface.Concat terms) =
  Surface.Concat <$> mapM renameTerm terms

renameAlt :: Surface.Alt Name -> HashingM (Surface.Alt Hash)
renameAlt = error "Hummingbird.Elaboration.Hash.renameAlt: not yet implemented"

