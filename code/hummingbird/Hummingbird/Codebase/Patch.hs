module Hummingbird.Codebase.Patch where

import Data.GADT.Compare
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Justified qualified as Justified
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import Prelude
import Prettyprinter

import Hummingbird.Codebase.Hash
import Hummingbird.Codebase.Id
import Hummingbird.Error
import Hummingbird.Name as Name
import Hummingbird.Surface qualified as Surface

type data Stage
  = Parsed
  | Renamed
  | Typechecked

data CodePatch (a :: Stage) where
  -- | Add some parsed declarations to the codebase.
  AddDecls ::
    [Error]
    -> [Surface.Declaration Name]
    -> CodePatch Parsed
  -- | Add some parsed expressions to the codebase.
  AddExprs ::
    [Error]
    -> [Surface.Term Name]
    -> CodePatch Parsed
  -- | Add (content-addressed) terms to the codebase.
  AddHashedTerms ::
    [Error]
    -> Map Name Hash
    -> Set Hash
    -> Map Hash (Surface.Term Hash)
    -> Map Hash (Surface.Type Hash)
    -> CodePatch Renamed
  -- | Add some fully typechecked terms to the codebase.
  AddCheckedTerms ::
    [Error]
    -> Map Name Hash
    -> Set Hash
    -> Map Hash (Surface.Term Hash)
    -> Map Hash (Surface.Type Hash)
    -> CodePatch Typechecked
  -- |
  {-
  FillHoles ::
    [Error]
    -> Map HoleId Hash
    -> Set Hash
    -> Map Hash (Surface.Term Hash)
    -> Map Hash (Surface.Type Hash)
    -> CodePatch Stages.Typechecked
    -}

instance Eq (CodePatch a) where
  (==) = defaultEq

instance GEq CodePatch where
  AddDecls errs1 decls1
    `geq`
    AddDecls errs2 decls2
    | errs1 == errs2
    , decls1 == decls2 = Just Refl
  
  AddExprs errs1 exprs1
    `geq`
    AddExprs errs2 exprs2
    | errs1 == errs2
    , exprs1 == exprs2 = Just Refl
  
  AddHashedTerms errs1 names1 ok1 terms1 types1
    `geq`
    AddHashedTerms errs2 names2 ok2 terms2 types2
    | errs1 == errs2
    , names1 == names2
    , ok1 == ok2
    , terms1 == terms2
    , types1 == types2 = Just Refl
  
  AddCheckedTerms errs1 names1 ok1 terms1 types1
    `geq`
    AddCheckedTerms errs2 names2 ok2 terms2 types2
    | errs1 == errs2
    , names1 == names2
    , ok1 == ok2
    , terms1 == terms2
    , types1 == types2 = Just Refl
  
  _ `geq` _ = Nothing

instance Pretty (CodePatch a) where
  pretty = \case
    AddDecls errors decls ->
      "Add the following declarations:"
        <> line
        <> vcat (map (\a -> "-" <+> pretty a) decls)
        <> ppErrors errors
        & hang 4
    AddExprs errors exprs ->
      "Add the following expressions:"
        <> line
        <> vcat (map (\a -> "-" <+> pretty a) exprs)
        <> ppErrors errors
        & hang 4
    AddHashedTerms errors names ok terms types ->
      "Add the following terms:"
        <> line
        <> vcat (map ("-" <+>) $ ppPatchTerms ok terms types)
        <> ppErrors errors
        & hang 4
    AddCheckedTerms errors names ok terms types ->
      "Add the following typechecked terms:"
        <> line
        <> vcat (map ("-" <+>) $ ppPatchTerms ok terms types)
        <> ppErrors errors
        & hang 4
    where
      ppErrors :: [Error] -> Doc ann
      ppErrors [] = ""
      ppErrors errors =
        "While constructing this patch, I ran into the following errors:"
          <> line
          <> vcat (map (\a -> "-" <+> pretty a) errors)
          & hang 4
          & (line <>)

deriving instance Show (CodePatch a)
deriving instance Typeable (CodePatch a)

data PatchTerms =
  PatchTerms
    (Map Name Hash)
    (Set Hash)
    (Map Hash (Surface.Term Hash))
    (Map Hash (Surface.Type Hash))
  deriving stock (Eq, Show)

ppPatchTerms ::
  (Ord a, Pretty a)
  => Set a
  -> Map a (Surface.Term a)
  -> Map a (Surface.Type a)
  -> [Doc ann]
ppPatchTerms ok terms types =
  Map.unionWith (\a b -> align $ vcat [a, b])
    (Map.mapWithKey (\k tm -> pretty k <+> "=" <+> pretty tm) terms)
    (Map.mapWithKey (\k ty -> pretty k <+> "::" <+> pretty ty) types)
    & flip Map.restrictKeys ok
    & Map.elems
