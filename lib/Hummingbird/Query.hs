module Hummingbird.Query where

import Control.Concurrent
import Control.Monad
import Control.Monad.Primitive
import Data.Dependent.HashMap (DHashMap)
import Data.GADT.Compare
import Data.GADT.Show
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.Primitive.MutVar (MutVar)
import Data.Some
import Data.Text (Text)
import Data.Text.Rope (Rope)
import Data.Typeable
import Prelude
import Prettyprinter

import Hummingbird.Codebase as Codebase
import Hummingbird.Codebase.Id
import Hummingbird.Elaboration.Var (Var)
import Hummingbird.Error
import Hummingbird.Fetch
import Hummingbird.Name as Name
import Hummingbird.Surface
  ( Declaration
  , Term
  , Type
  )
import Hummingbird.Surface qualified as Surface

data Query answer where
  -- | Initalize a new codebase.
  InitCodebase :: Query Codebase
  -- |
  LookupName :: Name -> Query (Maybe (Hash, Term Var))
  -- |
  RenameExpr :: Term Name -> Query (CodePatch Renamed)
  -- |
  RenameDecls :: [Declaration Name] -> Query (CodePatch Renamed)
  -- | Ingest a parsed surface-language declaration and try to intern
  -- its changes into the codebase.
  IngestDecl ::
    !Name.Module
    -> Declaration Name
    -> Query (Maybe [Error])
  -- | What is the raw text of this source file?
  FileText :: !FilePath -> Query Text
  -- | What is the rope representation of this source file?
  FileRope :: !FilePath -> Query Rope
  -- |
  ParsedFile :: !FilePath -> Query (Either [Error] (Surface.Module Name))
  -- | Ingest a source file and try to intern its changes into the codebase.
  IngestFile :: !FilePath -> Query (Maybe [Error])
  -- | Ingest a directory recursively.
  IngestDirectory :: !FilePath -> Query (Maybe [Error])

instance Eq (Query a) where
  (==) = defaultEq

instance GEq Query where
  InitCodebase        `geq` InitCodebase                  = Just Refl
  LookupName x        `geq` LookupName y        | x == y  = Just Refl
  RenameExpr x        `geq` RenameExpr y        | x == y  = Just Refl
  RenameDecls x       `geq` RenameDecls y       | x == y  = Just Refl
  IngestDecl nm1 decl1 `geq` IngestDecl nm2 decl2
    | nm1 == nm2, decl1 == decl2                          = Just Refl
  FileText x          `geq` FileText y          | x == y  = Just Refl
  FileRope x          `geq` FileRope y          | x == y  = Just Refl
  ParsedFile x        `geq` ParsedFile y        | x == y  = Just Refl
  IngestFile x        `geq` IngestFile y        | x == y  = Just Refl
  IngestDirectory x   `geq` IngestDirectory y   | x == y  = Just Refl
  _                   `geq` _                             = Nothing

instance GShow Query where
  gshowsPrec = defaultGshowsPrec

instance Hashable (Query a) where
  hashWithSalt = defaultHashWithSalt

  hash = \case
    InitCodebase        -> go  0 ()
    LookupName a        -> go  1 a
    RenameExpr a        -> go  2 a
    RenameDecls a       -> go  3 a
    IngestDecl a b      -> go  7 (a, b)
    FileText a          -> go 10 a
    FileRope a          -> go 11 a
    ParsedFile a        -> go 12 a
    IngestFile a        -> go 13 a
    IngestDirectory a   -> go 14 a
    where
      go :: (Hashable b) => Int -> b -> Int
      go tag a = Data.Hashable.hash tag `hashWithSalt` a

instance Hashable (Some Query) where
  hash (Some query) = hash query
  hashWithSalt salt (Some query) = hashWithSalt salt query

deriving instance Show (Query a)
deriving instance Typeable (Query a)

{-# Specialize
    memoiseWithCycleDetection ::
      (Memoiseable Query)
      => MutVar RealWorld (DHashMap Query MemoEntry)
      -> MutVar RealWorld (HashMap ThreadId ThreadId)
      -> GenRules IO Query Query
      -> GenRules IO Query Query
    #-}
