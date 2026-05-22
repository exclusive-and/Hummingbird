module Hummingbird.Query where

import Data.GADT.Compare
import Data.Hashable
import Data.HashSet (HashSet)
import Data.Kind
import Data.Map (Map)
import Data.Some
import Data.Text (Text)
import Data.Text.Rope (Rope)
import Data.Typeable
import Prelude
import Prettyprinter
import System.FilePath

import Hummingbird.Codebase as Codebase
import Hummingbird.Error as Error
import Hummingbird.Name as Name
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var (Var)

data Query (answer :: Type) where
  -- | Initalize a new codebase.
  InitCodebase :: Query Codebase
  -- | Get the latest interned version of a module.
  GetModule :: !Name.Module -> Query (Surface.Module Var)
  -- | What names does this module define?
  ModuleDefines :: !Name.Module -> Query (Map Name Var)
  -- | What definitions does this module contain?
  ModuleDefinitions :: !Name.Module -> Query [Surface.Declaration Var]
  -- |
  RenameExpr :: Surface.Term Name -> Query (CodePatch Codebase.Renamed)
  -- |
  RenameDecl :: Surface.Declaration Name -> Query (CodePatch Codebase.Renamed)
  -- | Ingest a parsed surface-language declaration and try to intern
  -- its changes into the codebase.
  IngestDecl ::
    !Name.Module
    -> Surface.Declaration Name
    -> Query (Maybe [Error])
  -- |
  ParsedRepl :: !Text -> Query (CodePatch Parsed)
  -- |
  IngestRepl :: !Text -> Query (CodePatch Codebase.Renamed)
  -- | What is the raw text of this source file?
  FileText :: !FilePath -> Query Text
  -- | What is the rope representation of this source file?
  FileRope :: !FilePath -> Query Rope
  -- |
  ParsedFile :: !FilePath -> Query (Surface.Module Name)
  -- | Ingest a source file and try to intern its changes into the codebase.
  IngestFile :: !FilePath -> Query (Maybe [Error])
  -- | Ingest a directory recursively.
  IngestDirectory :: !FilePath -> Query (Maybe [Error])

instance Eq (Query a) where
  (==) = defaultEq

instance GEq Query where
  InitCodebase        `geq` InitCodebase                  = Just Refl
  GetModule x         `geq` GetModule y         | x == y  = Just Refl
  ModuleDefines x     `geq` ModuleDefines y     | x == y  = Just Refl
  ModuleDefinitions x `geq` ModuleDefinitions y | x == y  = Just Refl
  RenameExpr x        `geq` RenameExpr y        | x == y  = Just Refl
  RenameDecl x        `geq` RenameDecl y        | x == y  = Just Refl
  IngestDecl nm1 decl1 `geq` IngestDecl nm2 decl2
    | nm1 == nm2, decl1 == decl2                          = Just Refl
  ParsedRepl x        `geq` ParsedRepl y        | x == y  = Just Refl
  IngestRepl x        `geq` IngestRepl y        | x == y  = Just Refl
  FileText x          `geq` FileText y          | x == y  = Just Refl
  FileRope x          `geq` FileRope y          | x == y  = Just Refl
  ParsedFile x        `geq` ParsedFile y        | x == y  = Just Refl
  IngestFile x        `geq` IngestFile y        | x == y  = Just Refl
  IngestDirectory x   `geq` IngestDirectory y   | x == y  = Just Refl
  _                   `geq` _                             = Nothing

instance Hashable (Query a) where
  hashWithSalt = defaultHashWithSalt

  hash = \case
    InitCodebase -> go 0 ()
    GetModule a -> go 1 a
    ModuleDefines a -> go 2 a
    ModuleDefinitions a -> go 3 a
    RenameExpr a -> go 4 a
    RenameDecl a -> go 5 a
    IngestDecl a b -> go 5 (a, b)
    ParsedRepl a -> go 6 a
    IngestRepl a -> go 7 a
    FileText a -> go 8 a
    FileRope a -> go 9 a
    ParsedFile a -> go 10 a
    IngestFile a -> go 11 a
    IngestDirectory a -> go 12 a
    where
      go :: (Hashable b) => Int -> b -> Int
      go tag a = hash tag `hashWithSalt` a

instance Hashable (Some Query) where
  hash (Some query) = hash query
  hashWithSalt salt (Some query) = hashWithSalt salt query

deriving instance Show (Query a)
deriving instance Typeable (Query a)
