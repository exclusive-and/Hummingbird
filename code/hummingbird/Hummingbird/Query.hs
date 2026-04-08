module Hummingbird.Query where

import Data.Hashable
import Data.HashSet (HashSet)
import Data.Kind
import Data.Text (Text)
import Data.Text.Rope (Rope)
import Prelude
import Prettyprinter
import System.FilePath

import Hummingbird.Codebase (Codebase)
import Hummingbird.Elaboration.Rename (RnMap)
import Hummingbird.Error (Error)
import Hummingbird.Error qualified as Error
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var (Var)

data Query (answer :: Type) where

  -- | Initalize a new codebase.
  InitCodebase :: Query Codebase

  -- | Get the latest interned version of a module.
  GetModule :: !Name.Module -> Query (Surface.Module Var)

  -- | What names does this module define?
  ModuleDefines :: !Name.Module -> Query RnMap

  -- | What definitions does this module contain?
  ModuleDefinitions :: !Name.Module -> Query [Surface.Declaration Var]
  
  -- | Ingest a parsed surface-language declaration and try to intern
  -- its changes into the codebase.
  IngestDecl ::
    !Name.Module
    -> Surface.Declaration Name
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
