module Hummingbird.Query where

import Num
import Text qualified

import Control.Monad
import Control.Monad.Trans
import Data.Bifunctor
import Data.Foldable
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Text.Rope (Rope)
import Prettyprinter (Pretty (pretty))
import Prettyprinter qualified as Pretty
import System.FilePath

import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Prelude
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var (
    Var (..),
    InScope,
    VarMap,
  )
import Hummingbird.Var qualified as Var

-- |  Questions about code.
-- I can compile things by computing answers to these queries.
data Query answer where

  -- | Which directories contain source files?
  SourceDirectories :: Query [FilePath]

  -- | What are my input files?
  InputFiles :: Query (HashSet FilePath)

  -- | What is the raw text of this source file?
  FileText :: !FilePath -> Query Text

  -- | What is the rope representation of this source file?
  FileRope :: !FilePath -> Query Rope

  -- | What file was this module defined in?
  ModuleFile :: !Name.Module -> Query FilePath

  -- |
  ParsedFile :: !FilePath -> Query (Surface.Module Name)

  -- |
  ParsedModule :: !Name.Module -> Query (Surface.Module Name)

  -- | What names does this module define?
  ModuleDefines :: !Name.Module -> Query (HashSet Name)

  -- | What definitions does this module contain?
  ModuleDefinitions :: !Name.Module -> Query [Surface.Declaration Name]
