module Hummingbird.Repl where

import Control.Exception
import Data.IORef
import Data.IORef.Extra (atomicModifyIORef_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Prelude
import Prettyprinter

import Fetch
  ( fetch
  , GenRules
  , Rules
  , Task
  , writer
  , Writer (..)
  )
import Fetch qualified

import Hummingbird.Codebase as Codebase
import Hummingbird.Error as Error
import Hummingbird.Ingest
import Hummingbird.Interpret (interpret)
import Hummingbird.Name as Name
import Hummingbird.Query
import Hummingbird.Repl.Command
import Hummingbird.Repl.Console
import Hummingbird.Surface qualified as Surface
import Hummingbird.Surface.Layoutize (defaultKws, layoutize)
import Hummingbird.Surface.Parse (parse)
import Hummingbird.Surface.Tokenize (tokenize)
import Hummingbird.VarMap qualified as VarMap
import Hummingbird.Version (Version)

replTask :: Version -> Task Query IO ()
replTask version = do
  let
    modName :: Name.Module
    modName = "Example"
  let
    srcFilePath :: FilePath
    srcFilePath =  "code/hummingbird/examples/test"
  codebase <- fetch InitCodebase
  ingested <- fetch $ IngestFile srcFilePath
  case ingested of
    Nothing -> pure ()
    Just errs -> liftIO $ throwIO errs
  modGuts <- fetch $ GetModule modName
  liftIO $ print $ pretty modGuts
  rnMap <- fetch $ ModuleDefines modName
  modDecls <- fetch $ ModuleDefinitions modName
  case Map.lookup "main" rnMap of
    Nothing -> pure ()
    Just entry -> liftIO $ do
      let decls = map (\(Surface.Fun name term) -> (name, term)) modDecls
      result <- interpret entry (VarMap.fromList decls)
      print $ pretty result
  pure ()
