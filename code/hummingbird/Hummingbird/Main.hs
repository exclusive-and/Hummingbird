module Hummingbird.Main where

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

-- | Run a 'Task' using the inference rules defined in 'compileRules'.
run :: Task Query IO a -> IO (a, [Error])
run task = do
  errorsRef <- newIORef []
  codebase <- Codebase.init
  let
    recordErrors :: p a -> [Error] -> Task Query IO ()
    recordErrors _key errs =
      liftIO $ atomicModifyIORef_ errorsRef (++ errs)
  let
    rules :: Rules Query
    rules = writer ignoreTaskKind
          $ writer recordErrors
          $ compileRules codebase (fmap Right . Text.readFile) parse_
  Fetch.runTask rules $ do
    result <- task
    errors <- liftIO (readIORef errorsRef)
    pure (result, errors)

-- | The compiler's top-level inference rules.
compileRules ::
  Codebase
  -> ReadFile
  -> (FilePath -> ParseModuleSource)
  -> GenRules
      (Writer [Error] (Writer TaskKind Query))
      Query

compileRules codebase readFile_ parse_ (Writer (Writer query)) = case query of
  InitCodebase ->
    noError $ pure codebase
  GetModule modName ->
    noError $ liftIO $
    Codebase.lookupModule modName codebase >>= \case
      Nothing -> do
        pure $ Surface.Module modName []
      Just found -> pure $ fst found
  ModuleDefines modName ->
    noError $ liftIO $
    Codebase.lookupModule modName codebase >>= \case
      Nothing -> pure Map.empty
      Just found -> pure $ snd found
  ModuleDefinitions modName ->
    noError $ do
      modDefn <- fetch $ GetModule modName
      pure $ Surface.decls modDefn
  IngestDecl modName decl ->
    noError $ ingestDecl modName decl codebase
  FileText path ->
    input $ getFileText readFile_ path
  FileRope path ->
    input $ getFileRope readFile_ path
  ParsedFile path ->
    noError $ parseModuleFile parse_ path
  IngestFile path ->
    noError $ ingestFile path codebase
  IngestDirectory rootPath ->
    noError $ ingestDirectory rootPath codebase

-- |
compileTask :: Version -> Task Query IO ()
compileTask version = do
  liftIO $ print "'compileTask'"
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

parse_ :: FilePath -> ParseModuleSource
parse_ path =
  tokenize 0 path >=> layoutize defaultKws path >=> parse path

data TaskKind = Input | NonInput
  deriving (Show)

ignoreTaskKind :: p a -> TaskKind -> Task Query IO ()
ignoreTaskKind _key _taskKind = pure ()

type Result a = ((a, TaskKind), [Error])

input :: (Functor m) => m a -> m (Result a)
input = fmap ((, mempty) . (, Input))

noError :: (Functor m) => m a -> m (Result a)
noError = fmap ((, mempty) . (, NonInput))

nonInput :: (Functor m) => m (a, [Error]) -> m (Result a)
nonInput = fmap (first (, NonInput))

