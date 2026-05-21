module Hummingbird.Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Chronicle
import Data.IORef
import Data.IORef.Extra (atomicModifyIORef_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These
import Prelude
import Prettyprinter

import Hummingbird.Codebase as Codebase
import Hummingbird.Codebase.Db as Codebase
import Hummingbird.Elaboration.Hash
  ( renameDeclTask
  , renameExprTask
  )
import Hummingbird.Error as Error
import Hummingbird.Fetch as Fetch
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
  memoVar <- newIORef mempty
  errorsRef <- newIORef []
  codebase <- Codebase.init
  let
    recordErrors ::
      (MonadIO m)
      => p a
      -> [Error]
      -> Task Query m ()
    recordErrors _key errs =
      liftIO $ atomicModifyIORef_ errorsRef (++ errs)
  let
    rules :: Rules IO Query
    rules =
      compileRules codebase (fmap Right . Text.readFile) parse_
        & writer recordErrors
        & writer ignoreTaskKind
        & memoise memoVar
  Fetch.runTask rules
      do
        result <- task
        errors <- liftIO (readIORef errorsRef)
        pure (result, errors)

-- | The compiler's top-level inference rules.
compileRules ::
  Codebase
  -> ReadFile
  -> (FilePath -> ParseModuleSource)
  -> GenRules
      IO
      (Writer [Error] (Writer TaskKind Query))
      Query

compileRules codebase readFile_ parse_ =
  let
    go :: Query a -> Task Query IO (Result a)
    go InitCodebase =
      noError $ pure codebase

    go (GetModule modName) = noError do
      Codebase.lookupModule modName codebase >>= \case
        Nothing -> do
          pure $ Surface.Module modName []
        Just found -> pure $ fst found

    go (ModuleDefines modName) = noError do
      Codebase.lookupModule modName codebase >>= \case
        Nothing -> pure Map.empty
        Just found -> pure $ snd found

    go (ModuleDefinitions modName) =
      noError $ Surface.decls <$> fetch (GetModule modName)
    
    go (RenameExpr expr) = noError do
      env <- Codebase.getNameMap codebase
      renameExprTask env expr
    
    go (RenameDecl decl) = noError do
      env <- Codebase.getNameMap codebase
      renameDeclTask env decl
    
    go (IngestDecl modName decl) =
      noError $ ingestDecl modName decl codebase
    go (ParsedRepl replLine) =
      input $ parseRepl replLine
    go (IngestRepl replLine) =
      noError $ ingestRepl replLine
    go (FileText path) =
      input $ getFileText readFile_ path
    go (FileRope path) =
      input $ getFileRope readFile_ path
    go (ParsedFile path) =
      noError $ parseModuleFile parse_ path
    go (IngestFile path) =
      noError $ ingestFile path codebase
    go (IngestDirectory rootPath) =
      noError $ ingestDirectory rootPath codebase
  in
    GenRules \(Writer (Writer query)) -> go query

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
    Just errs -> liftIO $ throwM errs
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

ignoreTaskKind :: (Monad m) => p a -> TaskKind -> Task Query m ()
ignoreTaskKind _key _taskKind = pure ()

type Result a = ((a, TaskKind), [Error])

input :: (Functor m) => m a -> m (Result a)
input = fmap ((, mempty) . (, Input))

noError :: (Functor m) => m a -> m (Result a)
noError = fmap ((, mempty) . (, NonInput))

nonInput :: (Functor m) => m (a, [Error]) -> m (Result a)
nonInput = fmap (first (, NonInput))

