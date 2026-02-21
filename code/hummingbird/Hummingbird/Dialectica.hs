module Hummingbird.Dialectica where

import Birds.Prelude

import Control.Monad
import Control.Monad.Trans
import Data.Bifunctor
import Data.Foldable
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Rope (Rope)
import Data.These
import Prettyprinter qualified as Pretty
import System.FilePath

import Fetch (
  fetch,
  Task,
  GenRules,
  Except (..),
  Writer (..),
  )
import Fetch qualified
import Fetch.Mapped qualified as Mapped

import Hummingbird.Builtin (builtins)
import Hummingbird.Codebase (Codebase)
import Hummingbird.Codebase qualified as Codebase
import Hummingbird.Error (Error)
import Hummingbird.Error qualified as Error
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Query (Query (..))
import Hummingbird.Rename (RnMap, runRename, renameBinds)
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var (Var (Prim))
import Hummingbird.VarMap (VarMap)
import Hummingbird.VarMap qualified as VarMap

rules ::
  ReadFile
  -> (FilePath -> ParseModuleSource)
  -> GenRules
      (Writer [Error] (Writer TaskKind Query))
      Query

rules readFile_ parse_ (Writer (Writer query)) =
  case query of
    GetCodebase ->
      noError $ pure Codebase.empty

    GetModule modName codebase ->
      noError $
      case Codebase.lookupModule modName codebase of
        Nothing -> do
          pure $ Surface.Module modName []
        Just found -> pure $ fst found

    ModuleDefines modName codebase ->
      noError $
      case Codebase.lookupModule modName codebase of
        Nothing -> pure Map.empty
        Just found -> pure $ snd found

    ModuleDefinitions modName codebase ->
      noError $ do
        modDefn <- fetch $ GetModule modName codebase
        pure $ Surface.decls modDefn

    IngestDecl modName decl codebase ->
      noError $ ingestDecl modName decl codebase

    FileText path ->
      input $ getFileText readFile_ path

    FileRope path ->
      input $ getFileRope readFile_ path

    ParsedFile path ->
      noError $ parseModuleFile parse_ path

    IngestFile path codebase ->
      noError $ ingestFile path codebase

    IngestDirectory rootPath codebase ->
      noError $ ingestDirectory rootPath codebase

type ReadFile = FilePath -> IO (Either Rope Text)

getFileText :: ReadFile -> FilePath -> Task Query IO Text
getFileText readFile_ path = do
  result <- liftIO $ readFile_ path
  case result of
    Left rope -> undefined
    Right text -> pure text

getFileRope :: ReadFile -> FilePath -> Task Query IO Rope
getFileRope readFile_ path = do
  result <- liftIO $ readFile_ path
  case result of
    Left rope -> pure rope
    Right text -> undefined

type Ingest = forall cdb cdb'.
  Codebase cdb -> Task Query IO (Maybe [Error], Codebase cdb')

ingestDecl :: Name.Module -> Surface.Declaration Name -> Ingest
ingestDecl modName decl codebase = do
  pure (Nothing, Codebase.clone codebase)
  -- TODO: Implement ingestion for pre-parsed declarations.

type ParseModuleSource = Text -> Either [Error] (Surface.Module Name)

parseModuleFile ::
  (FilePath -> ParseModuleSource)
  -> FilePath
  -> Task Query IO (Either [Error] (Surface.Module Name))

parseModuleFile parse_ path = do
  parse_ path <$> fetch (FileText path)

ingestFile :: FilePath -> Ingest
ingestFile path codebase = do
  parsed <- fetch $ ParsedFile path
  case parsed of
    Left errs -> pure (Just errs, Codebase.clone codebase)
    Right (Surface.Module modName decls) -> do
      let
        (errs, renamed) = renameMod modName decls
      case renamed of
        Nothing -> pure (errs, Codebase.clone codebase)
        Just (rnMap, renamed') -> do
          let modRenamed = Surface.Module modName $ map (uncurry Surface.Fun) renamed'
          pure (errs, Codebase.insertModule modName modRenamed rnMap codebase)
  where
    prims = Prim <$> builtins

    renameMod modName decls =
      case runRename prims (`rename` decls) of
        This msgs -> (wrapRnMsg modName msgs, Nothing)
        That result -> (Nothing, Just result)
        These msgs result -> (wrapRnMsg modName msgs, Just result)

    wrapRnMsg modName msg = Just [Error.Rename modName msg]

    rename inScope decls = renameBinds inScope $ binds [] decls

    binds acc [] = acc
    binds acc (decl:decls) = case decl of
      Surface.Fun name term -> binds ((name, term):acc) decls
      Surface.Sig _name _type -> binds acc decls

ingestDirectory :: FilePath -> Ingest
ingestDirectory rootPath codebase = do
  pure (Nothing, Codebase.clone codebase)
  -- TODO: Implement recursive ingestion for directories.

type Result a = ((a, TaskKind), [Error])

data TaskKind
  = Input
  | NonInput
  deriving (Show)

input :: (Functor m) => m a -> m (Result a)
input = fmap ((, mempty) . (, Input))

noError :: (Functor m) => m a -> m (Result a)
noError = fmap ((, mempty) . (, NonInput))

nonInput :: (Functor m) => m (a, [Error]) -> m (Result a)
nonInput = fmap (first (, NonInput))
