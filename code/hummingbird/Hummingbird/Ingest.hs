module Hummingbird.Ingest where

import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Rope (Rope)
import Data.These
import Prelude
import Prettyprinter
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
import Hummingbird.Elaboration.Rename (RnMap, runRename, renameBinds)
import Hummingbird.Error (Error)
import Hummingbird.Error qualified as Error
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Query (Query (..))
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var (Var (Prim))
import Hummingbird.VarMap (VarMap)
import Hummingbird.VarMap qualified as VarMap

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

type Ingest = Codebase -> Task Query IO (Maybe [Error])

ingestDecl :: Name.Module -> Surface.Declaration Name -> Ingest
ingestDecl modName decl codebase = do
  pure Nothing
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
    Left errs -> pure $ Just errs
    Right (Surface.Module modName decls) -> do
      let
        (errs, renamed) = renameMod modName decls
      case renamed of
        Nothing -> pure errs
        Just (rnMap, renamed') -> liftIO $ do
          let modRenamed = Surface.Module modName $ map (uncurry Surface.Fun) renamed'
          Codebase.addModule modName modRenamed rnMap codebase
          pure errs
  where
    prims = Prim <$> builtins

    renameMod modName decls =
      case runRename prims (`rename` decls) of
        This msgs -> (Just msgs, Nothing)
        That result -> (Nothing, Just result)
        These msgs result -> (Just msgs, Just result)

    rename inScope decls = renameBinds inScope $ binds [] decls

    binds acc [] = acc
    binds acc (decl:decls) = case decl of
      Surface.Fun name term -> binds ((name, term):acc) decls
      Surface.Sig _name _type -> binds acc decls

ingestDirectory :: FilePath -> Ingest
ingestDirectory rootPath codebase = do
  pure Nothing
  -- TODO: Implement recursive ingestion for directories.
