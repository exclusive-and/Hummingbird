module Hummingbird.Ingest where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Chronicle
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

import Hummingbird.Builtin (builtins)
import Hummingbird.Codebase as Codebase
import Hummingbird.Codebase.Db as Codebase
import Hummingbird.Elaboration.Rename (RnMap, runRename, renameBinds)
import Hummingbird.Error (Error)
import Hummingbird.Error qualified as Error
import Hummingbird.Fetch as Fetch
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Query (Query (..))
import Hummingbird.Surface qualified as Surface
import Hummingbird.Surface.Layoutize (defaultKws, layoutize)
import Hummingbird.Surface.Parse (parse, parseDeclOrExpr)
import Hummingbird.Surface.Tokenize (tokenize)
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

parseRepl :: Text -> Task Query IO (CodePatch Parsed)
parseRepl replContents =
  case parse_ replContents of
    Left errors             -> pure (AddDecls errors mempty)
    Right (Nothing  , expr) -> pure (AddExprs [] [expr])
    Right (Just name, expr) -> pure (AddDecls [] [Surface.Fun name expr])
  where
    path = "<repl>"
    parse_ = tokenize 0 path >=> layoutize defaultKws path >=> parseDeclOrExpr path

ingestRepl :: Text -> Task Query IO (CodePatch Hashed)
ingestRepl replLine = do
  parsed <- fetch $ ParsedRepl replLine
  case parsed of
    AddExprs errors [expr] -> do
      renamed <- fetch $ RenameExpr expr
      case renamed of
        AddHashedTerms errors' names ok terms types ->
          pure $ AddHashedTerms (errors ++ errors') names ok terms types
    AddDecls errors [decl] -> do
      renamed <- fetch $ RenameDecl decl
      case renamed of
        AddHashedTerms errors' names ok terms types ->
          pure $ AddHashedTerms (errors ++ errors') names ok terms types
    _ ->
      error "Hummingbird.Ingest.ingestRepl: cannot ingest more than one item at a time"

type ParseModuleSource = Text -> Either [Error] (Surface.Module Name)

parseModuleFile ::
  (FilePath -> ParseModuleSource)
  -> FilePath
  -> Task Query IO (Surface.Module Name)

parseModuleFile parse_ path = do
  sourceText <- fetch (FileText path)
  case parse_ path sourceText of
    Left errors -> throwM errors
    Right parsed -> pure parsed

ingestFile :: FilePath -> Ingest
ingestFile path codebase = do
  parsed <- fetch $ ParsedFile path
  case parsed of
    (Surface.Module modName decls) -> do
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
