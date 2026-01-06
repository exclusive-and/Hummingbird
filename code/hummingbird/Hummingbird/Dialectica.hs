{-# Language NamedFieldPuns #-}
{-# Language OverloadedRecordDot #-}
{-# Language OverloadedStrings #-}

module Hummingbird.Dialectica where

import Num
import Text qualified

import Control.Monad
import Control.Monad.Trans
import Data.Bifunctor
import Data.Foldable
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Text.Rope (Rope)
import Fetch (fetch, Task, GenRules)
import Fetch qualified
import Fetch.Chronicle
import Fetch.Except (Except (..))
import Fetch.Mapped qualified as Mapped
import Fetch.Writer (Writer (..))
import Prettyprinter (Pretty (pretty))
import Prettyprinter qualified as Pretty
import System.FilePath

import Hummingbird.Error (Error)
import Hummingbird.Error qualified as Error
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Prelude
import Hummingbird.Query (Query (..))
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var (
    Var (..),
    InScope,
    VarMap,
  )
import Hummingbird.Var qualified as Var

{-}
data Rules = Rules {
    getSourceDirs :: Task Query [FilePath],
    getFileText :: FilePath -> Task Query Text,
    getFileRope :: FilePath -> Task Query Rope,
    getModule :: Name.Module -> Task Query Module,
    getModuleFile :: Name.Module -> Task Query (Either [Error] FilePath),
    getModuleParsed :: FilePath -> Task Query (Either [Error] (Surface.Module Name)),
    getModuleDefines :: Name.Module -> Task Query (HashSet Name),
    getModuleDefinitions :: Name.Module -> Task Query [Surface.Declaration Name]
  }
-}

rules ::
     HashSet FilePath
  -> HashSet FilePath
  -> (FilePath -> IO (Either Rope Text))
  -> (FilePath -> Text -> Either [Error] (Surface.Module Name))
  -> GenRules
      (Writer Fetch.TaskKind (Except [Error] Query))
      Query

rules dirs files readFile_ parse_ (Writer (Except query)) =
  case query of
    SourceDirectories ->
      Fetch.input $ getSourceDirs dirs files
    InputFiles ->
      Fetch.input $ pure files
    FileText path ->
      Fetch.input $ getFileText readFile_ path
    FileRope path ->
      Fetch.input $ getFileRope readFile_ path
    ParsedModule moduleName ->
      Fetch.noError $ getParsedModule moduleName
    ModuleFile moduleName ->
      Fetch.nonInput $ getModuleFile moduleName
    ParsedFile path ->
      Fetch.nonInput $ getParsedFile path parse_
    ModuleDefines moduleName ->
      Fetch.noError $ getModuleDefines moduleName
    ModuleDefinitions moduleName ->
      Fetch.noError $ getModuleDefinitions moduleName

-- |
getSourceDirs ::
  HashSet FilePath
  -> HashSet FilePath
  -> Task Query IO [FilePath]
getSourceDirs dirs files = do
  case (HashSet.toList dirs, HashSet.toList files) of
    ([], [file]) ->
      pure [takeDirectory file]
    (dirs', _) ->
      pure dirs'

-- |
getFileText ::
  (FilePath -> IO (Either Rope Text))
  -> FilePath
  -> Task Query IO Text
getFileText readFile_ path = do
  result <- liftIO $ readFile_ path
  case result of
    Left rope -> undefined
    Right text -> pure text

-- |
getFileRope ::
  (FilePath -> IO (Either Rope Text))
  -> FilePath
  -> Task Query IO Rope
getFileRope readFile_ path = do
  result <- liftIO $ readFile_ path
  case result of
    Left rope -> pure rope
    Right text -> undefined

-- |
getParsedModule ::
  Name.Module
  -> Task Query IO (Surface.Module Name)
getParsedModule moduleName = do
  path <- fetch $ ModuleFile moduleName
  fetch $ ParsedFile path

-- |
getModuleFile ::
  Name.Module
  -> Task Query IO (Either [Error] FilePath)
getModuleFile moduleName@(Name.Module modNameText) = do
  files <- fetch InputFiles
  dirs <- fetch SourceDirectories
  let
    candidates =
      [ candidate
      | dir <- dirs
      , let candidate = dir </> joinPath (map Text.unpack $ Text.splitOn "." modNameText)
      , candidate `HashSet.member` files
      ]
  case candidates of
    [] ->
      pure $ Left [Error.ModuleNotFound moduleName]
    [path] ->
      pure $ Right path
    path:paths ->
      pure $ Left [Error.DuplicateModules moduleName (path:paths)]

-- |
getParsedFile ::
  FilePath
  -> (FilePath -> Text -> Either [Error] (Surface.Module Name))
  -> Task Query IO (Either [Error] (Surface.Module Name))
getParsedFile path parse_ = do
  text <- fetch $ FileText path
  case parse_ path text of
    Left errs -> pure $ Left errs
    Right okay -> pure $ Right okay

-- |
getModuleDefines ::
  Name.Module
  -> Task Query IO (HashSet Name)
getModuleDefines moduleName = do
  undefined

-- |
getModuleDefinitions ::
  Name.Module
  -> Task Query IO [Surface.Declaration Name]
getModuleDefinitions moduleName = do
  Surface.Module{Surface.decls} <- fetch $ ParsedModule moduleName
  pure decls
