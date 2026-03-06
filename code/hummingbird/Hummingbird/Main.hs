{-# Language DataKinds #-}
{-# Language GADTs #-}
{-# Language OverloadedStrings #-}

module Hummingbird.Main where

import Birds.Prelude

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Foldable
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Data.These
import Prettyprinter qualified as Pretty

import Fetch (
  fetch,
  Task,
  Rules,
  GenRules,
  Except (..),
  Writer (..),
  )
import Fetch qualified

import Hummingbird.Builtin
import Hummingbird.Dialectica qualified as Dialectica
import Hummingbird.Error (Error)
import Hummingbird.Error qualified as Error
import Hummingbird.Interpret (interpret)
import Hummingbird.Name qualified as Name
import Hummingbird.Surface qualified as Hb
import Hummingbird.Surface.Layoutize
import Hummingbird.Surface.Parse
import Hummingbird.Surface.Parsec
import Hummingbird.Query (Query)
import Hummingbird.Query qualified as Query
import Hummingbird.Rename (runRename, renameBinds)
import Hummingbird.Surface qualified as Surface
import Hummingbird.Surface.Token as Token
import Hummingbird.Surface.Tokenize
import Hummingbird.Var
import Hummingbird.VarMap (VarMap)
import Hummingbird.VarMap qualified as VarMap

main :: Name.Module -> FilePath -> IO ()
main modName srcFilePath = do
  run (compileTask modName srcFilePath) >>= \case
    Right okay -> pure okay
    Left errs -> print $ Pretty.vcat $ pretty <$> errs

run :: Task Query IO a -> IO (Either [Error] a)
run task = do
  let
    parse_ path text = first (pure . Error.Parse path . Text.pack . show) $ do
      tokens0 <- parse (tokensLex @(Token 'Layout)) path text
      tokens1 <- runParser (tokensLex @(Token 'NonLayout)) (initLayoutState [Module]) path tokens0
      parse hummingbirdP path tokens1
  let
    raiseErrors ::
      Writer Dialectica.TaskKind Query a
      -> [Error]
      -> Task Query IO ()
    raiseErrors _ errs = liftIO $ print $ pretty errs
  let
    ignoreTaskKind ::
      Query a
      -> Dialectica.TaskKind
      -> Task Query IO ()
    ignoreTaskKind q _ = pure ()
  let
    readFile_ path = Right . Text.pack <$> readFile path
  let
    rules :: Rules Query
    rules =
      Fetch.writer ignoreTaskKind $
        Fetch.writer raiseErrors $
          Dialectica.rules readFile_ parse_
  catch
    (Right <$> Fetch.runTask rules task)
    (\(Error.Errors errs) -> pure (Left errs))

compileTask :: Name.Module -> FilePath -> Task Query IO ()
compileTask modName srcFilePath = do
  codebase <- fetch Query.GetCodebase
  (ingested, codebase') <- fetch $ Query.IngestFile srcFilePath codebase
  case ingested of
    Nothing -> pure ()
    Just errs -> liftIO $ print $ pretty errs
  modGuts <- fetch $ Query.GetModule modName codebase'
  liftIO $ print $ pretty modGuts
  rnMap <- fetch $ Query.ModuleDefines modName codebase'
  modDecls <- fetch $ Query.ModuleDefinitions modName codebase'
  case Map.lookup "main" rnMap of
    Nothing -> pure ()
    Just entry -> liftIO $ do
      let decls = map (\(Surface.Fun name term) -> (name, term)) modDecls
      result <- interpret entry (VarMap.fromList decls)
      print $ pretty result
  pure ()
