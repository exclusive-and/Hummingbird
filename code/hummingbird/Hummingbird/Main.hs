{-# Language DataKinds #-}
{-# Language GADTs #-}
{-# Language OverloadedStrings #-}

module Hummingbird.Main where

import Num
import Text qualified

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Foldable
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe
import Data.These
import Fetch (fetch, runTask, Task, Rules, GenRules)
import Fetch qualified
import Fetch.Except (Except)
import Fetch.Except qualified as Fetch
import Fetch.Writer (Writer)
import Fetch.Writer qualified as Fetch
import Prettyprinter (Pretty (pretty))
import Prettyprinter qualified as Pretty

import Hummingbird.Dialectica qualified as Dialectica
import Hummingbird.Error (Error)
import Hummingbird.Error qualified as Error
import Hummingbird.Prelude
import Hummingbird.Surface qualified as Hb
import Hummingbird.Surface.Layoutize
import Hummingbird.Surface.Parse
import Hummingbird.Surface.Parsec
import Hummingbird.Query (Query)
import Hummingbird.Query qualified as Query
import Hummingbird.Surface.Token as Token
import Hummingbird.Surface.Tokenize
import Hummingbird.Var (
    Var (..),
    InScope,
    VarMap,
  )
import Hummingbird.Var qualified as Var

main :: IO ()
main = do
  let
    dirs = HashSet.fromList [
        "hummingbird/examples"
      ]
    files = HashSet.fromList [
        "hummingbird/examples/test"
      ]
  result <- run dirs files compileTask
  case result of
    Left errs -> print $ Pretty.vcat $ pretty <$> errs
    Right okay -> pure okay

run ::
  HashSet FilePath
  -> HashSet FilePath
  -> Task Query IO a
  -> IO (Either [Error] a)
run sourceDirectories files task = do
  let
    parse_ path text = first (pure . Error.Parse path . Text.pack . show) $ do
      tokens0 <- parse (tokensLex @(Token 'Layout)) path text
      tokens1 <- runParser (tokensLex @(Token 'NonLayout)) (initLayoutState [Module]) path tokens0
      parse hummingbirdP path tokens1
  let
    raiseErrors ::
      Query a
      -> [Error]
      -> Task Query IO a
    raiseErrors _ errs =
      liftIO $ throwIO (Error.Errors errs)
  let
    ignoreTaskKind ::
      Fetch.Except [Error] Query a
      -> Fetch.TaskKind
      -> Task Query IO ()
    ignoreTaskKind q _ = pure ()
  let
    readFile_ path = Right <$> Text.pack <$> readFile path
  let
    rules :: Rules Query
    rules =
      Fetch.handle raiseErrors $
        Fetch.writer ignoreTaskKind $
          Dialectica.rules sourceDirectories files readFile_ parse_
  catch
    (Right <$> Fetch.runTask rules task)
    (\(Error.Errors errs) -> Left <$> pure errs)

compileTask :: Task Query IO ()
compileTask = do
  paths <- fetch $ Query.InputFiles
  liftIO $ print paths
  modGuts <- fetch $ Query.ParsedModule "test"
  liftIO $ print $ pretty $ modGuts
  pure ()
