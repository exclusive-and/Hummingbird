module Hummingbird.Repl.Command where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Text qualified as Text
import Prelude
import Prettyprinter
import Prettyprinter.Render.Terminal

import System.IO qualified as IO

import Text.Parsec
  ( Parsec
  )
import Text.Parsec qualified as Parsec

import System.Console.Haskeline

data Cmd
  = ShutdownRepl
  | InterpretLine !Text
  deriving (Eq, Ord, Show)
