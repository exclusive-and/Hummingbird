module Main where

import Control.Exception
import Data.IORef
import Data.IORef.Extra (atomicModifyIORef_)
import GHC.IO
import GHC.IO.Handle
import Prelude
import Prettyprinter

import System.IO as IO

import Hummingbird.Error as Error
import Hummingbird.Main
import Hummingbird.Repl
import Hummingbird.Version

main :: IO ()
main =
  do
  _ <- newIORef []
  
  --
  -- Make sure standard outputs are properly buffered so that diagnostics and
  -- error reports are legible.
  hSetBuffering stderr LineBuffering
  hSetBuffering stdout LineBuffering

  let
    version :: Version
    version =
      Version "hummingbird" ("", "")

  let
    critical :: IO ()
    critical = do
      (_, errors) <- run (replTask version)
      Error.reportAll errors
  
  catch @[Error] critical Error.reportAll
