module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Data.IORef
import Data.IORef.Extra (atomicModifyIORef_)
import Data.Text qualified as Text
import Data.Typeable
import Prelude
import Prettyprinter
import Prettyprinter.Render.Terminal

import GHC.Conc.Sync (setUncaughtExceptionHandler)

import System.Environment
import System.Exit
import System.IO

import Hummingbird.Error as Error
import Hummingbird.Main
import Hummingbird.Repl
import Hummingbird.Version as Version

main :: IO ()
main =
  do
  setUncaughtExceptionHandler \uncaught -> do
    let
      displayed =
        displayException uncaught
          & Text.pack
          & Text.lines
          & vcat . map pretty
    hPutDoc stderr $ hang 4 $ vcat [
        annotate (color Red <> underlined) "catastrophic uncaught exception:"
      , displayed
      ]

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
  
  catch @_ @[Error] critical Error.reportAll
