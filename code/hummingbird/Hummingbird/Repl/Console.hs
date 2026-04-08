module Hummingbird.Repl.Console where

import Data.Text qualified as Text
import Prelude
import Prettyprinter
import Prettyprinter.Render.Terminal
import System.Console.ANSI
  ( hNowSupportsANSI
  )
import System.Console.ANSI qualified as ANSI
import System.Console.Haskeline
import System.FilePath
import System.IO
  ( stderr
  , stdin
  , stdout
  )
import System.IO qualified as IO
