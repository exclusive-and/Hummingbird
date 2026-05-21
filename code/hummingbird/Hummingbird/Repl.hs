module Hummingbird.Repl where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Chronicle
import Data.IORef
import Data.IORef.Extra (atomicModifyIORef_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Prelude
import Prettyprinter

import Hummingbird.Codebase as Codebase
import Hummingbird.Error as Error
import Hummingbird.Fetch as Fetch
import Hummingbird.Ingest
import Hummingbird.Interpret (interpret)
import Hummingbird.Name as Name
import Hummingbird.Query
import Hummingbird.Repl.Command
import Hummingbird.Repl.Console
import Hummingbird.Surface qualified as Surface
import Hummingbird.VarMap qualified as VarMap
import Hummingbird.Version (Version)

replTask :: Version -> Chan Cmd -> Task Query (ChronicleT [Error] IO) ()
replTask version cmdChan = go
  where
    go = do
      cmd <- liftIO $ readChan cmdChan
      case cmd of
        ShutdownRepl -> pure ()
        InterpretLine exprText -> do
          ingested <- memento $ fetch $ IngestRepl exprText
          case ingested of
            Left errors -> liftIO $ throwM errors
            Right _result -> go
        _ -> go
