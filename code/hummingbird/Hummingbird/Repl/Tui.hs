module Hummingbird.Repl.Tui where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Chronicle
import Control.Monad.State
import Data.IORef
import Data.IORef.Extra (atomicModifyIORef_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String
import Data.Text qualified as Text
import Prelude
import Prettyprinter
import Prettyprinter.Render.Text

import Brick
import Brick.AttrMap
import Brick.BChan
import Brick.Widgets.Border

import Graphics.Vty qualified as Vty

import Hummingbird.Codebase as Codebase
import Hummingbird.Codebase.Patch as Codebase
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

tuiMain :: Chan Cmd -> BChan Event -> IO ()
tuiMain replCmdChan bchan = do
  replContentsRef <- newIORef mempty
  cmdContentsRef <- newIORef mempty
  stackContentsRef <- newIORef mempty
  let
    initialConfig = Config replCmdChan replContentsRef cmdContentsRef stackContentsRef
    initialState = UIState mempty mempty mempty
  (_, vty) <- Brick.customMainWithDefaultVty
    (Just bchan)
    (app initialConfig)
    initialState
  Vty.shutdown vty

data AppName
  = HummingbirdTui
  | ReplViewport
  | StackViewport
  deriving (Eq, Ord, Show)

data UIState = UIState {
    replContents :: [Text]
  , cmdContents :: String
  , stackContents :: [Text]
  }

data Event
  = StackChanged
  | ReplError [Error]
  | ReplIngested (CodePatch Typechecked)
  deriving (Eq, Show)

data Config e = Config {
    replCmdChan :: Chan Cmd
  , replContentsRef :: IORef [Text]
  , cmdContentsRef :: IORef String
  , stackContentsRef :: IORef [Text]
  }

tuiTask ::
  Version
  -> Chan Cmd
  -> BChan Event
  -> Task Query IO ()
tuiTask version cmdChan bchan = go
  where
    go = do
      cmd <- liftIO $ readChan cmdChan
      case cmd of
        ShutdownRepl -> pure ()
        InterpretLine exprText -> do
          fetch (ParsedRepl exprText) >>= applyPatch bchan
          go
        _ -> go

applyPatch :: BChan Event -> CodePatch a -> Task Query IO ()
applyPatch bchan = go
  where
    go :: forall a. CodePatch a -> Task Query IO ()
    go (AddDecls [] decls) = do
      fetch (RenameDecls decls) >>= go
    go (AddDecls errors _) = do
      liftIO $ writeBChan bchan $ ReplError errors
    go (AddExprs [] [expr]) = do
      fetch (RenameExpr expr) >>= go
    go (AddExprs errors _) = do
      liftIO $ writeBChan bchan $ ReplError errors
    go (AddHashedTerms [] env ok tms tys) = do
      -- TODO: some sort of typechecking here...
      go $ AddCheckedTerms [] env ok tms tys
    go (AddHashedTerms errors _ _ _ _) = do
      liftIO $ writeBChan bchan $ ReplError errors
    go patch@(AddCheckedTerms errors env ok tms tys) = do
      codebase <- fetch InitCodebase
      liftIO do
        writeBChan bchan $ ReplIngested patch
        Codebase.applyChecked codebase patch

app :: Config Event -> Brick.App UIState Event AppName
app config = Brick.App {
    Brick.appDraw = drawUI
  , Brick.appChooseCursor = showFirstCursor
  , Brick.appHandleEvent = handleEvent config
  , Brick.appStartEvent = pure ()
  , Brick.appAttrMap = const $ attrMap Vty.defAttr []
  }

drawUI :: UIState -> [Widget AppName]
drawUI UIState{..} = [replWidget Brick.<+> stackWidget]
  where
    replWidget =
      drawRepl replContents cmdContents
        & viewport ReplViewport Vertical
        & border

    stackWidget =
      []
      & vBox
      & viewport StackViewport Vertical
      & hLimit 20
      & border

drawRepl :: [Text] -> String -> Widget AppName
drawRepl prevCmds cmd =
  drawPrevCmds <=> str ("> " ++ cmd)
  where
    drawPrevCmds = vBox (map txt prevCmds)

handleEvent ::
  Config Event
  -> BrickEvent AppName Event
  -> EventM AppName UIState ()

handleEvent _ (AppEvent (ReplError errs)) = do
  repl <- gets replContents
  let
    repl' = map (renderStrict . layoutPretty defaultLayoutOptions . pretty) errs
  modify \s -> s {
      replContents = repl ++ repl'
    }

handleEvent _ (AppEvent (ReplIngested ingested)) = do
  repl <- gets replContents
  let
    repl' = Text.lines $ renderStrict $ layoutPretty defaultLayoutOptions $ pretty ingested
  modify \s -> s {
      replContents = repl ++ repl'
    }

handleEvent Config{..} (VtyEvent (Vty.EvKey key mods)) = do
  case key of
    Vty.KEsc -> do
      liftIO $ writeChan replCmdChan ShutdownRepl
      halt
    Vty.KChar c -> do
      cmd <- gets cmdContents
      modify \s -> s { cmdContents = cmd ++ [c] }
    Vty.KBS -> do
      cmd <- gets cmdContents
      modify \s -> s { cmdContents = reverse $ drop 1 $ reverse cmd }
    Vty.KEnter -> do
      repl <- gets replContents
      cmd <- gets cmdContents
      modify \s -> s {
          replContents = repl ++ [Text.pack ("> " ++ cmd)]
        , cmdContents = ""
        }
      liftIO $ writeChan replCmdChan (InterpretLine $ Text.pack cmd)
    _otherKey -> pure ()

handleEvent _ _ = pure ()
