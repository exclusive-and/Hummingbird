module Hummingbird.Interpret where

import Birds.Prelude

import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.State
import Data.Foldable
import Data.IORef
import Data.IntMap qualified
import Data.Map (Map)
import Data.Map qualified
import Data.Typeable
import Prettyprinter qualified as Pretty

import Hummingbird.Builtin
import Hummingbird.Literal (Literal)
import Hummingbird.Literal qualified as Literal
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Rename
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var
import Hummingbird.VarMap (VarMap)
import Hummingbird.VarMap qualified as VarMap

infixr 5 :::

type InterpretM = StateT (VarMap (Surface.Term Var)) (Either Text)

data Rep
  = Array [Rep]
  | Closure !(Surface.Term Var) [Rep]
  | Lit !Literal
  | Text !Text
  | Word !Var
  deriving (Show)

data Stack a = Bottom | !a ::: !(Stack a)
  deriving (Functor, Foldable)

instance Pretty Rep where
  pretty = \case
    Array reps -> pretty $ reverse reps
    Closure quoted args ->
      Pretty.brackets $ Pretty.hsep [
          Pretty.hsep $ Pretty.brackets . pretty <$> args
        , pretty quoted
        ]
    Lit  lit  -> pretty lit
    Text text -> pretty text
    Word word -> pretty word

newtype Failure = Failure (Pretty.Doc ())
  deriving (Typeable)

instance Exception Failure

instance Show Failure where
  show (Failure message) = show message

interpret :: Var -> VarMap (Surface.Term Var) -> IO [Rep]
interpret entry defns = do
  stackRef <- newIORef Bottom

  let
    interpTerm :: Surface.Term Var -> IO ()
    interpTerm = \case
      Surface.Lit literal -> do
        modifyIORef' stackRef (Lit literal :::)
      Surface.Word var -> interpWord var
      Surface.Lambda{} -> undefined
      Surface.Match{} -> undefined
      Surface.Quoted quoted -> do
        modifyIORef' stackRef (Closure quoted [] :::)
      Surface.Concat terms -> do
        forM_ terms interpTerm

    interpWord :: Var -> IO ()
    interpWord (Prim prim) = interpIntrinsic prim
    interpWord var =
      case VarMap.lookup var defns of
        Just defn ->
          interpTerm defn
        Nothing ->
          throwIO $ Failure $ Pretty.hcat [
              "No definition matching: "
            , pretty var
            ]

    interpIntrinsic :: Builtin -> IO ()
    interpIntrinsic prim = do
      stack <- readIORef stackRef
      case (prim, stack) of
        (Apply, Closure quoted args ::: r) -> do
          writeIORef stackRef r
          forM_ args $ \arg -> modifyIORef' stackRef (arg :::)
          interpTerm quoted

        (Cake, Closure a args ::: b ::: r) -> do
          writeIORef stackRef r
          forM_ [
              Closure a (b:args)        -- [[B] A]
            , Array [b, Closure a args] -- [A [B]]
            ]
            (\x -> modifyIORef' stackRef (x :::))
        
        (Cat, Array a ::: Array b ::: r) -> do
          writeIORef stackRef $ Array (a <> b) ::: r

        (Dip, Closure quoted args ::: b ::: r) -> do
          writeIORef stackRef r
          forM_ args $ \arg -> modifyIORef' stackRef (arg :::)
          interpTerm quoted
          modifyIORef' stackRef (b :::)

        (Drop, _ ::: r) -> do
          writeIORef stackRef r
        
        (Dup, a ::: r) -> do
          writeIORef stackRef $ a ::: a ::: r

        (K, Closure a args ::: b ::: r) -> do
          writeIORef stackRef r
          forM_ args $ \arg -> modifyIORef' stackRef (arg :::)
          interpTerm a

        (Swap, a ::: b ::: r) -> do
          writeIORef stackRef $ b ::: a ::: r

        (Placeholder, r) -> pure ()

  interpWord entry
  reverse . toList <$> readIORef stackRef
