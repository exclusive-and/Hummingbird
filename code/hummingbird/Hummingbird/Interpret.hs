module Hummingbird.Interpret where

import Birds.Prelude

import Control.Monad.Trans
import Control.Monad.State
import Data.Foldable
import Data.IntMap qualified
import Data.Map (Map)
import Data.Map qualified

import Hummingbird.Builtin
import Hummingbird.Rename
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var
import Hummingbird.VarMap (VarMap)
import Hummingbird.VarMap qualified as VarMap

type InterpretM = StateT (VarMap (Surface.Term Var)) (Either Text)

interpret ::
  Var
  -> [(Var, Surface.Term Var)]
  -> Either Text [Surface.Term Var]

interpret entry defns =
  let
    go ::
      Surface.Term Var
      -> [Surface.Term Var]
      -> InterpretM [Surface.Term Var]

    go (Surface.Word var) stk =
      case var of
        Prim Cat -> cat var stk
        Prim Apply -> apply var stk
        Prim Dip -> dip var stk
        Prim Swap -> swap var stk
        Prim Dup -> dup var stk
        Prim Drop -> drop var stk
        Prim K -> k var stk
        Prim Cake -> cake var stk
        Prim Placeholder -> pure stk
        _ -> do
          defmb <- def var
          case defmb of
            Nothing -> pure $ Surface.Word var : stk
            Just defn -> go defn stk

    go (Surface.Lambda b body) (arg : stk) =
      flip go stk =<< subst (b, arg) body

    go (Surface.Quoted expr) stk =
      pure $ expr : stk

    go (Surface.Concat exprs) stk = foldlM (flip go) stk exprs

    go expr stk =
      pure $ expr : stk

    subst ::
      (Var, Surface.Term Var)
      -> Surface.Term Var
      -> InterpretM (Surface.Term Var)

    subst (b, arg) body =
      case body of
        Surface.Word v | b == v -> pure arg
        Surface.Lambda b' body' | b /= b' -> Surface.Lambda b' <$> subst (b, arg) body'
        Surface.Quoted expr -> Surface.Quoted <$> subst (b, arg) expr
        Surface.Concat exprs -> Surface.Concat <$> traverse (subst (b, arg)) exprs
        _ -> pure body

    def :: Var -> InterpretM (Maybe (Surface.Term Var))
    def var = gets (VarMap.lookup var)

    cat _ (x:y:stk) = pure $ (y <> x) : stk
    cat v stk       = pure $ Surface.Word v : stk

    apply _ (x:stk) = go x stk
    apply v stk     = pure $ Surface.Word v : stk

    dip _ (x:y:stk) = (y:) <$> go x stk
    dip v stk       = pure $ Surface.Word v : stk

    swap _ (x:y:stk) = pure $ y : x : stk
    swap v stk       = pure $ Surface.Word v : stk

    dup _ (x:stk) = pure $ x : x : stk
    dup v stk     = pure $ Surface.Word v : stk

    drop _ (_:stk)  = pure stk
    drop v stk      = pure $ Surface.Word v : stk

    k _ (x:y:stk) = go x stk
    k v stk       = pure $ Surface.Word v : stk

    cake _ (x:y:stk)  = pure $ (x <> Surface.Quoted y) : (Surface.Quoted y <> x) : stk
    cake v stk        = pure $ Surface.Word v : stk

    defnMap = VarMap.fromList $ map (\(bndr, body) -> (bndr, body)) defns
  in
    reverse <$> evalStateT (go (Surface.Word entry) []) defnMap
