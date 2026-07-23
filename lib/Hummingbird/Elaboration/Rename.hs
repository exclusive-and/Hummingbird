module Hummingbird.Elaboration.Rename where

import Prelude
import Prettyprinter
import Prettyprinter.Render.Text

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Primitive
import Control.Monad.Reader
import Data.Binary
import Data.Foldable
import Data.Graph qualified as Graph
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Primitive.MutVar
import Data.Primitive.MutVar.Extra
import Data.Semigroup
import Data.Set (Set)
import Data.Set qualified as Set
import Data.These
import Data.Traversable
import Data.Typeable

import Hummingbird.Builtin as Builtin
import Hummingbird.Codebase
import Hummingbird.Codebase.Id as Codebase
import Hummingbird.Error
import Hummingbird.Fetch
import Hummingbird.Name as Name
import Hummingbird.Query (Query)
import Hummingbird.Surface as Surface

{-
Note [Renamer plan]
~~~~~~~~~~~~~~~~~~~
Here's a sketch that outlines the rough plan for the renamer:

  Name := Term Name
    ->
  Int := Term OpenVar
    ->
  SCC of Int := Term ClosedVar
    ->
  SCC of Int := Term ResolvedVar
    ->
  SCC of Codebase.Id := Term ResolvedVar
-}

-- | An 'OpenVar' may be a reference to a definition that hasn't
-- been hashed yet.
data OpenVar
  = HashOV !Codebase.Id
  | HoleOV
  | LocalOV !Int
  | GlobalOV !Int
  deriving
    ( Eq
    , Generic
    , Ord
    , Show
    )
  deriving anyclass (Binary, Hashable)

-- | A 'ClosedVar' references either an existing hashed definition
-- or the SCC of its own definition.
data ClosedVar
  = HashCV !Codebase.Id
  | LocalCV !Int
  | CycleBreakCV
  deriving
    ( Eq
    , Generic
    , Ord
    , Show
    )
  deriving anyclass (Binary, Hashable)

-- | A 'ResolvedVar' is a stable reference to a fully-resolved definition.
data ResolvedVar
  = HashV !Codebase.Id
  | LocalV !Int
  deriving
    ( Eq
    , Generic
    , Ord
    , Show
    )
  deriving anyclass (Binary, ContentAddress, Hashable)

type FreeVars = Set Int

type SourceTerm   = Term Name
type OpenTerm     = Term OpenVar
type ClosedTerm   = Term ClosedVar
type ResolvedTerm = Term ResolvedVar

data RnEnv =
  RnEnv
  { rnNextLocalId   :: !Int
  , rnNextGlobalId  :: !Int
  , rnHashEnv       :: Map Name Id
  , rnLocalEnv      :: Map Name Int
  , rnGlobalEnv     :: Map Name Int
  , rnOpenEnv       :: Map Name OpenVar
  , rnResolvedEnv   :: Map Name ResolvedVar
  }

data RnCtx =
  RnCtx
  { rnCtxEnv    :: !(MutVar RealWorld RnEnv)
  , rnCtxErrors :: !(MutVar RealWorld [Error])
  }

initRnEnv :: Map Name Id -> RnEnv
initRnEnv idEnv = RnEnv{..}
  where
    rnNextLocalId = 0
    rnNextGlobalId = 0
    rnHashEnv = idEnv
    rnLocalEnv = mempty
    rnGlobalEnv = mempty
    rnOpenEnv = Map.map HashOV idEnv
    rnResolvedEnv = Map.map HashV idEnv

initRnCtx :: Map Name Id -> Task Query IO RnCtx
initRnCtx idEnv = do
  rnCtxEnv <- newMutVar (initRnEnv idEnv)
  rnCtxErrors <- newMutVar []
  pure RnCtx{..}

newtype RnM a = RnM (RnCtx -> Task Query IO a)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadFetch Query
    , MonadThrow
    ) via (ReaderT RnCtx (Task Query IO))

instance MonadReader RnEnv RnM where
  ask =
    RnM $ readMutVar . rnCtxEnv

  local f (RnM act) = RnM \RnCtx{..} -> do
    env <- readMutVar rnCtxEnv
    env' <- newMutVar (f env)
    act RnCtx{rnCtxEnv = env', ..}

rnReport :: [Error] -> RnM ()
rnReport msgs =
  RnM \RnCtx{..} ->
    atomicModifyMutVar_ rnCtxErrors (++ msgs)

{-
rnExtendVarEnv :: Map Name Var -> RnM (Map Name Var)
rnExtendVarEnv newEnv =
  RnM \env ->
    pure $ rnVarEnv env <> newEnv

rnWithVarEnv :: Map Name Var -> RnM a -> RnM a
rnWithVarEnv newEnv (RnM inner) =
  RnM \env ->
    inner (env { rnVarEnv = newEnv })
-}

rnUnboundOpenVar :: Name -> RnM OpenVar
rnUnboundOpenVar name =
  HoleOV <$ rnReport [Elaboration $ NotInScope name]

rnNameToResolvedVar :: Name -> RnM (Maybe ResolvedVar)
rnNameToResolvedVar name =
  asks (Map.lookup name . rnResolvedEnv)

rnNameToOpenVar :: Name -> RnM (OpenVar, FreeVars)
rnNameToOpenVar name = do
  mbResolved <- rnNameToResolvedVar name
  case mbResolved of
    Just (LocalV lcl) -> pure (LocalOV lcl, Set.empty)
    Just (HashV hashId) -> pure (HashOV hashId, Set.empty)
    Nothing -> do
      varEnv <- asks rnOpenEnv
      case Map.lookup name varEnv of
        Just var -> pure (var, Set.singleton var)
        Nothing  -> (, Set.empty) <$> rnUnboundOpenVar name

rnTerm :: SourceTerm -> RnM (OpenTerm, FreeVars)
rnTerm (Surface.Lit lit) =
  pure (Surface.Lit lit, Set.empty)
rnTerm (Surface.Word name) = undefined
rnTerm (Surface.Lambda bndr body) =
  error "Hummingbird.Elaboration.Hash.rnTerm: no support for lambdas yet"
rnTerm (Surface.Match alts) =
  bimap Surface.Match Set.unions . unzip <$> traverse rnAlt alts
rnTerm (Surface.Quoted quoted) =
  first Surface.Quoted <$> rnTerm quoted
rnTerm (Surface.Concat terms) =
  bimap Surface.Concat Set.unions . unzip <$> mapM rnTerm terms

rnAlt :: Alt Name -> RnM (Alt OpenVar, FreeVars)
rnAlt = error "Hummingbird.Elaboration.Hash.rnAlt: not yet implemented"

rnGetFreshOV :: Name -> RnM Int
rnGetFreshOV name =
  RnM \RnCtx{..} ->
    atomicModifyMutVar rnCtxEnv \RnEnv{..} ->
      (RnEnv{rnNextGlobalId = rnNextGlobalId + 1, ..}, rnNextGlobalId)

rnLhs :: (Name, a) -> RnM (Int, a)
rnLhs (name, body) =
  (, body) <$> rnGetFreshOV name

rnRhs :: (Int, SourceTerm) -> RnM ((Int, OpenTerm), FreeVars)
rnRhs (var, body) = do
  (body', fvs) <- rnTerm body
  pure ((var, body'), fvs)

rnBinds :: [(Name, SourceTerm)] -> RnM [((Int, OpenTerm), FreeVars)]
rnBinds binds = do
  freshened <- mapM (\(name, body) -> first (name,) <$> rnLhs (name, body)) binds
  let
    newIds = Map.fromList
      [ (name, GlobalOV fv) | ((name, fv), _) <- freshened ]
  varEnv' <- rnExtendVarEnv newIds
  rnWithVarEnv varEnv' $ mapM rnRhs freshened

detectCycles :: (Ord k) => [((k, a), Set k)] -> [Graph.SCC (k, a) ()]
detectCycles binds =
  let
    graph =
      Graph.fromKeyMap $
        Map.fromList [ (k, (a, Set.toList fvs)) | ((k, a), fvs) <- binds ]
  in
    Graph.sccs_ graph

caHashCycle ::
  (ContentAddress ClosedTerm)
  => [(k, ClosedTerm)]
  -> (Hash, [ClosedTerm])
caHashCycle binds =
  error "Hummingbird.Elaboration.ContentAddress.caHashCycle: not yet implemented"

{-
rnVarsToIds :: Map Int Id -> Term Var -> Term Var
rnVarsToIds idMap = go
  where
    go (Surface.Lit lit) = Lit lit
    go (Surface.Word (FV (RnId name i))) =
      case Map.lookup i idMap of
        Just caId -> Word $ CAId caId
        Nothing   -> Word $ FV (RnId name i)
    go (Surface.Word word) = Word word
    go (Surface.Lambda bndr body) =
      error "Hummingbird.Elaboration.Hash.rnVarsToIds: no support for lambdas yet"
    go (Surface.Match alts) =
      error "Hummingbird.Elaboration.Hash.rnVarsToIds: no support for match expressions yet"
    go (Surface.Quoted quoted) = Quoted $ go quoted
    go (Surface.Concat terms) = Concat $ go <$> terms
-}

runRnM :: RnM a -> Map Name Id -> Task Query IO (These [Error] a)
runRnM (RnM m) idEnv = do
  ctx <- initRnCtx idEnv
  result <- m ctx
  errors <- readMutVar (rnCtxErrors ctx)
  case errors of
    [] -> pure $ That result
    _  -> pure $ These errors result

rnTermsTask ::
  Map Name Id
  -> [(Maybe Name, Term Name)]
  -> Task Query IO (CodePatch Renamed)
rnTermsTask env tms =
  error "Hummingbird.Elaboration.Hash.rnTermsTask: not yet implemented"

renameDeclsTask ::
  Map Name Id
  -> [Declaration Name]
  -> Task Query IO (CodePatch Renamed)

renameDeclsTask env decls = do
  let
    defns = [(name, body) | Fun name body <- decls]
  (errors, renamed) <-
    runRnM (rnBinds defns) env
      & fmap (fromThese [] [])
  let
    graph =
      Graph.fromKeyMap $
        Map.fromList [ (k, (a, Set.toList fvs)) | ((k, a), fvs) <- renamed ]
    sccs = Graph.sccs_ graph
  pure $ AddHashedTerms
    (RawMessage (renderStrict $ layoutPretty defaultLayoutOptions $ pretty sccs) : errors)
    Map.empty
    Set.empty
    Map.empty
    Map.empty

renameExprTask ::
  Map Name Id
  -> Term Name
  -> Task Query IO (CodePatch Renamed)

renameExprTask env expr = do
  (errors, renamed) <- fromThese [] [] <$> runRnM (mapM rnTerm [expr]) env
  let
    hashed = [(contentHash tm, tm) | (tm, _) <- renamed]
  pure $ AddHashedTerms
    errors
    Map.empty
    (Set.fromList $ fst <$> hashed)
    (Map.fromList hashed)
    Map.empty
