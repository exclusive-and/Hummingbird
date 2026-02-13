{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language StandaloneDeriving #-}

module Hummingbird.Rename
(
  -- * Renaming
  renameVar,
  renameTerm,
  renameAlt,
  renameBinds,
  renameLhs,
  renameRhs,
  RenameMessage (..),

  -- * Creating variables
  RnMap,
  freshen,
  freshenNoShadowing,
  withFresh,
  withFreshNoShadowing,

  -- * Rename monad
  RenameM,
  runRename,
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Control.Monad.Chronicle
import Control.Monad.State
import Control.Monad.Trans
import Data.Foldable
import Data.Semigroup
import Data.These
import Data.Traversable
import Prettyprinter (Pretty (pretty))
import Prettyprinter qualified as Pretty

import Hummingbird.Builtin
import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name
import Hummingbird.Prelude
import Hummingbird.Surface
import Hummingbird.Var
import Hummingbird.VarMap (VarMap)
import Hummingbird.VarMap qualified as VarMap

renameTop :: RnMap -> Module Name -> RenameM (Module Var)
renameTop inScope (Module modName decls) = do
  undefined

-- |
renameVar :: RnMap -> Name -> RenameM Var
renameVar inScope name =
  case Map.lookup name inScope of
    Just term ->
      pure term
    Nothing ->
      confess $ NotInScope name

-- |
data RenameMessage
  = NotInScope Name
  | Ambiguous Name Var
  | ManyRnMessages [RenameMessage]
  deriving (Eq, Show)

instance Pretty RenameMessage where
  pretty = \case
    NotInScope name ->
      Pretty.hsep ["Not in scope:", pretty name]
    Ambiguous name var ->
      Pretty.hsep ["Ambiguous:", pretty name, pretty var]
    ManyRnMessages msgs ->
      Pretty.vcat (map pretty msgs)

instance Semigroup RenameMessage where
  a <> b = catMessages [a, b]
  sconcat = catMessages . toList

-- |
catMessages :: [RenameMessage] -> RenameMessage
catMessages = \case
  [msg] -> msg
  msgs  -> ManyRnMessages $ concatMap go msgs
  where
    go = \case
      ManyRnMessages xs -> xs
      other             -> [other]

-- |
type RnMap = Map Name Var

type InScope = Map Name Var

-- |
renameTerm :: RnMap -> Term Name -> RenameM (Term Var)
renameTerm inScope = \case
  Word name ->
    Word <$> renameVar inScope name

  Lit literal ->
    pure $ Lit literal

  Lambda bndr body ->
    withBound bndr inScope
      (\bndr' inScope' -> Lambda bndr' <$> renameTerm inScope' body)
  
  Match alts ->
    Match <$> mapM (renameAlt inScope) alts

  Quoted quoted ->
    Quoted <$> renameTerm inScope quoted
  
  Concat terms ->
    Concat <$> mapM (renameTerm inScope) terms

-- |
renameAlt :: InScope -> Alt Name -> RenameM (Alt Var)
renameAlt = error "renameAlt: not yet implemented"

-- |
renameBinds :: RnMap -> [(Name, Term Name)] -> RenameM (RnMap, [(Var, Term Var)])
renameBinds inScope binds = do
  (inScope', freshened) <- mapAccumM renameLhs inScope binds
  renamed <- mapM (renameRhs inScope') freshened
  pure (inScope', renamed)
    
-- |
renameLhs :: RnMap -> (Name, a) -> RenameM (RnMap, (Var, a))
renameLhs inScope (bndr, body) =
  withFreshNoShadowing bndr inScope
    (\bndr' inScope' -> pure (inScope', (bndr', body)))

-- |
renameRhs :: RnMap -> (Var, Term Name) -> RenameM (Var, Term Var)
renameRhs inScope (bndr, body) =
  (,) bndr <$> renameTerm inScope body

-- |
freshen :: Name -> RenameM Var
freshen name = Var name <$> createFreshId

-- |
withFresh :: Name -> RnMap -> (Var -> RnMap -> RenameM a) -> RenameM a
withFresh name inScope k = do
  var <- freshen name
  k var (Map.insert name var inScope)

-- |
withBound :: Name -> RnMap -> (Var -> RnMap -> RenameM a) -> RenameM a
withBound name inScope k = do
  var <- freshen name
  k var (Map.insert name var inScope)

-- |
freshenNoShadowing :: Name -> RnMap -> RenameM Var
freshenNoShadowing name inScope =
  case Map.lookup name inScope of
    Nothing ->
      freshen name
    Just other ->
      confess $ Ambiguous name other

-- |
withFreshNoShadowing :: Name -> RnMap -> (Var -> RnMap -> RenameM a) -> RenameM a
withFreshNoShadowing name inScope k = do
  var <- freshenNoShadowing name inScope
  k var (Map.insert name var inScope)

-- |
newtype RenameM a = RenameM {
    unRename :: ChronicleT RenameMessage (State RenameState) a
  }
  deriving (Functor, Applicative, Monad)

deriving instance MonadChronicle RenameMessage RenameM

-- |
runRename :: InScope -> (InScope -> RenameM a) -> These RenameMessage a
runRename inScope k =
  evalState
    (runChronicleT $ unRename $ k inScope)
    (initRnState $ length inScope)

createFreshId :: RenameM Int
createFreshId = do
  i <- getFreshId
  modifyFreshId (+ 1)
  pure i

data RenameState = RenameState {
    freshId :: Int
  }

getRnState :: RenameM RenameState
getRnState = RenameM (lift get)

getFreshId :: RenameM Int
getFreshId = freshId <$> getRnState

modifyFreshId :: (Int -> Int) -> RenameM ()
modifyFreshId f = modifyRnState $ \s -> s { freshId = f $ freshId s }

modifyRnState :: (RenameState -> RenameState) -> RenameM ()
modifyRnState f = RenameM (lift $ modify f)

initRnState :: Int -> RenameState
initRnState freshId =
  let
  in
    RenameState{..}
