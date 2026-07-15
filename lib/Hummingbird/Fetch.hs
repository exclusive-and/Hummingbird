{-# Language PolyKinds #-}
{-# Language UndecidableInstances #-}

module Hummingbird.Fetch where

import Prelude

import Control.Concurrent qualified
import Control.Monad
import Control.Monad.Accum
import Control.Monad.Catch
import Control.Monad.Chronicle
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.RWS.Strict qualified
import Control.Monad.State
import Control.Monad.State.Strict qualified
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Writer (WriterT)
import Control.Monad.Writer.Strict qualified
import Data.Dependent.HashMap (DHashMap)
import Data.Dependent.HashMap qualified as DHashMap
import Data.GADT.Compare
import Data.GADT.Show (GShow)
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Primitive.Concurrent
import Data.Primitive.MutVar
import Data.Primitive.MutVar.Extra (atomicModifyMutVar_)
import Data.Primitive.MVar as MVar
import Data.Primitive.MVar.Extra as MVar
import Data.Some
import Data.Typeable

-- | Monads that can 'fetch' answers to queries from a query type @q@.
class (Monad m) => MonadFetch q m | m -> q where
  fetch :: q a -> m a
  fetch = lift . fetch

  default fetch ::
    (MonadTrans t, MonadFetch q n, m ~ t n)
    => q a
    -> m a

instance (MonadFetch q m, Semigroup c) => MonadFetch q (ChronicleT c m)
instance (MonadFetch q m) => MonadFetch q (ContT r m)
instance (MonadFetch q m) => MonadFetch q (ExceptT e m)
instance (MonadFetch q m) => MonadFetch q (IdentityT m)
instance (MonadFetch q m) => MonadFetch q (MaybeT m)
instance (MonadFetch q m) => MonadFetch q (ReaderT r m)
instance (MonadFetch q m, Monoid w) => MonadFetch q (RWST r w s m)
instance (MonadFetch q m) => MonadFetch q (StateT s m)
instance (MonadFetch q m, Monoid w) => MonadFetch q (WriterT w m)

instance
  (MonadFetch q m, Monoid w)
  => MonadFetch q (Control.Monad.RWS.Strict.RWST r w s m)

instance
  (MonadFetch q m)
  => MonadFetch q (Control.Monad.State.Strict.StateT s m)

instance
  (MonadFetch q m, Monoid w)
  => MonadFetch q (Control.Monad.Writer.Strict.WriterT w m)

newtype Fetch q m = Fetch (forall a. q a -> m a)

-- | Computes an answer @a@. May ask itself @q@-questions to compute the answer.
newtype Task q m a = Task {
    unTask :: ReaderT (Fetch q m) m a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadCont
    , MonadFail
    , MonadFix
    , MonadIO
    , MonadMask
    , MonadThrow
    , PrimMonad
    )

instance (Monad m) => MonadFetch q (Task q m) where
  fetch key = Task $
    asks (\(Fetch fetch_) -> fetch_ key) >>= lift

instance MonadTrans (Task q) where
  lift = Task . lift

instance (MonadReader r m) => MonadReader r (Task q m) where
  ask = lift ask
  local f (Task t) = Task $ mapReaderT (local f) t

deriving newtype instance (MonadAccum w m) => MonadAccum w (Task q m)
deriving newtype instance (MonadChronicle c m) => MonadChronicle c (Task q m)
deriving newtype instance (MonadError e m) => MonadError e (Task q m)
deriving newtype instance (MonadRWS r w s m) => MonadRWS r w s (Task q m)
deriving newtype instance (MonadState s m) => MonadState s (Task q m)
deriving newtype instance (MonadWriter w m) => MonadWriter w (Task q m)

-- | Hoist @p@-question 'Task's into @q@-question 'Task's via a natural transformation on
-- questions.
hoistQuery ::
  (forall b. p b -> Task q m b)
  -> Task p m a
  -> Task q m a
hoistQuery f (Task task) =
  Task $ ReaderT \fetch_ ->
    runReaderT task $ Fetch \key ->
      runReaderT (unTask $ f key) fetch_

-- |
data Writer w f a where
  Writer :: f a -> Writer w f (a , w)

-- |
writer ::
  (Monad m)
  => (forall a. p a -> w -> Task q m ())
  -> GenRules m (Writer w p) q
  -> GenRules m p q
writer write (GenRules rules) =
  GenRules \key -> do
    (result, w) <- rules $ Writer key
    write key w
    pure result

-- | Inference rules for questions in @q@.
type Rules m q = GenRules m q q

-- | Inference rules for @p@-questions that can depend on @q@-questions.
newtype GenRules m p q = GenRules (forall a. p a -> Task q m a)

runTask :: Rules m q -> Task q m a -> m a
runTask (GenRules rules) (Task task) =
  runReaderT task $ Fetch $ runTask (GenRules rules) . rules

type Memoiseable p =
  ( Typeable p
  , GEq p
  , GShow p
  , Hashable (Some p)
  )

type MonadMemoise p s m = (Memoiseable p, MonadMask m, MonadPrim s m)

memoise ::
  (MonadMemoise p s m)
  => MutVar s (DHashMap p (MVar s))
  -> GenRules m p q
  -> GenRules m p q

memoise memoCtxVar (GenRules rules) =
  GenRules \key ->
    do
    memoCtx <- readMutVar memoCtxVar
    case key `DHashMap.lookup` memoCtx of
      Just memoVar -> readMVar memoVar
      Nothing -> join do
        freshMemoVar <- newEmptyMVar
        let
          launch = do
            value <- rules key
            putMVar freshMemoVar value
            pure value
        let
          recall Nothing =
            (launch, Just freshMemoVar)
          recall (Just memoVar) =
            (readMVar memoVar, Just memoVar)
        atomicModifyMutVar memoCtxVar $ swap . DHashMap.alterF recall key

newtype Cyclic (f :: Type -> Type) = Cyclic (Some f)
  deriving
    ( Eq
    , Generic
    , Show
    , Typeable
    )

instance (GShow f, Typeable f) => Exception (Cyclic f)

data MemoEntry a
  = Done a
  | Started !Control.Concurrent.ThreadId
            !(MVar RealWorld (Maybe a))
            !(MVar RealWorld (Maybe [Control.Concurrent.ThreadId]))

-- | 'detectMemoCycle' tests whether a thread is blocked by itself, including
-- transitive dependencies. The second argument represents the blocked
-- status of each live thread as follows:
--
--  * @'Nothing'@: the thread isn't blocked. It can't be in a cycle.
--
--  * @'Just' depId@: the thread is blocked by another thread with ID @depId@.
detectMemoCycle ::
  Control.Concurrent.ThreadId
  -> HashMap
      Control.Concurrent.ThreadId
      Control.Concurrent.ThreadId
  -> Bool

detectMemoCycle threadId deps =
  let
    go tid =
      case tid `HashMap.lookup` deps of
        Nothing -> False
        Just depId
          | depId == threadId -> True
          | otherwise         -> go depId
  in
    go threadId

memoiseWithCycleDetection ::
  forall p m q.
  (MonadMemoise p RealWorld m)
  => MutVar RealWorld (DHashMap p MemoEntry)
  -> MutVar RealWorld
      ( HashMap
          Control.Concurrent.ThreadId
          Control.Concurrent.ThreadId
      )
  -> GenRules m p q
  -> GenRules m p q

memoiseWithCycleDetection memoCtxVar depsVar (GenRules rules) =
  GenRules $ fix \mwcd (key :: p a) -> do
    let
      getMemo :: MemoEntry a -> Task q m a
      getMemo (Done a) = pure a
      getMemo (Started hostThreadId memoVar stbyVar) = do
        threadId <- getThreadId
        let
          checkNoCycles deps =
            if detectMemoCycle threadId deps' then
              -- The host thread says it depends on me! We're stuck!
              ( deps
              , throwM (Cyclic $ Some key)
              )
            else
              (deps', pure ())
            where
              deps' = HashMap.insert threadId hostThreadId deps
        modifyMVar_ stbyVar \case
          Nothing -> pure Nothing
          Just stbyThreads ->
            Just (threadId : stbyThreads)
              <$ join (atomicModifyMutVar depsVar checkNoCycles)
        readMVar memoVar >>= maybe (mwcd key) pure

    memoCtx <- readMutVar memoCtxVar
    case key `DHashMap.lookup` memoCtx of
      -- I already have a MemoEntry for `key`: get the memoised result data.
      Just entry -> getMemo entry
      -- I haven't seen `key` before:
      --  1. create a new MemoEntry for the query;
      --  2. call `rules` on the query to process it in this thread.
      Nothing -> join do
        threadId <- getThreadId
        memoVar <- newEmptyMVar
        stbyVar <- newMVar (Just [])
        let
          cleanup Nothing =
            error "Hummingbird.Fetch.memoiseWithCycleDetection: something impossible happened"
          cleanup (Just stbyThreads) =
            pure
              ( Nothing
              , atomicModifyMutVar_ depsVar \deps ->
                  foldl' (flip HashMap.delete) deps stbyThreads
              )
        let
          launch = do
            value <- rules key
            -- The query is finished: I can remove my thread from the record
            -- of dependencies now.
            join $ modifyMVar stbyVar cleanup
            -- putMVar wakes any client threads that were blocked on this query.
            putMVar memoVar (Just value)
            atomicModifyMutVar memoCtxVar $
              (, value) . DHashMap.insert key (Done value)
        let
          recall (Just entry) =
            (getMemo entry, Just entry)
          recall Nothing =
            (launch, Just $ Started threadId memoVar stbyVar)
        atomicModifyMutVar memoCtxVar $ swap . DHashMap.alterF recall key

