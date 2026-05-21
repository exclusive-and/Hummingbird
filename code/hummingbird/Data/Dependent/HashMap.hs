{-# Language UndecidableInstances #-}

module Data.Dependent.HashMap
(
  DHashMap,
  empty,
  singleton,
  null,
  size,
  member,
  lookup,
  lookupDefault,
  insert,
  insertWith,
  -- delete,
  -- adjust,
  -- update,
  alter,
  alterF,
  alterLookup,
  union,
  unionWith,
  unions,
  -- unionsWith,
  map,
  mapWithKey,
  traverse,
  traverseWithKey,
  -- keys,
  -- elems,
) where

import Data.Constraint.Extras
import Data.Dependent.Sum
import Data.Foldable qualified as Foldable
import Data.GADT.Compare
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.Semigroup
import Data.Some
import Data.Type.Equality
import Prelude hiding
  ( empty
  , lookup
  , null
  , map
  , traverse
  , foldMap
  , foldl
  , foldl'
  , foldr
  , filter
  )
import Prelude qualified

newtype DHashMap k v =
  DHashMap (HashMap (Some k) (DSum k v))

deriving newtype instance (GEq k, Has' Eq k v) => Eq (DHashMap k v)
deriving newtype instance (GCompare k, Has' Eq k v, Has' Ord k v) => Ord (DHashMap k v)
deriving newtype instance (GEq k, Hashable (Some k)) => Monoid (DHashMap k v)
deriving newtype instance (GEq k, Hashable (Some k)) => Semigroup (DHashMap k v)

empty :: DHashMap k v
empty =
  DHashMap HashMap.empty

singleton :: (Hashable (Some k)) => k a -> v a -> DHashMap k v
singleton k v =
  DHashMap (HashMap.singleton (Some k) (k :=> v))

null :: DHashMap k v -> Bool
null (DHashMap h) = HashMap.null h

size :: DHashMap k v -> Int
size (DHashMap h) = HashMap.size h

member ::
  (GEq k, Hashable (Some k))
  => k a
  -> DHashMap k v
  -> Bool
member k (DHashMap h) = HashMap.member (Some k) h

dsumOrDefault ::
  (GEq k)
  => r
  -> (v a -> r)
  -> k a
  -> DSum k v
  -> r
dsumOrDefault default_ f k (k' :=> v) =
  case geq k k' of
    Nothing   -> default_
    Just Refl -> f v

lookup ::
  (GEq k, Hashable (Some k))
  => k a
  -> DHashMap k v
  -> Maybe (v a)
lookup k (DHashMap h) =
  dsumOrDefault Nothing Just k =<< HashMap.lookup (Some k) h

lookupDefault ::
  (GEq k, Hashable (Some k))
  => v a
  -> k a
  -> DHashMap k v
  -> v a
lookupDefault default_ k (DHashMap h) =
  dsumOrDefault
    (error "Data.Dependent.HashMap.lookupDefault: key mismatch")
    id
    k
    (HashMap.lookupDefault (k :=> default_) (Some k) h)

insert ::
  (GEq k, Hashable (Some k))
  => k a
  -> v a
  -> DHashMap k v
  -> DHashMap k v
insert k v (DHashMap h) =
  DHashMap (HashMap.insert (Some k) (k :=> v) h)

insertWith ::
  (GEq k, Hashable (Some k))
  => (v a -> v a -> v a)
  -> k a
  -> v a
  -> DHashMap k v
  -> DHashMap k v
insertWith f k v (DHashMap h) =
  DHashMap (HashMap.insertWith go (Some k) (k :=> v) h)
  where
    go (k1 :=> v1) (k2 :=> v2) =
      case (geq k k1, geq k k2) of
        (Just Refl, Just Refl) ->
          k :=> f v1 v2
        _ ->
          error "Data.Dependent.HashMap.insertWith: key mismatch"

alter ::
  (GEq k, Hashable (Some k))
  => (Maybe (v a) -> Maybe (v a))
  -> k a
  -> DHashMap k v
  -> DHashMap k v
alter f k (DHashMap h) =
  DHashMap (HashMap.alter go (Some k) h)
  where
    go Nothing =
      (:=>) k <$> f Nothing
    go (Just (k' :=> v)) =
      case geq k k' of
        Just Refl -> (:=>) k <$> f (Just v)
        Nothing   -> (:=>) k <$> f Nothing

alterF ::
  (Functor f, GEq k, Hashable (Some k))
  => (Maybe (v a) -> f (Maybe (v a)))
  -> k a
  -> DHashMap k v
  -> f (DHashMap k v)
alterF f k (DHashMap h) =
  DHashMap <$> HashMap.alterF go (Some k) h
  where
    go Nothing =
      fmap (k :=>) <$> f Nothing
    go (Just (k' :=> v)) =
      case geq k k' of
        Just Refl -> fmap (k :=>) <$> f (Just v)
        Nothing   -> fmap (k :=>) <$> f Nothing

alterLookup ::
  (GEq k, Hashable (Some k))
  => (Maybe (v a) -> Maybe (v a))
  -> k a
  -> DHashMap k v
  -> (Maybe (v a), DHashMap k v)
alterLookup f = alterF \mv -> (mv, f mv)

union ::
  (GEq k, Hashable (Some k))
  => DHashMap k v
  -> DHashMap k v
  -> DHashMap k v
union = (<>)

unionWith ::
  (GEq k, Hashable (Some k))
  => (forall a. v a -> v a -> v a)
  -> DHashMap k v
  -> DHashMap k v
  -> DHashMap k v
unionWith f (DHashMap h1) (DHashMap h2) =
  DHashMap (HashMap.unionWith go h1 h2)
  where
    go (k1 :=> v1) (k2 :=> v2) =
      case geq k1 k2 of
        Just Refl -> k1 :=> f v1 v2
        Nothing ->
          error "Data.Dependent.HashMap.unionWith: key mismatch"

unions ::
  (GEq k, Hashable (Some k), Foldable f)
  => f (DHashMap k v)
  -> DHashMap k v
unions = Foldable.foldl' union empty

map :: (forall a. v a -> v' a) -> DHashMap k v -> DHashMap k v'
map f (DHashMap h) =
  DHashMap (HashMap.map (\(k :=> v) -> k :=> f v) h)

mapWithKey :: (forall a. k a -> v a -> v' a) -> DHashMap k v -> DHashMap k v'
mapWithKey f (DHashMap h) =
  DHashMap (HashMap.map (\(k :=> v) -> k :=> f k v) h)

traverse ::
  (Applicative f)
  => (forall a. v a -> f (v' a))
  -> DHashMap k v
  -> f (DHashMap k v')
traverse f (DHashMap h) =
  DHashMap <$> Prelude.traverse (\(k :=> v) -> (:=>) k <$> f v) h

traverseWithKey ::
  (Applicative f)
  => (forall a. k a -> v a -> f (v' a))
  -> DHashMap k v
  -> f (DHashMap k v')
traverseWithKey f (DHashMap h) =
  DHashMap <$> Prelude.traverse (\(k :=> v) -> (:=>) k <$> f k v) h
