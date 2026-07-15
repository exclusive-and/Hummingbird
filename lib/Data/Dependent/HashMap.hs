{-# Language PolyKinds #-}
{-# Language UndecidableInstances #-}

module Data.Dependent.HashMap
(
  DHashMap,
  -- * Construction
  empty,
  singleton,
  -- ** From lists
  Data.Dependent.HashMap.fromList,
  -- * Query
  member,
  lookup,
  lookupDefault,
  -- ** Size
  null,
  size,
  -- * Insertion
  insert,
  insertWith,
  -- * Deletion (and other destructive operations)
  delete,
  adjust,
  update,
  alter,
  alterF,
  alterLookup,
  -- * Combinators
  -- ** Union
  union,
  unionWith,
  unions,
  unionsWith,
  -- * Traversal
  map,
  mapWithKey,
  traverse,
  traverseWithKey,
  -- * Conversion
  keys,
  elems,
  -- ** To lists
  Data.Dependent.HashMap.toList,
) where

import Prelude qualified
import Prelude hiding
  ( empty
  , filter
  , foldl
  , foldl'
  , foldMap
  , foldr
  , fromList
  , lookup
  , map
  , null
  , toList
  , traverse
  )

import Data.Constraint.Extras
import Data.Dependent.Sum
import Data.Foldable qualified as Foldable
import Data.GADT.Compare
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.Kind
import Data.Semigroup
import Data.Some
import Data.Type.Equality

-- | @'DHashMap' k v@ is a dependent hash-map datatype:
-- a key with type @k x@ can influence the type @v x@ of the value at that key, via
-- the type parameter @x@.
newtype DHashMap k v = DHashMap (HashMap (Some k) (DSum k v))

deriving instance (GEq k, Has' Eq k v) => Eq (DHashMap k v)
deriving instance (GCompare k, Has' Eq k v, Has' Ord k v) => Ord (DHashMap k v)

deriving newtype instance (GEq k, Hashable (Some k)) => Monoid (DHashMap k v)
deriving newtype instance (GEq k, Hashable (Some k)) => Semigroup (DHashMap k v)

empty :: DHashMap k v
empty = DHashMap HashMap.empty

singleton ::
  (Hashable (Some k))
  => k a
  -> v a
  -> DHashMap k v
singleton k v =
  DHashMap $ HashMap.singleton (Some k) (k :=> v)

fromList ::
  (GEq k, Hashable (Some k))
  => [DSum k v]
  -> DHashMap k v
fromList xs =
  DHashMap $
    HashMap.fromList [ (Some k, kv) | kv@(k :=> _) <- xs ]

member ::
  (GEq k, Hashable (Some k))
  => k a
  -> DHashMap k v
  -> Bool
member k (DHashMap h) = Some k `HashMap.member` h

lookup ::
  (GEq k, Hashable (Some k))
  => k a
  -> DHashMap k v
  -> Maybe (v a)
lookup k (DHashMap h) =
  let
    go (k' :=> v)
      | Just Refl <- geq k k' = Just v
    go _impossible =
      error "Data.Dependent.HashMap.lookup: key mistmatch"
  in
    Some k `HashMap.lookup` h >>= go

lookupDefault ::
  (GEq k, Hashable (Some k))
  => v a
  -> k a
  -> DHashMap k v
  -> v a
lookupDefault default_ k (DHashMap h) =
  let
    go (k' :=> v)
      | Just Refl <- geq k k' = v
    go _impossible =
      error "Data.Dependent.HashMap.lookupDefault: key mismatch"
  in
    go $ HashMap.lookupDefault (k :=> default_) (Some k) h

null :: DHashMap k v -> Bool
null (DHashMap h) = HashMap.null h

size :: DHashMap k v -> Int
size (DHashMap h) = HashMap.size h

insert ::
  (GEq k, Hashable (Some k))
  => k a
  -> v a
  -> DHashMap k v
  -> DHashMap k v
insert k v (DHashMap h) =
  DHashMap $ HashMap.insert (Some k) (k :=> v) h

insertWith ::
  (GEq k, Hashable (Some k))
  => (v a -> v a -> v a)
  -> k a
  -> v a
  -> DHashMap k v
  -> DHashMap k v
insertWith f k v (DHashMap h) =
  let
    go (k1 :=> v1) (k2 :=> v2)
      | Just Refl <- geq k k1
      , Just Refl <- geq k k2 = k :=> f v1 v2
    go _ _impossible =
      error "Data.Dependent.HashMap.insertWith: key mismatch"
  in
    DHashMap $ HashMap.insertWith go (Some k) (k :=> v) h

delete ::
  (GEq k, Hashable (Some k))
  => k a
  -> DHashMap k v
  -> DHashMap k v
delete k (DHashMap h) =
  DHashMap $ HashMap.delete (Some k) h

adjust ::
  (GEq k, Hashable (Some k))
  => (v a -> v a)
  -> k a
  -> DHashMap k v
  -> DHashMap k v
adjust f k (DHashMap h) =
  let
    go (k' :=> v)
      | Just Refl <- geq k k' = k :=> f v
    go _impossible =
      error "Data.Dependent.HashMap.adjust: key mismatch"
  in
    DHashMap $ HashMap.adjust go (Some k) h

update ::
  (GEq k, Hashable (Some k))
  => (v a -> Maybe (v a))
  -> k a
  -> DHashMap k v
  -> DHashMap k v
update f k (DHashMap h) =
  let
    go (k' :=> v)
      | Just Refl <- geq k k' = (k :=>) <$> f v
    go _impossible =
      error "Data.Dependent.HashMap.update: key mismatch"
  in
    DHashMap $ HashMap.update go (Some k) h

alter ::
  (GEq k, Hashable (Some k))
  => (Maybe (v a) -> Maybe (v a))
  -> k a
  -> DHashMap k v
  -> DHashMap k v
alter f k (DHashMap h) =
  let
    go Nothing = (:=>) k <$> f Nothing
    go (Just (k' :=> v))
      | Just Refl <- geq k k' = (k :=>) <$> f (Just v)
      | otherwise             = (k :=>) <$> f Nothing
  in
    DHashMap $ HashMap.alter go (Some k) h

alterF ::
  (GEq k, Hashable (Some k))
  => (Functor f)
  => (Maybe (v a) -> f (Maybe (v a)))
  -> k a
  -> DHashMap k v
  -> f (DHashMap k v)
alterF f k (DHashMap h) =
  let
    go Nothing = fmap (k :=>) <$> f Nothing
    go (Just (k' :=> v))
      | Just Refl <- geq k k' = fmap (k :=>) <$> f (Just v)
      | otherwise             = fmap (k :=>) <$> f Nothing
  in
    DHashMap <$> HashMap.alterF go (Some k) h

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
  let
    go (k1 :=> v1) (k2 :=> v2)
      | Just Refl <- geq k1 k2 = k1 :=> f v1 v2
    go _ _impossible =
      error "Data.Dependent.HashMap.unionWith: key mismatch"
  in
    DHashMap $ HashMap.unionWith go h1 h2

unions ::
  (GEq k, Hashable (Some k))
  => (Foldable f)
  => f (DHashMap k v)
  -> DHashMap k v
unions = Foldable.foldl' union empty

unionsWith ::
  (GEq k, Hashable (Some k))
  => (Foldable f)
  => (forall a. v a -> v a -> v a)
  -> f (DHashMap k v)
  -> DHashMap k v
unionsWith f = Foldable.foldl' (unionWith f) empty

map ::
  (forall a. v a -> v' a)
  -> DHashMap k v
  -> DHashMap k v'
map f (DHashMap h) =
  DHashMap $ HashMap.map (\(k :=> v) -> k :=> f v) h

mapWithKey ::
  (forall a. k a -> v a -> v' a)
  -> DHashMap k v
  -> DHashMap k v'
mapWithKey f (DHashMap h) =
  DHashMap $ HashMap.map (\(k :=> v) -> k :=> f k v) h

traverse ::
  (Applicative f)
  => (forall a. v a -> f (v' a))
  -> DHashMap k v
  -> f (DHashMap k v')
traverse f (DHashMap h) =
  DHashMap <$>
    Prelude.traverse (\(k :=> v) -> (k :=>) <$> f v) h

traverseWithKey ::
  (Applicative f)
  => (forall a. k a -> v a -> f (v' a))
  -> DHashMap k v
  -> f (DHashMap k v')
traverseWithKey f (DHashMap h) =
  DHashMap <$>
    Prelude.traverse (\(k :=> v) -> (k :=>) <$> f k v) h

keys :: DHashMap k v -> [Some k]
keys (DHashMap h) = HashMap.keys h

elems :: DHashMap k v -> [Some v]
elems (DHashMap h) = [ Some v | _ :=> v <- HashMap.elems h ]

toList :: DHashMap k v -> [DSum k v]
toList (DHashMap h) = HashMap.elems h
