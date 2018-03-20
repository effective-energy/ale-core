{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module wraps standard 'Data.HashMap.Strict'.  This is done
-- to ensure that serialisation is deterministic: we need to be sure
-- that final value 'HashMap' serialise to does not depend on order of
-- modification operations.
module Ale.Data.HashMap
       ( HashMap (HashMap)

       , empty
       , singleton

       , lookup
       , lookupDefault
       , member
       , insert
       , insertWith
       , delete
       , adjust
       , alter

       , foldrWithKey

       , filter
       , mapMaybe
       , map

       , fromList

       ) where

import Universum hiding (HashMap, empty, filter, map, mapMaybe)

import Codec.Serialise (Serialise (decode, encode))
import Control.Lens (At (at), Index, IxValue, Ixed (ix), iso)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Exts (IsList)

import Ale.Fmt (Buildable (..), jsonMapF)

import qualified Data.HashMap.Strict as HM
import qualified GHC.Exts

newtype HashMap k v = HashMap { getHashMap :: HM.HashMap k v }
    deriving (Eq, Show, Functor, Foldable, Traversable, Semigroup, Monoid, Hashable,
              ToJSON, FromJSON, ToPairs)

instance (Eq k, Hashable k) => IsList (HashMap k v) where
    type Item (HashMap k v) = (k, v)
    fromList = Ale.Data.HashMap.fromList
    toList = toPairs

instance (Eq k, Hashable k) => Ixed (HashMap k v) where
    ix i = iso getHashMap HashMap . ix i

instance (Eq k, Hashable k) => At (HashMap k v) where
    at i = iso getHashMap HashMap . at i

instance (Eq a, Hashable a, Buildable a, Buildable b) => Buildable (HashMap a b) where
    build (HashMap hm) = jsonMapF hm

type instance Index (HashMap k v) = k
type instance IxValue (HashMap k v) = v

empty :: HashMap k v
empty = HashMap HM.empty

singleton :: (Hashable k) => k -> v -> HashMap k v
singleton k = HashMap . HM.singleton k

lookup :: (Hashable k, Eq k) => k -> HashMap k v -> Maybe v
lookup k = HM.lookup k . getHashMap

lookupDefault :: (Hashable k, Eq k) => v -> k -> HashMap k v -> v
lookupDefault v k = HM.lookupDefault v k . getHashMap

member :: (Hashable k, Eq k) => k -> HashMap k v -> Bool
member k = HM.member k . getHashMap

insert :: (Hashable k, Eq k) => k -> v -> HashMap k v -> HashMap k v
insert k v = HashMap . HM.insert k v . getHashMap

insertWith :: (Hashable k, Eq k) => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
insertWith f k v = HashMap . HM.insertWith f k v . getHashMap

delete :: (Hashable k, Eq k) => k -> HashMap k v -> HashMap k v
delete k = HashMap . HM.delete k . getHashMap

adjust :: (Hashable k, Eq k) => (v -> v) -> k -> HashMap k v -> HashMap k v
adjust f k = HashMap . HM.adjust f k . getHashMap

alter :: (Hashable k, Eq k) => (Maybe v -> Maybe v) -> k -> HashMap k v -> HashMap k v
alter f k = HashMap . HM.alter f k . getHashMap

foldrWithKey :: (k -> v -> a -> a) -> a -> HashMap k v -> a
foldrWithKey f a = HM.foldrWithKey f a . getHashMap

filter :: (v -> Bool) -> HashMap k v -> HashMap k v
filter p = HashMap . HM.filter p . getHashMap

mapMaybe :: (v1 -> Maybe v2) -> HashMap k v1 -> HashMap k v2
mapMaybe f = HashMap . HM.mapMaybe f . getHashMap

map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
map f = HashMap . HM.map f . getHashMap

fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
fromList = HashMap . HM.fromList

-- | Serialise a 'HashMap' in a deterministic order for future hashing.
instance (Serialise k, Serialise v, Hashable k, Ord k) => Serialise (HashMap k v) where
    encode = encode . sortOn fst . HM.toList . getHashMap
    decode = HashMap <$> HM.fromList <$> decode
