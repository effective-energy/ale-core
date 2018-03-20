{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Set of messages previously seen on the blockchain.
module Ale.State.MessageSet
       ( MessageSet ()
       , empty
       , size
       , insert
       , lookup
       , delete
       , toList
       , difference
       , union
       ) where

import Universum hiding (empty, toList)

import Codec.Serialise (Serialise)
import Control.Lens (At (at), Index, IxValue, Ixed (ix), iso)
import Data.Map (Map)

import Ale.Core.Entity (Entity)
import Ale.Core.Message (MessagePointer, mkPointer)
import Ale.Core.Storage.Types (HasPrefixTag (..), StoragePrefix (..))
import Ale.Fmt (Buildable (..), jsonMapF', tupleF)

import qualified Data.Map as M

newtype MessageSet m = MessageSet
    { getMessageSet :: Map (MessagePointer m) (Entity, m)
    } deriving (Eq, Show, Serialise, ToPairs)

instance Buildable m => Buildable (MessageSet m) where
    build (MessageSet x) = jsonMapF' build tupleF x

instance Ixed (MessageSet m) where
    ix i = iso getMessageSet MessageSet . ix i

instance At (MessageSet m) where
    at i = iso getMessageSet MessageSet . at i

type instance Index (MessageSet m) = MessagePointer m
type instance IxValue (MessageSet m) = (Entity, m)

instance HasPrefixTag a => HasPrefixTag (MessageSet a) where
    storagePrefix = StoragePrefix $ "messageSetOf" <> getStoragePrefix (storagePrefix @a)

-- | Create an empty 'MessageSet'.
empty :: MessageSet m
empty = MessageSet M.empty

-- | Get number of messages stored in 'MessageSet'
size :: MessageSet m -> Int
size (MessageSet m) = M.size m

-- | Remember a new 'Message'.
insert :: Serialise m
       => Entity  -- ^ Sender of the message
       -> m       -- ^ Content of the 'Message' to insert
       -> MessageSet m -> MessageSet m
insert sender msg (MessageSet m) = MessageSet $ M.insert k (sender, msg) m
  where
    k = mkPointer sender msg

-- | Retrieve a previously stored 'Message'.
lookup :: MessagePointer m  -- ^ Pointer to the requested message
       -> MessageSet m -> Maybe (Entity, m)
lookup p (MessageSet m) = M.lookup p m

-- | Remove a previously stored 'Message'.
delete :: MessagePointer m  -- ^ Pointer to the message being removed
       -> MessageSet m -> MessageSet m
delete p (MessageSet m) = MessageSet $ M.delete p m

-- | Get all elements stored in 'MessageSet'.
toList :: MessageSet m -> [(MessagePointer m, Entity, m)]
toList (MessageSet m) = map (\(a, (b, c)) -> (a, b, c)) $ M.toList m

difference :: MessageSet m -> MessageSet m -> MessageSet m
difference (MessageSet m) (MessageSet m') = MessageSet $ M.difference m m'

union :: MessageSet m -> MessageSet m -> MessageSet m
union (MessageSet m) (MessageSet m') = MessageSet $ M.union m m'
