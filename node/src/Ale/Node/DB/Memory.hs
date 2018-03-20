-- | This module provides a memory-backed database.
-- All operations are atomic so it can be safely used from multiple threads.
module Ale.Node.DB.Memory
       ( DB ()
       , withDB

       , getDB
       , getDBRaw

       , putDB
       , putDBRaw
       ) where

import Universum

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import STMContainers.Map as STM (Map)
import STMContainers.Map as STM.Map

import Ale.Core.Storage (HasPrefixTag (..), PrefixTag, StoragePrefix (..))

import qualified Data.ByteString.Lazy as BSL


-- | Mamory database type.
type DB = STM.Map (PrefixTag, ByteString) ByteString


-- | Perform an action that has access to a memory database.
withDB :: MonadIO m => (DB -> m a) -> m a
withDB act = liftIO STM.Map.newIO >>= act

-- | Retrieve a vaue from the database.
getDB :: forall m k v. (Serialise k, HasPrefixTag v, MonadIO m)
      => k   -- ^ Key
      -> DB  -- ^ Database
      -> m (Either Text v)
getDB key db = do
    val <- getDBRaw (getStoragePrefix $ storagePrefix @v)
                    (BSL.toStrict $ serialise key) db
    pure $ case val of
        Nothing   -> Left "No such key in DB"
        Just val' -> first show $ deserialiseOrFail $ BSL.fromStrict val'

-- | Retrieve a raw 'ByteString', don't try to deserialise it.
getDBRaw :: MonadIO m
         => PrefixTag   -- ^ Prefix tag of the value type
         -> ByteString  -- ^ Raw key
         -> DB          -- ^ Database
         -> m (Maybe ByteString)
getDBRaw pref key = atomically . STM.Map.lookup (pref, key)


-- | Store a value in the database.
putDB :: forall m k v. (Serialise k, HasPrefixTag v, MonadIO m)
      => k   -- ^ Key
      -> v   -- ^ Value
      -> DB  -- ^ Database
      -> m ()
putDB key val = putDBRaw (getStoragePrefix $ storagePrefix @v)
                         (BSL.toStrict $ serialise key)
                         (BSL.toStrict $ serialise val)

-- | Store a raw value in the database.
putDBRaw :: MonadIO m
         => PrefixTag   -- ^ Prefix tag of the value type
         -> ByteString  -- ^ Raw key
         -> ByteString  -- ^ Raw value
         -> DB          -- ^ Database
         -> m ()
putDBRaw pref key val = atomically . STM.Map.insert val (pref, key)
