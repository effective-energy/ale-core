-- | This module provides a wrapper around 'SecretKey' that can be
-- persisted.

module Ale.Wallet.Crypto
       ( StorableSecretKey (..)
       ) where

import Universum

import Codec.Serialise (Serialise (..))

import Ale.Core.Crypto (SecretKey, secretFromBytes, secretToBytes)
import Ale.Core.Storage.Types (HasPrefixTag (..), StoragePrefix (..))
import Ale.Node.DB.Types (Persistable)

-- | 'SecretKey' can not be persisted simply.  This is intended
-- behaviour.  But Wallet needs to stora 'SecretKey's in DB and this
-- 'StorableSecretKey' wrapper addresses this issue by providing all
-- missing instances.
newtype StorableSecretKey = StorableSecretKey
    { getStorableSecretKey :: SecretKey }
    deriving (Generic)

-- | TODO encrypt them probably?
instance Serialise StorableSecretKey where
    encode = encode . secretToBytes . getStorableSecretKey
    decode = StorableSecretKey <$> (decode >>= secretFromBytes)

instance HasPrefixTag StorableSecretKey where
    storagePrefix = StoragePrefix "secretKey"

instance Persistable StorableSecretKey
