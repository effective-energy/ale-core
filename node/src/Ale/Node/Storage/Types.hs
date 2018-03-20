-- | This module provides 'DHTRequest' and 'DHTReply' types that are
-- used in interaction between local caching DHT node and remote
-- fallback DHT.

module Ale.Node.Storage.Types
       ( DHTRequest (..)
       , DHTReply (..)
       ) where

import Universum

import Codec.Serialise (Serialise)

import Ale.Core.Storage.Types (PrefixTag)


-- | This 'DHTRequest's are send over the network during
-- communication.  There are reasons for this type to use raw
-- 'ByteString's.  The desired types for 'Get' and 'Put' are
-- existential:
--
-- @Get :: (forall s. Storable s => Reference s) -> DHTRequest@
--
-- @Put :: (forall s. Storable s => s) -> DHTRequest@
--
-- This types would allow typesafe communication between client and
-- server.  The problem is that 'DHTRequest' should be 'Serialise'able
-- to enable network communication but deserialisation of existential
-- types is impossible (AFAIU it's possible with dependent types
-- though).
--
-- So we pass a 'PrefixTag' for type being in use now in both 'Get'
-- and 'Put'.
data DHTRequest = Get PrefixTag ByteString | Put PrefixTag ByteString
    deriving (Generic, Show)

instance Serialise DHTRequest where

-- | The reply is simply "Hey, new (prefix, key, item) pair is
-- available, please store it".
data DHTReply
    = NotFound PrefixTag ByteString
    | NewItem PrefixTag ByteString ByteString
    deriving (Generic, Show)

instance Serialise DHTReply where
