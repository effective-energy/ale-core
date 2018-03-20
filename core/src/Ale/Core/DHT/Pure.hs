-- | This module provides pure DHT implementation.  Provided
-- implementation doesn't persist data anywhere.

module Ale.Core.DHT.Pure
       ( DHT
       , emptyPureDHT
       , getPureDHT
       , putPureDHT

       , PureDHTT ()
       , runPureDHTT
       , evalPureDHTT

       , PureDHT ()
       , runPureDHT
       , evalPureDHT
       ) where

import Universum hiding (Key)

import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad.State (MonadState (..))
import Crypto.Hash (Blake2b_256, Digest)
import qualified Data.ByteString.Lazy as BSL (fromStrict, toStrict)
import qualified Data.Map as M

import Ale.Core.DHT.Types (MonadDHT (..))
import Ale.Core.Storage.Types (HasPrefixTag (..), PrefixTag, Reference, Storable,
                               StoragePrefix (..), mkReference, unsafeUnpackReference)

-- | 'Key' is a pair of (prefix, hash).  Prefix is added here to
-- ensure that if some typed value was stored, value of different type
-- can't be retrieved before it's stored with correct type.
type Key = (PrefixTag, Digest Blake2b_256)

-- | We need to omit the 'Reference''s phantom type here so the map's
-- key here is just raw hash.  'unsafeUnpackReference' function exists
-- solely to be used here.
type DHT = Map Key ByteString

emptyPureDHT :: DHT
emptyPureDHT = M.empty

toKey :: forall a. Storable a => Reference a -> Key
toKey ref = (getStoragePrefix (storagePrefix @a), unsafeUnpackReference ref)

putPureDHT :: Storable item => item -> DHT -> (DHT, Reference item)
putPureDHT x dht = (M.insert (toKey ref) x' dht, ref)
  where
    ref = mkReference x
    x' = BSL.toStrict $ serialise x

getPureDHT :: Storable item => Reference item -> DHT -> Either Text item
getPureDHT ref dht = case M.lookup (toKey ref) dht of
    Nothing -> Left "Key not found"
    Just v  -> first show $ deserialiseOrFail (BSL.fromStrict v)

-- | A local-only (non-)DHT storage.
newtype PureDHTT m a = PureDHTT (StateT DHT m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

type PureDHT = PureDHTT Identity

runPureDHTT :: Monad m => PureDHTT m a -> m (a, DHT)
runPureDHTT (PureDHTT m) = runStateT m M.empty

evalPureDHTT :: Monad m => PureDHTT m a -> m a
evalPureDHTT (PureDHTT m) = evalStateT m M.empty

-- | Performs a computation that uses DHT and returns a map representing
-- the resulting state of DHT.
runPureDHT :: PureDHT a -> (a, DHT)
runPureDHT = runIdentity . runPureDHTT

-- | Performs a computation that uses DHT.
evalPureDHT :: PureDHT a -> a
evalPureDHT = runIdentity . evalPureDHTT

instance Monad m => MonadDHT (PureDHTT m) where
    getDHT key = PureDHTT $ getPureDHT key <$> get
    putDHT item = PureDHTT $ do
        dht <- get
        let (dht', ref) = putPureDHT item dht
        put dht'
        pure ref
