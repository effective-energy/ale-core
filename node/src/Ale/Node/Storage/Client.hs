{-# LANGUAGE TypeOperators #-}

-- | This module provides 'StorageComponent' that allows nodes to
-- establish real DHT shared with server.
module Ale.Node.Storage.Client
       ( withStorage
       ) where

import Universum

import Async.Combinators (withWorker)
import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Control.Concurrent.Broadcast (Broadcast, listenTimeout, signal)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (withReaderT)
import Data.ByteString (ByteString)
import Focus (Decision (Keep, Remove, Replace), StrategyM)
import Monad.Capabilities (CapImpl (CapImpl), CapsT, HasCap, HasNoCap, addCap)
import STMContainers.Map as STM (Map)
import STMContainers.Map as STM.Map
import System.Wlog (WithLoggerIO)

import Ale.Capabilities (CapImplIO)
import Ale.Core.DHT.Types (DHT (..))
import Ale.Core.Storage (HasPrefixTag (..), PrefixTag, Reference, Storable, StoragePrefix (..),
                         mkReference, unsafeReferenceToBytes)
import Ale.Node.Context.Logging (Logging)
import Ale.Node.DB.Memory (DB, getDBRaw, putDBRaw, withDB)
import Ale.Node.Networking.Client (ClientComponent, RawClientComponent, clientReceive,
                                   clientRequest, fromRaw)
import Ale.Node.Storage.Types (DHTReply (..), DHTRequest (..))

import qualified Control.Concurrent.Broadcast as Broadcast
import qualified Data.ByteString.Lazy as BSL

type Waiting = Broadcast (Maybe ByteString)
type DhtMap = STM.Map (PrefixTag, ByteString) Waiting


withStorage :: (MonadMask m, MonadUnliftIO m,
                HasNoCap DHT caps, HasCap Logging caps)
            => RawClientComponent
            -> Integer
            -> (CapImplIO DHT '[] -> CapsT (DHT : caps) m a)
            -> CapsT caps m a
withStorage rcc timeout cont = do
    let cc :: ClientComponent DHTRequest DHTReply = fromRaw rcc

    -- When we try to get a value we first try to look it up in the local database.
    -- If it is not there we will send a request to the server, but first we will
    -- create a 'Broadcast' that serves two purposes:
    --
    -- 1. The thread that made the request will wait for the reply on this broadcast.
    -- 2. If any other threads are trying to get the same value, they will see
    --    the broadcast and wait on it too, so we avoid extra server requests.
    requested <- liftIO STM.Map.newIO

    withDB $ \db ->
        withWorker "storageWorker" (receive cc requested db) $
            let _getDHT :: forall m s. (MonadIO m, Storable s)
                            => Reference s -> m (Either Text s)
                _getDHT ref = do
                    let pref = getStoragePrefix $ storagePrefix @s
                        key  = unsafeReferenceToBytes ref
                    -- Try local cache first, if not found, lookup remotely.
                    getDBRaw pref key db >>= \case
                        Nothing -> lookupRemotely cc pref key requested
                        Just v -> pure . first show .
                            deserialiseOrFail . BSL.fromStrict $ v

                _putDHT :: forall m s. (MonadIO m, Storable s)
                            => s -> m (Reference s)
                _putDHT item = do
                    let ref  = mkReference item
                        pref = getStoragePrefix $ storagePrefix @s
                        key  = unsafeReferenceToBytes ref
                        bs   = BSL.toStrict . serialise $ item
                    putDBRaw pref key bs db         -- Cache locally
                    clientRequest cc $ Put pref bs  -- then send to server.
                    pure ref

                dhtImpl :: CapImplIO DHT '[]
                dhtImpl = CapImpl DHT{_getDHT, _putDHT}

            in withReaderT (addCap dhtImpl) (cont dhtImpl)
  where
    -- Nothing in local cache. Lookup existing broadcast for this
    -- reference and if there is none, insert a new one.
    lookupRemotely :: forall m s . (MonadIO m, Storable s, Serialise s)
                   => ClientComponent DHTRequest DHTReply
                   -> ByteString
                   -> ByteString
                   -> STM.Map.Map (ByteString, ByteString) Waiting
                   -> m (Either Text s)
    lookupRemotely cc pref key requested = do
        newBroadcast <- liftIO Broadcast.new
        mb <- atomically $ STM.Map.focus (strategy newBroadcast) (pref, key) requested
        clientRequest cc $ Get pref key
        liftIO (flip listenTimeout timeout $ fromMaybe newBroadcast mb) <&> \case
            Nothing         -> Left "Client request timeout"
            Just Nothing    -> Left "Value not found"
            Just (Just val) -> decode' val
          where
            strategy :: Waiting -> StrategyM STM Waiting (Maybe Waiting)
            strategy newBroadcast = \case
                Nothing -> pure (Nothing, Replace newBroadcast)
                Just b  -> pure (Just b, Keep)
            decode' :: Serialise s => ByteString -> Either Text s
            decode' bs = first show $ deserialiseOrFail (BSL.fromStrict bs)

    receive :: WithLoggerIO m
            => ClientComponent DHTRequest DHTReply -> DhtMap -> DB -> m ()
    receive cc requested db = forever $ clientReceive cc >>= \case
        NewItem pref key value -> do
            putDBRaw pref key value db -- Store item in local cache.
            signalAndRemove pref key (Just value) requested
        NotFound pref key -> signalAndRemove pref key Nothing requested
      where
        -- Check if someone is waiting for this item, remove the broadcast.
        signalAndRemove :: WithLoggerIO m
                        => PrefixTag -> ByteString -> Maybe ByteString -> DhtMap -> m ()
        signalAndRemove pref key mv dhtmap = flip whenJustM (liftIO . flip signal mv)
            $ atomically $ STM.Map.focus (pure . (, Remove)) (pref, key) dhtmap
