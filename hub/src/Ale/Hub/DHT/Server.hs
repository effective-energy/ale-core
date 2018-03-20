{-# LANGUAGE TypeOperators #-}

-- | This module contains 'DHTComponent'.  This component stores DHT
-- request from all nodes locally and answers with data stored.
module Ale.Hub.DHT.Server
       ( withStorage
       ) where

import Universum

import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (withReaderT)
import Monad.Capabilities (CapImpl (CapImpl), CapsT, HasCaps, HasNoCap, addCap)
import System.Wlog (WithLoggerIO, logDebug)
import UnliftIO.Async (withAsync)

import Ale.Capabilities (CapImplIO)
import Ale.Core.DHT.Types (DHT (..))
import Ale.Core.Storage (HasPrefixTag (..), Reference, Storable, StoragePrefix (..), mkReference,
                         unsafeMkReferenceFromRaw, unsafeReferenceToBytes)
import Ale.Fmt ((+||), (||+))
import Ale.Node.Context.Logging (Logging)
import Ale.Node.DB.Types (DB, MonadDB, getDBRaw, putDBRaw)
import Ale.Node.Networking.Server (RawServerComponent, ServerComponent, fromRaw, serverReply)
import Ale.Node.Storage.Types (DHTReply (..), DHTRequest (..))

import qualified Data.ByteString.Lazy as BSL


withStorage :: (MonadMask m, MonadUnliftIO m,
                HasNoCap DHT caps, HasCaps [DB, Logging] caps)
            => RawServerComponent
            -> (CapImplIO DHT '[DB] -> CapsT (DHT : caps) m a)
            -> CapsT caps m a
withStorage rsc cont = do
    let sc :: ServerComponent DHTRequest DHTReply = fromRaw rsc

    withAsync (receive sc) $ \_ ->
        let _getDHT :: forall m s. (MonadIO m, MonadDB m, Storable s)
                    => Reference s -> m (Either Text s)
            _getDHT ref = do
                let pref = getStoragePrefix $ storagePrefix @s
                    key = unsafeReferenceToBytes ref

                mbs <- getDBRaw pref key
                case mbs of
                    Right bs ->
                        pure . first show . deserialiseOrFail . BSL.fromStrict $ bs
                    Left e ->
                        pure . Left $ show e

            _putDHT :: forall m s. (MonadIO m, MonadDB m, Storable s)
                        => s -> m (Reference s)
            _putDHT item = do
                let ref  = mkReference item
                    pref = getStoragePrefix $ storagePrefix @s
                    key  = unsafeReferenceToBytes ref
                    bs   = BSL.toStrict . serialise $ item
                putDBRaw pref key bs
                pure ref

            dhtImpl :: CapImplIO DHT '[DB]
            dhtImpl = CapImpl DHT{_getDHT, _putDHT}

        in withReaderT (addCap dhtImpl) (cont dhtImpl)
  where
    receive :: (MonadDB m, WithLoggerIO m)
            => ServerComponent DHTRequest DHTReply -> m ()
    receive sc = forever $ serverReply sc $ \req rep ->
        case req of
            Get pref key -> do
                logDebug $ "DHT Get req: "+||pref||+" "+||key||+""
                res <- getDBRaw pref key
                res & either (\_ -> rep $ NotFound pref key) (rep . NewItem pref key)

            Put pref bs -> do
                let ref = unsafeMkReferenceFromRaw bs
                    key = unsafeReferenceToBytes ref
                logDebug $ "DHT Put req: "+||pref||+" "+||key||+""
                putDBRaw pref key bs
