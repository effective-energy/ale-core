-- |

module Main
       ( main
       ) where

import Universum

import Control.Concurrent (threadDelay)
import Monad.Capabilities (CapsT)
import System.Wlog (logInfo)
import UnliftIO.Async (async)

import Ale.Core.DHT.Types (MonadDHT (..))
import Ale.Core.Storage.Types (mkReference)
import Ale.Fmt ((+||), (||+))
import Ale.Node.Context.Logging (Logging, setupAleLoggingWithName)
import Ale.Node.Networking.Client (ClientContext (..), withClient)
import Ale.Node.Storage.Client (withStorage)

main :: IO ()
main = setupAleLoggingWithName "ale-node-storage" storage

storage :: CapsT '[Logging] IO ()
storage = do
    logInfo "Storage example starting..."

    withClient "127.0.0.1" 12347 [ "dht" ] 1000000 $ \cCtx -> do
        let [cc] = ccComponents cCtx
        withStorage cc timeout $ \_ -> do
            _ <- async $ do
                logInfo "Trying to get... (should hang for 5 seconds _only_ during first run)"
                x <- getDHT (mkReference ("kek" :: ByteString))
                logInfo $ "Got "+||x||+" from first"
            logInfo "Waiting for 5 seconds..."
            liftIO $ threadDelay 5000000
            logInfo "Putting value..."
            ref <- putDHT ("kek" :: ByteString)
            logInfo "Getting value... (should finish immediately)"
            y <- getDHT ref
            logInfo $ "Got "+||y||+" from second"

            liftIO $ threadDelay 1000000
  where
    timeout = 5000000
