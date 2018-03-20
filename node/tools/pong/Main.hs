module Main
       ( main
       ) where

import Universum

import Control.Concurrent.STM.TChan (readTChan)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import System.Wlog (WithLoggerIO, logInfo)

import Ale.Log (asyncLog, setupAleLoggingWithName)
import Ale.Node.Networking.Server (ServerContext (..), withServer)
import Ale.Node.PingPong.Server (PongComponent (..), initialise)


main :: IO ()
main = setupAleLoggingWithName "ale-node-pong" pong

pong :: (MonadMask m, MonadUnliftIO m, WithLoggerIO m) => m ()
pong = do
    logInfo "Starting... "

    withServer "127.0.0.1" 12345 [ "ping-pong" ] 1000000 $ \sCtx -> do
        let [sc] = scComponents sCtx
        pc   <- initialise sc

        logInfo "Started."

        void . asyncLog . forever $ do
            () <- atomically $ readTChan (pcChan pc)
            logInfo "Ping-pong"

        unlessM (elem 'q' <$> getContents) $ pure ()

        stats <- pcPongStats pc
        putStrLn $ encodePretty stats
