{-# LANGUAGE Rank2Types #-}

-- | Ping-pong client provides an interface for pinging the server.
--
-- After you initialise it, it gives you the 'ping' function that sends a ping
-- and blocks waiting for a response from the server.
module Ale.Node.PingPong.Client
       ( PingComponent (pcPing, pcPingStats)
       , PingStats ()
       , psPingsSent
       , withPing
       ) where

import Universum

import Async.Combinators (withWorker)
import Control.Concurrent.MVar (modifyMVar_)
import Control.Lens (makeLenses, (+~))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Serokell.Aeson.Options (defaultOptions)
import System.Wlog (WithLoggerIO)

import Ale.Node.Networking.Client (ClientComponent, RawClientComponent, clientReceive,
                                   clientRequest, fromRaw)

import qualified Control.Concurrent.Event as E

data PingComponent = PingComponent
    { pcPing      :: forall m. MonadIO m => m ()
    , pcPingStats :: forall m. MonadIO m => m PingStats
    }

data PingStats = PingStats
    { _psPingsSent :: Int
    } deriving (Eq, Generic, Show)

makeLenses ''PingStats

instance ToJSON PingStats where
    toJSON = genericToJSON defaultOptions

instance FromJSON PingStats where
    parseJSON = genericParseJSON defaultOptions

withPing :: (MonadMask m, MonadUnliftIO m, WithLoggerIO m)
         => RawClientComponent
         -> (PingComponent -> m a) -> m a
withPing rcc cont = do
    evt <- liftIO E.new
    stats <- newMVar PingStats { _psPingsSent = 0 }

    let runReceiver = forever $ do
            clientReceive cc
            liftIO $ E.signal evt

    withWorker "pingWorker" runReceiver $
        cont PingComponent { pcPing = liftIO $ do
                                 clientRequest cc ()
                                 modifyMVar_ stats $ pure . (psPingsSent +~ 1)
                                 E.wait evt
                           , pcPingStats = liftIO $ readMVar stats
                           }
  where
    cc :: ClientComponent () ()
    cc = fromRaw rcc
