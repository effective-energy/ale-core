{-# LANGUAGE Rank2Types #-}

-- | Ping-pong server implements a ponging server.
--
-- After you initialise it, it starts responding to client pings in the background
-- and also gives you a channel that contains an element for each ping received.
module Ale.Node.PingPong.Server
       ( PongComponent (pcChan, pcPongStats)
       , PongStats ()
       , psPingsReceived
       , initialise
       ) where

import Universum

import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (modifyMVar_)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)
import Control.Lens (makeLenses, (+~))
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Serokell.Aeson.Options (defaultOptions)
import System.Wlog (WithLoggerIO, liftLogIO)

import Ale.Node.Networking.Server (RawServerComponent, ServerComponent, fromRaw, serverReply)

data PongComponent = PongComponent
    { pcChan      :: TChan ()
    , pcPongStats :: forall m. MonadIO m => m PongStats
    }

data PongStats = PongStats
    { _psPingsReceived :: Int
    } deriving (Eq, Generic, Show)

makeLenses ''PongStats

instance ToJSON PongStats where
    toJSON = genericToJSON defaultOptions

instance FromJSON PongStats where
    parseJSON = genericParseJSON defaultOptions

initialise :: WithLoggerIO m => RawServerComponent -> m PongComponent
initialise rsc = do
    chan <- liftIO newTChanIO
    stats <- newMVar PongStats { _psPingsReceived = 0 }

    void . liftLogIO async . forever $ serverReply sc $ \() resp -> do
        resp ()
        liftIO $ modifyMVar_ stats $ pure . (psPingsReceived +~ 1)
        atomically $ writeTChan chan ()

    pure PongComponent { pcChan = chan
                       , pcPongStats = liftIO $ readMVar stats
                       }
  where
    sc :: ServerComponent () ()
    sc = fromRaw rsc
