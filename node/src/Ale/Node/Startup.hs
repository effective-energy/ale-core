-- | Simplify new tools creation process by providing a `withNode`
-- function that start all components.

module Ale.Node.Startup
       ( withNode
       , withNode'
       , Components (..)
       , IOCaps (..)
       ) where

import Universum

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (withReaderT)
import Control.Monad.Trans.Resource (MonadBaseControl)
import Monad.Capabilities (Capabilities, CapabilitiesBuilder (..), CapsT, addCap, initCaps)
import System.Wlog (WithLoggerIO, logError, logInfo)

import Ale.Core.DHT.Types (DHT, MonadDHT (..))
import Ale.Core.Genesis.Block (initialState)
import Ale.Core.Genesis.Data (gdServerKey)
import Ale.Core.Storage.Types (unsafeReferenceFromBase16)
import Ale.Fmt ((+|), (|+))
import Ale.Node.Blockchain.Client (BlockchainClientComponent, withBlockchain)
import Ale.Node.Config (AleNodeConfigP (..), PartialAleNodeConfig, finalise, loadNodeConfig)
import Ale.Node.Context.Logging (Logging, LoggingImplIO (..), mkLoggingImpl)
import Ale.Node.Context.Mining (Mining)
import Ale.Node.Context.State (stateDBImpl)
import Ale.Node.DB.Rocks (withDBImpl)
import Ale.Node.DB.Types (DB)
import Ale.Node.Mining.Client (withMining)
import Ale.Node.Networking.Client (ClientContext (ccComponents), withClient)
import Ale.Node.Storage.Client (withStorage)
import Ale.State (AleState, getASBlockOrError)
import Ale.Tools.Paths (genesisHashFile, getDataDir, nodeDBDir)


data Components = Components
    { cBlockchain :: BlockchainClientComponent
    }

type NodeCapabilities = '[Mining, AleState, DB, DHT, Logging]

-- | This wrapper helps to keep capabilities polymorphic.
data IOCaps = IOCaps
    { unIOCaps :: forall m. (MonadIO m, MonadMask m, MonadBaseControl IO m)
               => Capabilities NodeCapabilities m
    }


-- | Starts all the components required for a node and launches a computation that
-- requires standard capabilities.
--
-- It does not setup logging, merely rewraps it, so you have to setup logging yourself
-- before calling this function.
withNode :: (MonadFail m, MonadMask m, MonadUnliftIO m, WithLoggerIO m, MonadBaseControl IO m)
         => FilePath              -- ^ File to load node config from
         -> PartialAleNodeConfig  -- ^ Overrides for the loaded config
         -> (Components -> CapsT NodeCapabilities m a)
         -> m a
withNode cfgPath cfgOver cont = withNode' cfgPath cfgOver $ \comps _ -> cont comps

-- | Starts all the components required for a node, prepared all the required
-- capabilities and gives them to the continuation.
--
-- It does not setup logging, merely rewraps it, so you have to setup logging yourself
-- before calling this function.
withNode' :: forall m a. (MonadFail m, MonadMask m, MonadUnliftIO m, WithLoggerIO m, MonadBaseControl IO m)
          => FilePath              -- ^ File to load node config from
          -> PartialAleNodeConfig  -- ^ Overrides for the loaded config
          -> (Components -> IOCaps -> CapsT NodeCapabilities m a)
          -> m a
withNode' cfgPath cfgOver cont = do
    loggingImpl <- mkLoggingImpl
    usingReaderT (addCap (getLoggingImplIO loggingImpl) $ initCaps NoCaps) $ do
        dataDir <- getDataDir

        cfgLoaded <- eitherIO "Loading config" =<< loadNodeConfig cfgPath
        AleNodeConfig{..} <- eitherIO "Invalid config" $ finalise $ mconcat
            [ mempty { ancGenesis = Last (Just $ genesisHashFile dataDir)
                     , ancDBDir = Last (Just $ nodeDBDir dataDir)
                     }
            , cfgLoaded
            , cfgOver
            ]

        genRef <- readFile ancGenesis >>= unsafeReferenceFromBase16

        withClient ancHost ancPort components ancTimeOut $ \cc -> do
            let [sc',bcc',mcc'] = ccComponents cc

            withStorage sc' (fromIntegral ancTimeOut) $ \dhtImpl ->
                getDHT genRef >>= \case
                    Left err -> do
                        logError $ "Failed to download Genesis data: "+|err|+""
                        exitFailure
                    Right genData -> do
                        logInfo "Genesis data was downloaded successfully"
                        withDBImpl ancDBDir $ \dbImpl -> withReaderT (addCap stateDBImpl) $ do
                            initialState genData
                            blk <- getASBlockOrError
                            withBlockchain blk (genData^.gdServerKey) bcc' $ \bcc ->
                                withMining mcc' $ \miningImpl -> do
                                    let iocaps = IOCaps $ initCaps
                                              $ AddCap miningImpl
                                              $ AddCap stateDBImpl
                                              $ AddCap dbImpl
                                              $ AddCap dhtImpl
                                              $ AddCap (getLoggingImplIO loggingImpl)
                                                NoCaps
                                    cont (Components bcc) iocaps
  where
    components = ["dht", "blockchain", "mining"]


-- TODO: This function has to be in some library.
eitherIO :: (WithLoggerIO m, Buildable e) => Text -> Either e a -> m a
eitherIO msg ea = case ea of
    Left e  -> do
        logError $ ""+|msg|+": "+|e|+""
        exitFailure
    Right a -> pure a
