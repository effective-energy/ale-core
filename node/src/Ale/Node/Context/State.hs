{-# LANGUAGE TypeOperators #-}

module Ale.Node.Context.State
       ( stateDBImpl
       , withAleState

       , mkIORefStateImpl
       ) where

import Universum

import Control.Monad.Reader (withReaderT)
import Fmt ((+|), (|+))
import Monad.Capabilities (CapImpl (CapImpl), CapsT, HasCaps, HasNoCap, addCap)
import System.Wlog (logError, logInfo)

import Ale.Capabilities (CapImplIO)
import Ale.Core.Block (BlockPointer)
import Ale.Core.DHT.Types (DHT)
import Ale.Core.Genesis.Block (initialState)
import Ale.Core.Genesis.Data (GenesisData)
import Ale.Core.Height (Height)
import Ale.Node.Context.Logging (Logging)
import Ale.Node.DB.Types (DB, DBError (..), MonadDB, Persistable, getDB', putDB')
import Ale.State (AleState (..), AleStateData)


stateDBImpl :: CapImplIO AleState '[DB]
stateDBImpl = CapImpl AleState
    { _getASBlock = getDB'' "state/block"
    , _putASBlock = putDB' "state/block"

    , _getASHeight = getDB'' "state/height"
    , _putASHeight = putDB' "state/height"

    , _getASData = getDB'' "state/data"
    , _putASData = putDB' "state/data"
    }
  where
    getDB'' :: (Persistable v, Monad m, MonadDB m) => ByteString -> m (Either Text v)
    getDB'' = fmap (first show) . getDB'


-- | Initialise Ale state if needed.
withAleState :: (MonadIO m,
                 HasCaps '[DB, Logging, DHT] caps, HasNoCap AleState caps)
             => GenesisData  -- ^ This data will be used to initialise state
                             -- in case there is none in the database.
             -> (CapImplIO AleState '[DB] -> CapsT (AleState : caps) m a)
             -> CapsT caps m a
withAleState genData cont =
    withReaderT (addCap stateDBImpl) $ do
        ebp <- getDB' "state/block"
        case ebp of
            Left DBKeyNotFound -> do
                logInfo "Initialising state data using genesis"
                initialState genData
            Left (DBCorruptedValue e) -> do
                logError $ "Corrupted database: "+|e|+""
                exitFailure
            Right (bp :: BlockPointer) -> logInfo $ "Resuming at block "+|bp|+""
        cont stateDBImpl



-- | An implementation of the 'AleState' capability which stores data in 'IORef's.
mkIORefStateImpl :: IORef BlockPointer
                 -> IORef Height
                 -> IORef AleStateData
                 -> (forall m. MonadIO m => CapImpl AleState '[] m)
mkIORefStateImpl blockVar heightVar dataVar = CapImpl AleState
    { _getASBlock = Right <$> readIORef blockVar
    , _putASBlock = writeIORef blockVar

    , _getASHeight = Right <$> readIORef heightVar
    , _putASHeight = writeIORef heightVar

    , _getASData = Right <$> readIORef dataVar
    , _putASData = writeIORef dataVar
    }
