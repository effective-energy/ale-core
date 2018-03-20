-- | Core functionality of an ALE node.
module Ale.Node
       ( monitorBlocks
       , foreverBlocks
       , waitFor
       ) where

import Universum

import System.Wlog (WithLoggerIO, logDebug, logError, logWarning)

import Ale.Core.DHT.Types (MonadDHT)
import Ale.Fmt ((+|), (+||), (|+), (||+))
import Ale.Node.Blockchain.Client (BlockchainClientComponent (..))
import Ale.State (MonadAleState, updateState)


-- | Apply blocks one by one peforming an action after each state update.
-- Keep doing this until action returns a value.
monitorBlocks :: (WithLoggerIO m, MonadAleState m, MonadDHT m)
              => BlockchainClientComponent  -- ^ Blockchain component
              -> m (Maybe a)                -- ^ Action to perform
              -> m a
monitorBlocks bcc act = do
    mv <- act
    case mv of
        Just v -> pure v
        Nothing -> do
            blk <- bccNextBlock bcc
            logDebug $ "Trying to update state with block "+|blk|+""
            res <- updateState blk (bccServerKey bcc)
            case res of
                Left err -> do
                    logError $ "Failed to update state: "+||err||+""
                    logWarning $ "Skipping block: "+|blk|+""
                Right diff -> logDebug $ "State updated. Diff: "+||diff||+""
            monitorBlocks bcc act


-- | Apply blocks one by one peforming an action after each state update.
-- Do this forever.
foreverBlocks :: (MonadAleState m, WithLoggerIO m, MonadDHT m)
              => BlockchainClientComponent  -- ^ Blockchain component
              -> m ()                       -- ^ Action to perform
              -> m ()
foreverBlocks bcc act = monitorBlocks bcc act'
  where
    act' = act >> pure Nothing


-- | Keep updating the state with incoming blocks, until
-- the query succeeds and returns a value.
waitFor :: (WithLoggerIO m, MonadAleState m, MonadDHT m)
        => BlockchainClientComponent  -- ^ Blockchain component
        -> Maybe a                    -- ^ Query to perform on the state
        -> m a
waitFor bcc query = monitorBlocks bcc (pure query)
