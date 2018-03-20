-- | This module implements the Blockchain component, that is reponsible for
-- keeping the blocks received from the server in order, fetching the missing
-- ones if needed.
module Ale.Node.Blockchain.Client
       ( BlockchainClientComponent(..)
       , withBlockchain
       ) where

import Universum

import Async.Combinators (withWorker)
import Control.Concurrent.STM (retry)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Focus (Decision (Replace))
import System.Wlog (WithLoggerIO, logDebug, logWarning)

import Ale.Core.Block (Block, BlockPointer, bParent, mkPointer)
import Ale.Core.Crypto (PublicKey)
import Ale.Core.Crypto.Signed (Signed (sData, sPublicKey))
import Ale.Fmt ((+|), (|+))
import Ale.Node.Blockchain.Types (BlockchainReply (..), BlockchainRequest (..))
import Ale.Node.Networking.Client (ClientComponent, RawClientComponent, clientReceive,
                                   clientRequest, fromRaw)

import qualified STMContainers.Map as STM (Map)
import qualified STMContainers.Map as STM.Map

data BlockchainClientComponent = BlockchainClientComponent
    { bccNextBlock :: forall m. MonadIO m => m Block
      -- ^ This action returns the next block. It is guaranteed
      -- that sequentially returned blocks form a valid chain, i.e.
      -- there are no blocks missing.
      -- Blocks until a new block is received from the server.
    , bccServerKey :: PublicKey
      -- ^ Hub 'PublicKey' is stored here because it's needed to
      -- verify incoming blocks and it's more handy to have it here
      -- comparing to extracting it from 'GenesisData' each time.
    }

withBlockchain :: (MonadMask m, MonadUnliftIO m, WithLoggerIO m)
    => BlockPointer                        -- ^ Last known block
    -> PublicKey                           -- ^ Public key Hub uses to sign blocks
    -> RawClientComponent                  -- ^ Networking client component
    -> (BlockchainClientComponent -> m a)  -- ^ Action to run
    -> m a
withBlockchain startBlock serverKey rcc cont = do
    let cc :: ClientComponent BlockchainRequest BlockchainReply = fromRaw rcc

    -- Channel which we will throw ordered blocks into
    updates <- liftIO newTChanIO

    -- Mapping from block to next block in chain
    nexts <- liftIO STM.Map.newIO

    -- Initially request current head to get things going.
    clientRequest cc GetHead

    logDebug "Blockchain component is now running"
    withWorker "blockchainWorker" (waitNextBlock startBlock nexts updates) $
        withWorker "receive" (receive cc nexts) $
            cont (BlockchainClientComponent
                     (atomically $ readTChan updates)
                     serverKey
                 )
  where
    -- | Thread which watches the map of the next blocks and whenever
    -- the block next to the current one appears, pushes it to the channel.
    waitNextBlock :: WithLoggerIO m
                  => BlockPointer
                  -> STM.Map BlockPointer Block
                  -> TChan Block
                  -> m ()
    waitNextBlock curBlock nextsMap updates = do
        blk <- atomically $ do
            mb <- STM.Map.lookup curBlock nextsMap
            case mb of
                Nothing -> retry
                Just b  -> writeTChan updates b >> pure b
        logDebug $ "Next block is "+|blk|+""
        waitNextBlock (mkPointer blk) nextsMap updates

    -- | Thread which processes messages from the server.
    receive :: WithLoggerIO m
            => ClientComponent BlockchainRequest BlockchainReply
            -> STM.Map BlockPointer Block
            -> m ()
    receive cc nextsMap = forever $ do
        rep <- clientReceive cc
        case rep of
            Head sbPtr -> whenM (isFromServer sbPtr) $ do
                let bPtr = sData sbPtr
                logDebug $ "Got new head: "+|bPtr|+""
                -- TODO check if we already have such block
                clientRequest cc $ GetBlock bPtr
                clientRequest cc GetHeadOnUpdate
                logDebug "Waiting for another head update..."
            NoSuchBlock bPtr    ->
                logWarning $ "Block not found on Hub: "+|bPtr|+""
            BlockContent sBlk -> whenM (isFromServer sBlk) $ do
                let blk = sData sBlk
                logDebug $ "Got block content: "+|blk|+""
                -- If we have not seen this block before, record it
                newParent <- atomically $ STM.Map.focus
                    (\ma -> pure $ case ma of
                                        Nothing -> (True, Replace blk)
                                        Just _  -> (False, Replace blk)
                    )
                    (blk^.bParent)
                    nextsMap
                -- and request its parent.
                -- TODO request the parent block only if we do not have it already.
                when newParent $ clientRequest cc $ GetBlock (blk^.bParent)
      where
        isFromServer s = do
            let eq = serverKey == sPublicKey s
            unless eq $ logWarning $ "Signed by "+|sPublicKey s|+" but not server"
            pure eq
