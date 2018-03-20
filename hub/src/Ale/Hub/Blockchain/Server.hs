{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides blockchain server functionality.
module Ale.Hub.Blockchain.Server
       ( Blockchain (..)
       , MonadBlockchain (..)

       , withBlockchain
       ) where

import Universum

import Control.Concurrent.STM (retry)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (withReaderT)
import Monad.Capabilities (CapImpl (CapImpl), CapsT, HasCaps, HasNoCap, addCap, makeCap)
import UnliftIO.Async (async, withAsync)

import Ale.Capabilities (CapImplIO)
import Ale.Core.Block (Block, BlockPointer, mkPointer)
import Ale.Core.Crypto (SecretKey)
import Ale.Core.Crypto.Signed (mkSigned)
import Ale.Node.Blockchain.Types (BlockchainReply (..), BlockchainRequest (..))
import Ale.Node.Context.Logging (Logging)
import Ale.Node.DB.Types (DB, getDB, putDB)
import Ale.Node.Networking.Server (RawServerComponent, ServerComponent, fromRaw, serverReply)


data Blockchain m = Blockchain
    { _bSaveBlock :: !(Block -> m ())  -- ^ Save a new block, we will serve it
    }
makeCap ''Blockchain


withBlockchain :: forall m caps a.
                  (MonadMask m, MonadUnliftIO m,
                   HasCaps '[DB, Logging] caps,
                   HasNoCap Blockchain caps)
               => SecretKey
               -> BlockPointer
               -> RawServerComponent
               -> (CapImplIO Blockchain '[] -> CapsT (Blockchain : caps) m a)
               -> CapsT caps m a
withBlockchain sk iniBPtr rsc cont = do
    -- Last block we received
    lastBlock <- liftIO $ newTVarIO iniBPtr

    bChan <- liftIO newTChanIO

    withAsync (reply lastBlock) $ \_ ->
        withAsync (saveBlocks bChan lastBlock) $ \_ ->
            let blockchainImpl :: CapImplIO Blockchain '[]
                blockchainImpl = CapImpl $ Blockchain (atomically . writeTChan bChan)
            in withReaderT (addCap blockchainImpl) (cont blockchainImpl)
  where
    reply :: TVar BlockPointer -> CapsT caps m ()
    reply lastBlock =
        forever $ serverReply sc $ \req rep ->
            case req of
                GetHead             -> do
                    -- NB: While we were asked to return Head, what we actually
                    -- return is the last block we received on our outside channel.
                    bp <- readTVarIO lastBlock
                    rep $ Head $ mkSigned sk bp
                GetHeadOnUpdate     -> void $ async $ do
                    prev <- readTVarIO lastBlock
                    new <- atomically $ do
                        cur <- readTVar lastBlock
                        when (prev == cur) retry
                        pure cur
                    rep $ Head $ mkSigned sk new
                GetBlock bPtr     -> do
                    mb <- getDB bPtr
                    case mb of
                        Left  _   -> rep $ NoSuchBlock bPtr
                        Right blk -> rep $ BlockContent $ mkSigned sk blk

    saveBlocks :: TChan Block -> TVar BlockPointer -> CapsT caps m ()
    saveBlocks bChan lastBlock = forever $ do
        b <- atomically $ readTChan bChan
        let bPtr = mkPointer b
        putDB bPtr b
        atomically $ writeTVar lastBlock bPtr

    sc :: ServerComponent BlockchainRequest BlockchainReply
    sc = fromRaw rsc
