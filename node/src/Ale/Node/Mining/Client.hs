{-# LANGUAGE TypeOperators #-}

-- | This module provides a client component for sending messages to
-- hub for blockchain inclusion.
module Ale.Node.Mining.Client
       ( withMining
       ) where

import Universum

import Async.Combinators (withWorker)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (withReaderT)
import Monad.Capabilities (CapImpl (CapImpl), CapsT, HasCap, HasNoCap, addCap)

import Ale.Capabilities (CapImplIO)
import Ale.Node.Context.Logging (Logging)
import Ale.Node.Context.Mining (Mining (..))
import Ale.Node.Mining.Types (MiningReply, MiningRequest (..))
import Ale.Node.Networking.Client (ClientComponent, RawClientComponent, clientReceive,
                                   clientRequest, fromRaw)


withMining :: (MonadMask m, MonadUnliftIO m,
               HasNoCap Mining caps, HasCap Logging caps)
           => RawClientComponent
           -> (CapImplIO Mining '[] -> CapsT (Mining : caps) m a)
           -> CapsT caps m a
withMining rcc cont = do
    let cc :: ClientComponent MiningRequest MiningReply = fromRaw rcc

    -- Replies are not meaningful.  Just receive them to not bloat the
    -- queue.
    let receiveReplies = forever $
            void $ clientReceive cc

    let miningImpl :: CapImplIO Mining '[]
        miningImpl = CapImpl Mining
            { _mine = clientRequest cc . SendMessage
            }

    withWorker "miningWorker" receiveReplies $
        withReaderT (addCap miningImpl) (cont miningImpl)
