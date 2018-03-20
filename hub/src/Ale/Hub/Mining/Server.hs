{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server mining component.
module Ale.Hub.Mining.Server where

import Universum

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Control.Concurrent.STM.TMVar (TMVar, newTMVarIO, putTMVar, takeTMVar)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (withReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Monad.Capabilities (CapImpl (CapImpl), CapsT, HasCaps, HasNoCap, addCap, makeCap)
import System.Wlog (WithLoggerIO, logDebug, logWarning)
import UnliftIO.Async (withAsync)

import Ale.Capabilities (CapImplIO)
import Ale.Core.Block (Block, Bundle (Bundle), UnsignedBlock (..), mkPointer, signBlock)
import Ale.Core.Crypto (SecretKey)
import Ale.Core.Crypto.Signed (Signed)
import Ale.Core.DHT.Types (DHT, MonadDHT (..))
import Ale.Core.Message (Message)
import Ale.Fmt ((+|), (+||), (|+), (||+))
import Ale.Node.Context.Logging (Logging)
import Ale.Node.DB.LexiNum (LexiInt (LexiInt))
import Ale.Node.DB.Types (Collection (..), DB, MonadDB, listDBCollection, putDBCollection,
                          wipeDBCollection)
import Ale.Node.Mining.Types (MiningReply, MiningRequest (..))
import Ale.Node.Networking.Server (RawServerComponent, ServerComponent, fromRaw, serverReply)
import Ale.State (AleState, MonadAleState (..), applySignedMessage, getASBlockOrError,
                  getASDataOrError, getASHeightOrError)


data Mining m = Mining
    { _mGetNextBlock :: !(m Block)  -- ^ Get next freshly mined block
    }
makeCap ''Mining

-- | Start the Mining server subsystem.
withMining :: (MonadMask m, MonadUnliftIO m, MonadBaseControl IO m,
               HasCaps '[DB, DHT, Logging, AleState] caps,
               HasNoCap Mining caps)
           => SecretKey           -- ^ Server 'SecretKey'
           -> Int                 -- ^ Block interval
           -> RawServerComponent
           -> (CapImplIO Mining '[DB] -> CapsT (Mining : caps) m a)
           -> CapsT caps m a
withMining sk delay rsc cont = do
    mscMined <- liftIO newTChanIO

    LexiInt maxMsg <- maximumWithMin 0 . map fst <$> listDBCollection msgsCol

    -- Next vacant number for a message to be included in the next block.
    -- This variable is also used to synchronise DB access.
    nextBlockCtr <- liftIO $ newTMVarIO (maxMsg + 1)

    withAsync (receive nextBlockCtr) $ \_ ->
        withAsync (mine nextBlockCtr mscMined) $ \_ ->
            let miningImpl :: CapImplIO Mining '[DB]
                miningImpl = CapImpl $ Mining (atomically $ readTChan mscMined)
            in withReaderT (addCap miningImpl) (cont miningImpl)
  where
    msgsCol :: Collection (LexiInt, Signed Message) '[LexiInt] (Signed Message)
    msgsCol = Collection
        { cName      = "nextBlockMsgs"
        , cToKeys    = fst
        , cToItem    = snd
        , cRecollect = (,)
        }

    maximumWithMin :: Ord a => a -> [a] -> a
    maximumWithMin = foldl' max


    -- | Receive and validate a new message. If validation succeeds, put it into
    -- the list of awaiting messages.
    receive :: (WithLoggerIO m,
                MonadDHT m, MonadDB m, MonadAleState m)
            => TMVar Int
            -> m ()
    receive nextBlockCtr = forever $
        serverReply sc $ \req _rep ->
            case req of
                SendMessage smsg -> do
                    logDebug $ "Message: "+|smsg|+""
                    stateData <- getASDataOrError
                    h <- getASHeightOrError
                    esd <- runExceptT $ applySignedMessage (h + 1) smsg stateData
                    case esd of
                        Left err  ->
                            logWarning $ "Failed to apply msg: "+||err||+""
                        Right stateData' -> do
                            count <- atomically $ takeTMVar nextBlockCtr
                            putDBCollection msgsCol (LexiInt count, smsg)
                            atomically $ putTMVar nextBlockCtr (count + 1)
                            putASData stateData'

    -- | Once in 'blockInterval' collect all the awaiting messages
    -- and issue a new block containing them.
    mine :: (WithLoggerIO m, MonadBaseControl IO m,
             MonadDHT m, MonadDB m, MonadAleState m)
         => TMVar Int
         -> TChan Block
         -> m ()
    mine nextBlockCtr mined = forever $ do
        nextBlock <- atomically $ takeTMVar nextBlockCtr
        when (nextBlock > 1) $ do
            msgs <- map snd <$> listDBCollection msgsCol
            bRef <- putDHT (Bundle $ reverse msgs)
            prevBlock <- getASBlockOrError
            h <- getASHeightOrError
            let block = signBlock sk $ UnsignedBlock prevBlock bRef
            logDebug $ "New block is "+|block|+""
            atomically $ writeTChan mined block
            putASBlock $ mkPointer block
            putASHeight $ h + 1
            wipeDBCollection msgsCol
        atomically $ putTMVar nextBlockCtr 1
        liftIO $ threadDelay delay

    sc :: ServerComponent MiningRequest MiningReply
    sc = fromRaw rsc
