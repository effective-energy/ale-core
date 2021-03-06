{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE Rank2Types #-}

-- | Internals of the Ale networking subsystem.
--
-- The internals of networking are not typed, which means that we send and receive
-- 'ByteString' (the lazy one). After the user initialises his client or server by
-- calling 'clientStart' or 'serverStart', they get a bunch of /raw/ components.
-- The functions that send and receive on these are not exported intentionally;
-- the user is expected to convert these raw components into typed ones and use
-- the functions from "Ale.Node.Networking.Client" or "Ale.Node.Networking.Server"
-- respectively.
--
-- The internal wire format for raw messages is just @(cid, bytes)@,
-- where @cid@ is the identifier of the component that sent the message.
-- This pair is being serialised using 'serialise' because it seems to be
-- the simplest way to reliably send a pair over our chosen transport.
module Ale.Node.Networking.Internal
       ( ComponentId

       , RawClientComponent (..)
       , RawClientComponentMap
       , ClientContext (..)
       , ServerContext (..)
       , withClient
       , clientRequest
       , clientReceive

       , RawServerComponent (..)
       , RawServerComponentMap
       , withServer
       , serverReply
       ) where

import Universum hiding (HashMap)

import Async.Combinators (withWorker)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, asyncThreadId, link, pollSTM, uninterruptibleCancel,
                                 waitCatch)
import Control.Concurrent.STM (retry)
import Control.Concurrent.STM.TChan (TChan, isEmptyTChan, newTChanIO, readTChan, writeTChan)
import Control.Exception (AsyncException (ThreadKilled), ioError)
import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Except (except)
import Data.ByteString.Lazy (fromChunks, toChunks)
import Data.Hashable (Hashable (hash, hashWithSalt))
import Data.Word (Word16)
import Network.Socket (HostName)
import Network.Transport (Connection (close, send), ConnectionId,
                          EndPoint (closeEndPoint, connect, receive), EndPointAddress (..),
                          Event (..), Reliability (ReliableOrdered), SendErrorCode (SendFailed),
                          Transport (closeTransport, newEndPoint), TransportError (TransportError),
                          defaultConnectHints)
import Network.Transport.TCP (TCPAddr (Unaddressable), createTransport, defaultTCPAddr,
                              defaultTCPParameters)
import System.IO.Error (userError)
import System.Wlog (LoggerNameBox, WithLoggerIO, logDebug, logError, logWarning)
import UnliftIO.Async (wait, withAsync)

import Ale.Data.HashMap (HashMap)
import Ale.Fmt ((+||), (||+))
import Ale.Log (asyncLog, liftLogIO2, withAsyncLog)

import qualified Ale.Data.HashMap as HM
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified ListT (toList)
import qualified STMContainers.Set as STMSet (Set, delete, deleteAll, insert, new, stream)

instance Hashable (Async a) where
    hash = hash . asyncThreadId
    hashWithSalt s a = hashWithSalt s (asyncThreadId a)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Component identifier used for routing messages.
type ComponentId = ByteString

-- | Heartbeat component is added to ensure TCP connetction between
-- client and server will not stall.
--
-- Network-Transport provides an attempt to solve this problem:
-- 'TCPParameters' have 'tcpKeepAlive' parameter that sets
-- @SO_KEEPALIVE@ socket option.  It could potentially solve the
-- problem but it requires OS tuning: default keepalive timeout is set
-- to 2 hours on Linux which is absolutely unacceptable.
heartbeatId :: ComponentId
heartbeatId = "__heartbeat"

-----------------------
-- Client
-----------------------

type RawClientComponentMap = HashMap ComponentId RawClientComponent

type AsyncResult = Either (TransportError SendErrorCode) ()

data ClientContext = ClientContext
    { ccComponents :: [RawClientComponent]
    , ccSendingSet :: STMSet.Set (Async AsyncResult)
    }

-- | Client. Networking context for a single component.
data RawClientComponent = RawClientComponent
    { ccId       :: !ComponentId
    , ccInChan   :: !(TChan BSL.ByteString)
    , ccOutChan  :: !(TChan (ComponentId, BSL.ByteString))
    , ccStopping :: !(TVar Bool)
    }

-- | Start a client.
--
-- This function returns a list containing /raw/ component contexts
-- each of which should be converted to a typed one, passed
-- to the respective component and used there for network communication.
withClient :: (MonadMask m, WithLoggerIO m, MonadUnliftIO m)
           => HostName       -- ^ Server host name
           -> Word16         -- ^ Server port
           -> [ComponentId]  -- ^ List of components to start
           -> Int            -- ^ Timeout on gracefully stopping
           -> (ClientContext -> m a) -> m a
withClient host port cids timeout cont = do

    -- Create a TCP endpoint.
    tr <- liftIO $ exceptIO show $ createTransport Unaddressable defaultTCPParameters
    ep <- liftIO $ exceptIO teText $ newEndPoint tr

    -- Setup connection to the server for sending outgoing messages.
    upCon <- liftIO $ exceptIO teText $ connect ep remoteEnd ReliableOrdered defaultConnectHints
    -- Channel for queuing outgoing messages. Components will write directly to it.
    outChan <- liftIO newTChanIO

    -- Is the networking shutting down
    stopping <- newTVarIO False
    -- STM Set that stores unfinished send requests
    sendingSet <- atomically STMSet.new
    -- Create channels for routing incoming messages to components.
    m <- foldlM (setupComponent outChan stopping) HM.empty (heartbeatId:cids)

    let runRouteIncoming = liftLogIO2 finally
                               (routeIncoming ep m)
                               (logDebug "Stopping routeIncoming")

    let runForwardOutgoing = liftLogIO2 finally
                                 (forwardOutgoing upCon outChan sendingSet)
                                 (logDebug "Stopping forwardOutgoing")

    let heartbeatComp = fromMaybe
            (error "impossible: heartbeat component was not inserted to map")
            (HM.lookup heartbeatId m)

    withWorker "net-hearbeat" (sendHeartbeats heartbeatComp) $
        withAsync runRouteIncoming $ \aRouteIncoming -> do
            -- exception propagation
            liftIO $ link aRouteIncoming
            withWorker "net-forwardOutgoing" runForwardOutgoing $ do
                res <- cont ClientContext
                    { ccComponents = catMaybes [HM.lookup cid m | cid <- cids]
                    , ccSendingSet = sendingSet
                    }

                atomically $ writeTVar stopping True
                -- give node some time to gracefully terminate
                withAsyncLog (liftIO (threadDelay timeout) >> killAll sendingSet)
                    $ const $ do
                        -- firstly wait for all the queues to become empty
                        waitForEmptyQueues m
                        waitUnfinished sendingSet

                liftIO $ closeEndPoint ep
                liftIO $ closeTransport tr

                wait aRouteIncoming

                pure res

  where
    remoteEnd = EndPointAddress $ BS8.pack host <> ":" <> show port <> ":0"

    setupComponent :: WithLoggerIO m
                   => TChan (ComponentId, BSL.ByteString)
                   -> TVar Bool
                   -> RawClientComponentMap
                   -> ComponentId
                   -> m RawClientComponentMap
    setupComponent outChan stopping m cid = do
        -- Channel to put incoming messages for this component into.
        inChan <- liftIO newTChanIO
        pure $ HM.alter (insertNew (RawClientComponent cid inChan outChan stopping)) cid m

    routeIncoming :: (MonadThrow m, WithLoggerIO m)
                  => EndPoint
                  -> HashMap ComponentId RawClientComponent
                  -> m ()
    routeIncoming ep m = loop
      where
        loop = do
            evt <- liftIO $ receive ep
            case evt of
                Received _ bs -> do
                    case lookupDestination bs m of
                        Left e          -> logError $ "Error: ("+||e||+") "+||bs||+""
                        Right (cc, bs') -> atomically $ writeTChan (ccInChan cc) bs'
                    loop
                ConnectionClosed {} -> loop
                ConnectionOpened {} -> loop
                ReceivedMulticast {} -> loop
                EndPointClosed -> throwM ThreadKilled
                ErrorEvent evnt -> do
                    logError $ "Error : ("+||evnt||+")"
                    throwM evnt

    forwardOutgoing :: forall m . (WithLoggerIO m, MonadMask m)
                    => Connection
                    -> TChan (ComponentId, BSL.ByteString)
                    -> STMSet.Set (Async AsyncResult)
                    -> m ()
    forwardOutgoing upCon outChan sendingSet = loop
      where
        loop :: m ()
        loop = do
            (cid, a) <- atomically $ readTChan outChan

            let sending :: LoggerNameBox IO AsyncResult
                sending = liftIO $ send upCon $ toChunks $ serialise (cid, a)
            asyncSending <- asyncLog sending
            -- TODO: https://github.com/serokell/ale-core/pull/101#discussion_r163435324
            atomically $ STMSet.insert asyncSending sendingSet
            -- run second thread to wait first
            void $ asyncLog $ do
                whenLeftM (liftIO $ waitCatch asyncSending) (\e -> logError $ "Error: ("+||e||+")")
                atomically $ STMSet.delete asyncSending sendingSet
            loop

    -- | This function operates on raw (not typed which are exposed to
    -- user) components for a following reason.  We want to hide from
    -- user the fact that there exists some service that maintains TCP
    -- connectivity: users should start only services they want.  This
    -- requires us to hide this details in 'serverStart' and
    -- 'clientStart' functions themselves: after this function are run
    -- connection is maintained automatically.  On the other hand,
    -- this 'clientRequest'/'serverReply' approach is quite handy and
    -- worth reusing.  This requirements force us to use raw
    -- component: at this time typed components are not defined yet
    -- and can not be imported because it will cause cyclic
    -- dependency.  We don't want to send anything meaningful anyway
    -- so losing typing is not a big deal.
    sendHeartbeats :: WithLoggerIO m
                   => RawClientComponent
                   -> m ()
    sendHeartbeats rcc = forever $ do
        clientRequest rcc ""
        logDebug "Heartbeat sent"
        liftIO $ threadDelay 30000000 -- every 30 seconds


    waitForEmptyQueues :: MonadIO m => RawClientComponentMap -> m ()
    waitForEmptyQueues m = atomically $
        unlessM (allM (isEmptyTChan . ccOutChan) (elems m)) retry

-- | Send a request to the server.
clientRequest :: MonadIO m
              => RawClientComponent  -- ^ Client context
              -> BSL.ByteString      -- ^ Request to send
              -> m ()
clientRequest cc req = atomically $ do
    stopping <- readTVar (ccStopping cc)
    -- TODO should we notify sender if message was discarded?
    unless stopping $ writeTChan (ccOutChan cc) (ccId cc, req)

-- | Receive a message from the server.
clientReceive :: MonadIO m
              => RawClientComponent  -- ^ Client context
              -> m BSL.ByteString
clientReceive cc = atomically $ readTChan (ccInChan cc)


-----------------------
-- Server
-----------------------

type RawServerComponentMap = HashMap ComponentId RawServerComponent

-- | Server. Networking context for a single component.
data RawServerComponent = RawServerComponent
    { scId       :: !ComponentId
    , scInChan   :: !(TChan (EndPointAddress, BSL.ByteString))
    , scOutChan  :: !(TChan (EndPointAddress, (ComponentId, BSL.ByteString)))
    , scStopping :: !(TVar Bool)
    }

data ServerContext = ServerContext
    { scComponents :: [RawServerComponent]
    , scSendingSet :: STMSet.Set (Async AsyncResult)
    }

-- | Start a server.
--
-- This function returns a list containing /raw/ component contexts
-- each of which should be converted to a typed one, passed
-- to the respective component and used there for network communication.
withServer :: (MonadMask m, WithLoggerIO m, MonadUnliftIO m)
           => HostName       -- ^ Listen host name
           -> Word16         -- ^ Listen port
           -> [ComponentId]  -- ^ List of components to start
           -> Int            -- ^ Timeout on gracefully stopping
           -> (ServerContext -> m a)
           -> m a
withServer host port cids timeout cont = do
    -- Create a TCP endpoint.
    tr <- liftIO $ exceptIO show $ createTransport listenAddr defaultTCPParameters
    ep <- liftIO $ exceptIO teText $ newEndPoint tr

    -- Is the networking shutting down
    stopping <- newTVarIO False
    -- Channel for queuing outgoing messages. Components will write directly to it.
    outChan <- liftIO newTChanIO
    -- STM Set that stores unfinished send requests
    sendingSet <- atomically STMSet.new

    -- Create channels for routing incoming messages to components.
    m <- foldlM (setupComponent outChan stopping) HM.empty (heartbeatId:cids)

    let heartbeatComp = fromMaybe
            (error "impossible: heartbeat component was not inserted to map")
            (HM.lookup heartbeatId m)

    withWorker "net-hearbeat" (receiveHeartbeats heartbeatComp) $
        withAsync (routeIncoming ep m) $ \aRouteIncoming ->
            withWorker "net-forwardOutgoing" (forwardOutgoing ep outChan sendingSet) $ do
                res <- cont ServerContext
                    { scComponents = catMaybes [HM.lookup cid m | cid <- cids]
                    , scSendingSet = sendingSet
                    }
                atomically $ writeTVar stopping True

                -- give node some time to gracefully terminate
                withAsyncLog (liftIO (threadDelay timeout) >> killAll sendingSet)
                    $ const $ do
                        -- firstly wait for all the queues to become empty
                        waitForEmptyQueues m
                        waitUnfinished sendingSet

                liftIO $ closeEndPoint ep
                liftIO $ closeTransport tr

                wait aRouteIncoming

                pure res
  where
    listenAddr = defaultTCPAddr host (show port)

    setupComponent :: WithLoggerIO m
                   => TChan (EndPointAddress, (ComponentId, BSL.ByteString))
                   -> TVar Bool
                   -> RawServerComponentMap
                   -> ComponentId
                   -> m RawServerComponentMap
    setupComponent outChan stopping m cid = do
        -- Channel to put incoming messages for this component into.
        inChan <- liftIO newTChanIO
        pure $ HM.alter (insertNew (RawServerComponent cid inChan outChan stopping)) cid m

    routeIncoming :: WithLoggerIO m
                  => EndPoint
                  -> HashMap ComponentId RawServerComponent
                  -> m ()
    routeIncoming ep m = loop HM.empty
      where
        loop :: WithLoggerIO m
             => HashMap ConnectionId EndPointAddress
             -> m ()
        loop conMap = do
            evt <- liftIO $ receive ep
            case evt of
                Received con bs -> do
                    case lookupDestination bs m of
                        Left e -> logError $ "Error: ("+||e||+") "+||bs||+""
                        Right (sc, bs') -> case HM.lookup con conMap of
                            Nothing -> logError "Impossible happened"
                            Just ea -> atomically $ writeTChan (scInChan sc) (ea, bs')
                    loop conMap
                ConnectionClosed con -> loop (HM.delete con conMap)
                ConnectionOpened con _ ea -> loop (HM.insert con ea conMap)
                ReceivedMulticast _ _ -> loop conMap
                EndPointClosed -> pass -- terminate
                ErrorEvent e -> do
                    logError $ "Error: ("+||e||+")"
                    loop conMap

    forwardOutgoing :: forall m . (WithLoggerIO m, MonadMask m)
                    => EndPoint
                    -> TChan (EndPointAddress, (ComponentId, BSL.ByteString))
                    -> STMSet.Set (Async AsyncResult)
                    -> m ()
    forwardOutgoing ep outChan sendingSet = loop
      where
        loop :: m ()
        loop = do
            (ea, (cid, a)) <- atomically $ readTChan outChan
            let sending :: LoggerNameBox IO AsyncResult
                sending = do
                    mcon <- liftIO $ connect ep ea ReliableOrdered defaultConnectHints
                    case mcon of
                        Left e -> do
                            logError $ "Error: ("+||e||+")"
                            pure $ Left (TransportError SendFailed "Fail to connect")
                        Right con -> let sendAction = send con . toChunks $ serialise (cid, a)
                                     in  liftIO $ sendAction `finally` close con
            asyncSending <- asyncLog sending
            -- TODO: https://github.com/serokell/ale-core/pull/101#discussion_r163435324
            atomically $ STMSet.insert asyncSending sendingSet
            -- run second thread to wait first
            void $ asyncLog $ do
                whenLeftM (liftIO $ waitCatch asyncSending) (\e -> logError $ "Error: ("+||e||+")")
                atomically $ STMSet.delete asyncSending sendingSet
            loop

    receiveHeartbeats :: WithLoggerIO m
                      => RawServerComponent
                      -> m ()
    receiveHeartbeats rsc = forever $ serverReply rsc $ \_req _rep ->
        logDebug "received heartbeat"

    waitForEmptyQueues :: MonadIO m => RawServerComponentMap -> m ()
    waitForEmptyQueues m = atomically $
        unlessM (allM (isEmptyTChan . scOutChan) (elems m)) retry

-- | Receive a request from a client and send a reply.
serverReply :: MonadIO m
            => RawServerComponent
            -> (BSL.ByteString -> (BSL.ByteString -> m ()) -> m ())
            -- ^ Request handler that takes as a parameter a function
            -- that sends a reply back to the client.
            -> m ()
serverReply sc cont = do
    (ea, req) <- atomically $ readTChan (scInChan sc)
    cont req $ \rep -> atomically $ do
        stopping <- readTVar (scStopping sc)
        -- TODO should we notify sender if message was discarded?
        unless stopping $ writeTChan (scOutChan sc) (ea, (scId sc, rep))



-----------------------
-- Internals
-----------------------

-- | Run an 'IO' that may fail and throw the error as an exception.
exceptIO :: MonadIO m
         => (a -> Text)
         -> m (Either a b)
         -> m b
exceptIO f m = do
    eres <- m
    case eres of
        Left e  -> liftIO . ioError . userError . toString . f $ e
        Right r -> pure r

-- | Extract 'Text' from a 'TransportError'.
teText :: TransportError a -> Text
teText (TransportError _ s) = toText s

-- | Insert a new value into a map, throwing an error on duplicate key.
insertNew :: v -> Maybe v -> Maybe v
insertNew _ (Just _) = error "Networking.insertNew: Duplicate key"
insertNew v Nothing  = Just v

-- | Given a message received from a client, find the destination component.
lookupDestination :: [ByteString]           -- ^ Received message
                  -> HashMap ComponentId d  -- ^ Component id map
                  -> Either Text (d, BSL.ByteString)
lookupDestination bs m = runExcept $ do
    (cid, bs') <- splitted
    comp <- except $ maybeToRight "Invalid cid" (HM.lookup cid m)
    pure (comp, bs')
  where
    -- The wire format is @(cid, bytes)@.
    splitted :: Except Text (ComponentId, BSL.ByteString)
    splitted = case deserialiseOrFail (fromChunks bs) of
        Left  _ -> throwError "Malformed message"
        Right v -> pure v

-- kill all unfinished threads
killAll :: (WithLoggerIO m, MonadMask m)
        => STMSet.Set (Async AsyncResult)
        -> m ()
killAll sendingSet = do
    logWarning "Start killing sending threads.."
    cancels <- atomically $ ListT.toList (STMSet.stream sendingSet)
    liftIO $ traverse_ uninterruptibleCancel cancels
    -- flush set with sending requests
    atomically $ STMSet.deleteAll sendingSet

waitUnfinished :: MonadIO m
               => STMSet.Set (Async AsyncResult)
               -> m ()
waitUnfinished sendingSet = atomically $
    unlessM condition retry
      where
        condition :: STM Bool
        condition = do
            stms         <- ListT.toList $ STMSet.stream sendingSet
            threadStates <- traverse pollSTM stms
            let allSendingThreadsAreDead = all isJust threadStates
            pure allSendingThreadsAreDead
