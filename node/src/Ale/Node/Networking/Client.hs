-- | This module provides a somewhat type-safe interface to client networking.
module Ale.Node.Networking.Client
       ( ComponentId
       , ClientContext (..)
       , withClient

       , ClientComponent ()
       , fromRaw
       , clientRequest
       , clientReceive

       , RawClientComponent ()
       ) where

import Universum

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import System.Wlog (WithLoggerIO, logWarning)

import Ale.Fmt ((+||), (||+))
import Ale.Node.Networking.Internal (ClientContext (..), ComponentId, RawClientComponent,
                                     withClient)

import qualified Ale.Node.Networking.Internal as Raw

-- | Client. Networking context for a single component.
newtype ClientComponent reqT repT = ClientComponent RawClientComponent

-- | Fix request and reply types in a raw component.
fromRaw :: (Serialise reqT, Serialise repT)
                 => RawClientComponent -> ClientComponent reqT repT
fromRaw = ClientComponent


-- | Send a request to the server.
clientRequest :: (MonadIO m, Serialise reqT, Serialise repT)
              => ClientComponent reqT repT  -- ^ Client context
              -> reqT                       -- ^ Request to send
              -> m ()
clientRequest (ClientComponent cc) = Raw.clientRequest cc . serialise

-- | Receive a message from the server.
clientReceive :: (WithLoggerIO m, Serialise reqT, Serialise repT)
              => ClientComponent reqT repT  -- ^ Client context
              -> m repT
clientReceive r@(ClientComponent cc) = Raw.clientReceive cc >>= \bs ->
    case deserialiseOrFail bs of
        Left e  -> do
            logWarning $ "Invalid message: ("+||e||+"):"+||bs||+""
            clientReceive r
        Right a -> pure a
