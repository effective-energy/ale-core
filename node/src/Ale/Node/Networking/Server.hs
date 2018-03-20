{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides a somewhat type-safe interface to server networking.

module Ale.Node.Networking.Server
       ( ComponentId
       , withServer

       , ServerComponent ()
       , ServerContext (..)
       , fromRaw
       , serverReply

       , RawServerComponent ()
       ) where

import Universum

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import System.Wlog (WithLoggerIO, logError)

import Ale.Fmt ((+||), (||+))
import Ale.Node.Networking.Internal (ComponentId, RawServerComponent, ServerContext (..),
                                     withServer)

import qualified Data.ByteString.Lazy as BSL

import qualified Ale.Node.Networking.Internal as Raw

-- | Server. Networking context for a single component.
newtype ServerComponent reqT repT = ServerComponent RawServerComponent

-- | Fix request and reply types in a raw component.
fromRaw :: (Serialise reqT, Serialise repT)
                 => RawServerComponent -> ServerComponent reqT repT
fromRaw = ServerComponent


-- | Receive a request from a client and send a reply.
serverReply :: forall m reqT repT . (WithLoggerIO m, Serialise reqT, Serialise repT)
            => ServerComponent reqT repT
            -> (reqT -> (repT -> m ()) -> m ())
            -- ^ Request handler that takes as a parameter a function
            -- that sends a reply back to the client.
            -> m ()
serverReply r@(ServerComponent sc) f = Raw.serverReply sc f'
  where
    f' :: BSL.ByteString -> (BSL.ByteString -> m ()) -> m ()
    f' bs go = case deserialiseOrFail bs of
                 Left e -> do
                     logError $ "Invalid message: ("+||e||+") "+||bs||+""
                     serverReply r f
                 Right a  -> f a (go . serialise)
