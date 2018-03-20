-- | Network message types for the Mining component.

module Ale.Node.Mining.Types
       ( MiningRequest(..)
       , MiningReply
       ) where

import Universum

import Codec.Serialise (Serialise)

import Ale.Core.Crypto.Signed (Signed)
import Ale.Core.Message (Message)


data MiningRequest
    = SendMessage (Signed Message)
    deriving (Generic, Show)

instance Serialise MiningRequest where


data MiningReply
    deriving (Generic)

instance Serialise MiningReply where
