-- | This module provides messages that blockchain subsystem use to
-- interact between client and server.

module Ale.Node.Blockchain.Types
       ( BlockchainRequest (..)
       , BlockchainReply (..)
       ) where

import Universum

import Codec.Serialise (Serialise)

import Ale.Core.Block (Block, BlockPointer)
import Ale.Core.Crypto.Signed (Signed)

data BlockchainRequest
    = GetBlock BlockPointer
      -- ^ @node@ requests block content.
    | GetHead
      -- ^ @node@ asks which block is currently last.
    | GetHeadOnUpdate
    deriving (Generic, Show)

instance Serialise BlockchainRequest where

data BlockchainReply
    = BlockContent (Signed Block)
      -- ^ @hub@ replies with block content.
    | NoSuchBlock BlockPointer
      -- ^ @node@ requested invalid 'BlockPointer'.
    | Head (Signed BlockPointer)
      -- ^ Last block was updated.
    deriving (Generic, Show)

instance Serialise BlockchainReply where
