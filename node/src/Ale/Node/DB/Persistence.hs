-- | Node Persistence layer.

module Ale.Node.DB.Persistence
    ( stateDBImpl
    ) where

import Universum

import Monad.Capabilities (CapImpl (CapImpl))

import Ale.Capabilities (CapImplIO)
import Ale.Node.DB.Types (DB, MonadDB, Persistable, getDB', putDB')
import Ale.State (AleState (..))


asBlockKey :: ByteString
asBlockKey = "asBlockKey"

asHeightKey :: ByteString
asHeightKey = "asHeightKey"

asASDataKey :: ByteString
asASDataKey = "asASDataKey"

stateDBImpl :: CapImplIO AleState '[DB]
stateDBImpl = CapImpl AleState
    { _getASBlock = getDB'' asBlockKey
    , _putASBlock = putDB' asBlockKey

    , _getASHeight = getDB'' asHeightKey
    , _putASHeight = putDB' asHeightKey

    , _getASData = getDB'' asASDataKey
    , _putASData = putDB' asASDataKey
    }
  where
    getDB'' :: (Persistable v, Monad m, MonadDB m) => ByteString -> m (Either Text v)
    getDB'' = fmap (first show) . getDB'
