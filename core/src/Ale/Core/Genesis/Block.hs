-- | Create Genesis Block
module Ale.Core.Genesis.Block
       ( initialState
       , genesisBlockPointer
       ) where

import Universum

import Ale.Core.Block (BlockPointer, unsafeGenesisPointer)
import Ale.Core.Genesis.Data (GenesisData, initialStateData)
import Ale.Core.Storage.Types (mkReference)
import Ale.State (MonadAleState (..))

-- | Create initial 'AleState' given 'GenesisData'.
initialState :: (Monad m, MonadAleState m)
             => GenesisData
             -> m ()
initialState genData = do
    putASBlock $ genesisBlockPointer genData
    putASHeight 0
    putASData $ initialStateData genData


-- | Derive the genesis block pointer from genesis data.
genesisBlockPointer :: GenesisData -> BlockPointer
genesisBlockPointer = unsafeGenesisPointer . mkReference
