module Test.Ale.Core.Block.Gen
       ( genBlock
       , genBundle
       ) where

import Universum

import Hedgehog (MonadGen)

import Ale.Core.Block (Block (Block), Bundle (Bundle))
import Ale.Core.DHT.Types (MonadDHT (putDHT))
import Ale.Core.Genesis.Block (initialState)
import Ale.State (MonadAleState, getASBlockOrError)

import Test.Ale.Core.Crypto.Gen (genSignature)
import Test.Ale.Core.Genesis.Data.Gen (genGenesisData)

import qualified Hedgehog.Range as Range


-- | Generate a random block.
genBlock :: (MonadAleState m, MonadDHT m, MonadGen m) => m Block
genBlock = do
    genData <- genGenesisData (Range.linear 0 50)
    initialState genData
    blockPtr <- getASBlockOrError
    bundleRef <- genBundle >>= putDHT
    -- FIXME: sign the block with the real key
    sig <- genSignature
    pure $ Block blockPtr bundleRef sig

genBundle :: MonadGen m => m Bundle
genBundle = pure $ Bundle []  -- TODO: generator for Bundle
