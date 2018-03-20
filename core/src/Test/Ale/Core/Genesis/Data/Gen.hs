module Test.Ale.Core.Genesis.Data.Gen
       ( genGenesisData
       ) where

import Universum

import Hedgehog (MonadGen, Range)

import Ale.Core.Genesis.Data (GenesisData (GenesisData), defaultValidityPeriod)

import Test.Ale.Core.Crypto.Gen (genPk)
import Test.Ale.Core.Genesis.Distribution.Gen (genBalanceDistribution)

-- | Generate Genesis data.
genGenesisData :: MonadGen m => Range Int -> m GenesisData
genGenesisData range = GenesisData <$> genBalanceDistribution range <*> genPk
    <*> pure defaultValidityPeriod
