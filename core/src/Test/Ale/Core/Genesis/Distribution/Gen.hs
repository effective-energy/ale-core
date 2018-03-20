module Test.Ale.Core.Genesis.Distribution.Gen
       ( genBalanceDistribution
       ) where

import Universum

import Hedgehog (MonadGen, Range)

import Ale.Core.Genesis.Distribution (BalanceDistribution, mkOwnUniform)

import Test.Ale.Core.Entity.Gen (genEntity)
import Test.Ale.Core.Tokens.Gen (genTokenCount)

import qualified Hedgehog.Gen as Gen


-- | Generate a uniform token distribution.
-- 'Range' is used to determine the number of entities who have tokens.
genBalanceDistribution :: MonadGen m => Range Int -> m BalanceDistribution
genBalanceDistribution range = mkOwnUniform <$> Gen.list range genEntity <*> genTokenCount
