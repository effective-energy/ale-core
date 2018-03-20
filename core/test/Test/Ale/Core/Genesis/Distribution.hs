module Test.Ale.Core.Genesis.Distribution where

import Universum

import Hedgehog (Property, forAll, property)

import Ale.Core.Genesis.Distribution ()

import Test.Ale.Core.Genesis.Distribution.Gen (genBalanceDistribution)
import Test.Json (jsoning)

import qualified Hedgehog.Range as Range


----------------------------------------
--- BalanceDistribution
----------------------------------------

hprop_DistributionSerialise :: Property
hprop_DistributionSerialise = property $ forAll genDistr >>= jsoning
  where
    genDistr = genBalanceDistribution $ Range.linear 0 50

hprop_DistributionJsonSerialise :: Property
hprop_DistributionJsonSerialise = property $ forAll genDistr >>= jsoning
  where
    genDistr = genBalanceDistribution $ Range.linear 0 50
