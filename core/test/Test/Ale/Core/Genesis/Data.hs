module Test.Ale.Core.Genesis.Data where

import Universum

import Hedgehog (Property, forAll, property)

import Ale.Core.Genesis.Data ()

import Test.Ale.Core.Genesis.Data.Gen (genGenesisData)
import Test.Serialise (serialising)

import qualified Hedgehog.Range as Range

----------------------------------------
--- GenesisData
----------------------------------------

hprop_GenesisDataSerialise :: Property
hprop_GenesisDataSerialise = property $ forAll genData >>= serialising
  where
    genData = genGenesisData $ Range.linear 0 50
