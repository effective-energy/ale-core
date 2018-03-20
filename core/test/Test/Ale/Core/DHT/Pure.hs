module Test.Ale.Core.DHT.Pure where

import Universum

import Hedgehog (Gen, Property, PropertyT, forAll, property)

import Control.Monad.Morph (hoist)

import Ale.Core.DHT.Pure (PureDHT, runPureDHT)
import Ale.State.Pure (PureAleStateT, emptyAleState, runPureAleStateT)

import Test.Ale.Core.DHT (getBeforePut, putGet, putGetMany)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | This function allows running tests having 'MonadDHT' in context.
-- It allows writing general 'MonadDHT' tests and select actual
-- implementation later.
propInPureDHT :: PropertyT PureDHT a -> Property
propInPureDHT = property . void . hoist (pure . fst . runPureDHT)

propInPureDHTState :: PropertyT (PureAleStateT PureDHT) a -> Property
propInPureDHTState = property . void . hoist (pure . fst . fst . runPureDHT . runPureAleStateT emptyAleState)

----------------------------------------
--- MonadDHT
----------------------------------------

hprop_purePutGet :: Property
hprop_purePutGet = propInPureDHT $ forAll genBS >>= putGet

hprop_getBeforePut :: Property
hprop_getBeforePut = propInPureDHT $ forAll genBS >>= getBeforePut

hprop_putGetMany :: Property
hprop_putGetMany = propInPureDHT $ forAll (Gen.list (Range.linear 0 100) genBS) >>= putGetMany

------------------------------
--- Internal function
------------------------------

genBS :: Gen ByteString
genBS = Gen.bytes (Range.linear 0 100)
