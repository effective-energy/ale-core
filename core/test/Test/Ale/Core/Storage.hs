module Test.Ale.Core.Storage where

import Universum

import Hedgehog (Property, forAll, property)
import Test.HUnit (Assertion, assertBool)

import Codec.Serialise (deserialiseOrFail, serialise)

import Ale.Core.Storage.Types (Reference, mkReference)

import Test.Serialise (serialising)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

----------------------------------------
--- Reference
----------------------------------------

unit_ReferenceDeserialiseFailure :: Assertion
unit_ReferenceDeserialiseFailure = assertBool "bad digest" $ isLeft
    (deserialiseOrFail @(Reference ByteString) $ serialise ("" :: ByteString))

hprop_ReferenceSerialise :: Property
hprop_ReferenceSerialise = property $ forAll genRef >>= serialising
  where
    genRef = mkReference <$> Gen.utf8 (Range.linear 0 100) Gen.digit
