module Test.Ale.Core.Submission.Acceptance where

import Universum

import Hedgehog (Property, forAll, property)
import Test.HUnit (Assertion, assertBool)

import Ale.Core.Submission.Acceptance (alwaysFail, facile, isFacile)

import Test.Ale.Core.Submission.Acceptance.Gen (genAcceptanceTest)
import Test.Json (jsoning)
import Test.Serialise (serialising)

----------------------------------------
--- AcceptanceTest
----------------------------------------

unit_facileIsFacile :: Assertion
unit_facileIsFacile = assertBool "facile should be facile" (isFacile facile)

unit_alwaysFailIsNotFacile :: Assertion
unit_alwaysFailIsNotFacile = assertBool "alwaysPass should not be facile" (not $ isFacile alwaysFail)

hprop_AcceptanceSerialise :: Property
hprop_AcceptanceSerialise = property $ forAll genAcceptanceTest >>= serialising

hprop_AcceptanceJsonSerialise :: Property
hprop_AcceptanceJsonSerialise = property $ forAll genAcceptanceTest >>= jsoning
