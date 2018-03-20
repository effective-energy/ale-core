module Test.Ale.Core.Requirements where

import Universum

import Codec.Serialise (serialise)
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray (ByteArray, ByteArrayAccess, convert)

import Hedgehog (Property, forAll, property, (===))
import Test.HUnit (Assertion, (@?=))

import Ale.Core.Crypto (sign)
import Ale.Core.DHT.Pure (evalPureDHT)
import Ale.Core.Requirements

import Test.Ale.Core.Crypto.Gen (genKeyPair, genSignature)
import Test.Ale.Core.Entity.Gen (genEntity)
import Test.Ale.Core.Requirements.Gen (genReqProof, genRequirements)
import Test.Json (jsoning)
import Test.Serialise (serialising)

import qualified Ale.Data.HashMap as HM
import qualified Data.ByteString.Lazy as BSL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Serokell.Util.Base16 as B16

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

hprop_RequirementsSerialise :: Property
hprop_RequirementsSerialise = property $
    forAll (genRequirements $ Range.linear 0 5) >>= serialising

hprop_RequirementsJsonSerialise :: Property
hprop_RequirementsJsonSerialise = property $
    forAll (genRequirements $ Range.linear 0 5) >>= jsoning

hprop_ReqProofSerialise :: Property
hprop_ReqProofSerialise = property $
    forAll (genReqProof $ Range.linear 0 5) >>= serialising

hprop_ReqProofJsonSerialise :: Property
hprop_ReqProofJsonSerialise = property $
    forAll (genReqProof $ Range.linear 0 5) >>= jsoning


unit_validateTrue :: Assertion
unit_validateTrue = validatePure emptyProof (RConst True) @?= True

unit_validateFalse :: Assertion
unit_validateFalse = validatePure emptyProof (RConst False) @?= False

-- | Check that @sha256(empty_string)@ is valid.
unit_validateSha256Empty :: Assertion
unit_validateSha256Empty = validatePure emptyProof (REqBs
    (RConst $ ReqByteString $ fromRight "" $ B16.decode "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    (RSha256 $ RConst "")) @?= True

hprop_validateAnd :: Property
hprop_validateAnd = property $ do
    bools <- forAll $ Gen.list (Range.linear 0 10)
                               (Gen.frequency [(1, pure False), (99, pure True)])

    let expr = RAnd $ map RConst bools
    validatePure emptyProof expr === and bools

hprop_validateLess :: Property
hprop_validateLess = property $ do
    i1 <- forAll $ Gen.integral (Range.constant (-100) 100)
    i2 <- forAll $ Gen.integral (Range.constant (-100) 100)

    let expr = RLess (RConst i1) (RConst i2)
    validatePure emptyProof expr === (i1 < i2)

hprop_validateSignEmpty :: Property
hprop_validateSignEmpty = property $ do
    ent <- forAll genEntity
    bs <- forAll $ Gen.bytes (Range.linear 0 128)

    let expr = RVerify (RConst $ ReqByteString bs) (RConst ent) (RVar 1)
    validatePure emptyProof expr === False

hprop_validateSha256 :: Property
hprop_validateSha256 = property $ do
    bs <- forAll $ Gen.bytes (Range.linear 0 128)

    let expr = REqBs (RConst $ sha256 bs) (RSha256 $ RVar 1)
    let proof = HM.fromList [(1, Proof $ BSL.toStrict $ serialise $ ReqByteString bs)]
    validatePure proof expr === True

hprop_validateDoubleSha256 :: Property
hprop_validateDoubleSha256 = property $ do
    bs <- forAll $ Gen.bytes (Range.linear 0 128)

    let expr = REqBs
            (RConst $ sha256 (sha256 bs :: ByteString))
            (RSha256 $ RSha256 $ RVar 1)
    let proof = HM.fromList [(1, Proof $ BSL.toStrict $ serialise $ ReqByteString bs)]
    validatePure proof expr === True

hprop_validateSignValid :: Property
hprop_validateSignValid = property $ do
    (sk, ent) <- forAll genKeyPair
    bs <- forAll $ Gen.bytes (Range.linear 0 128)

    let expr = RVerify (RConst $ ReqByteString bs) (RConst ent) (RVar 1)

    let sig = sign sk bs
    let proof = HM.fromList [(1, Proof $ BSL.toStrict $ serialise sig)]
    validatePure proof expr === True

hprop_validateSignInvalid :: Property
hprop_validateSignInvalid = property $ do
    (sk, ent) <- forAll genKeyPair
    bs <- forAll $ Gen.bytes (Range.linear 0 128)

    let expr = RVerify (RConst $ ReqByteString bs) (RConst ent) (RVar 1)

    sig <- forAll genSignature
    guard $ sig /= sign sk bs

    let proof = HM.fromList [(1, Proof $ BSL.toStrict $ serialise sig)]
    validatePure proof expr === False

-----------------------
-- Helpers
-----------------------

-- | Validate expression in empty DHT with not proof
validatePure :: ReqProof -> Requirements -> Bool
validatePure p req = evalPureDHT (validate p req)

sha256 :: (ByteArrayAccess a, ByteArray b) => a -> b
sha256 bs' = convert (hash bs' :: Digest SHA256)
