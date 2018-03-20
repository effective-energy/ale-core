module Test.Ale.Core.Crypto where

import Universum

import Data.ByteString (ByteString)
import Data.Maybe (fromJust)

import Hedgehog (Property, assert, forAll, property, (===))
import Test.HUnit (Assertion, assertBool, (@?=))

import Ale.Core.Crypto

import Test.Ale.Core.Crypto.Gen (genPk, genSignature)
import Test.Json (jsoning)
import Test.Serialise (serialising)

import qualified Data.ByteString.Lazy as BSL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Serokell.Util.Base64 as SB64


--------------------------------------------
---- Test data
--------------------------------------------

skBytes :: ByteString
skBytes = "&\DLE\130\226\198E#\241\133\220\207D\ENQ\145\160'p\STX\129W\138\246G\219\SUB\162\180\165\227\220Ey"

pkBytes :: ByteString
pkBytes = "Y\130l?b\246\243\205\208\ACK\161\SOH\235\&2\175\226\144\248\139\206:B\163MDJnv\158UX\182"

pkB64 :: Text
pkB64 = "WYJsP2L2883QBqEB6zKv4pD4i846QqNNREpudp5VWLY="


--------------------------------------------
--------------------------------------------
--------------------------------------------


-----------------------
-- Secret key
-----------------------

sk :: SecretKey
sk = fromJust $ secretFromBytes skBytes

unit_secretFromBytes :: Assertion
unit_secretFromBytes = assertBool "sec parse failed" (isJust $ secretFromBytes skBytes)

unit_toPublic :: Assertion
unit_toPublic = toPublic sk @?= pk

hprop_secretBadFromBytes :: Property
hprop_secretBadFromBytes = property $ do
    bs <- forAll $ Gen.bytes $ Range.linear 0 (secretSize - 1)
    Nothing === secretFromBytes bs

-----------------------
-- Public key
-----------------------

pk :: PublicKey
pk = fromJust $ publicFromBytes pkBytes

unit_publicFromBytes :: Assertion
unit_publicFromBytes = assertBool "pub parse failed" (isJust $ publicFromBytes pkBytes)

unit_publicToBytes :: Assertion
unit_publicToBytes = publicToBytes pk @?= pkBytes

unit_publicToBase64 :: Assertion
unit_publicToBase64 = publicToBase64 pk @?= pkB64

unit_publicFromBase64 :: Assertion
unit_publicFromBase64 = publicFromBase64 pkB64 @?= Just pk

hprop_publicBadFromBytes :: Property
hprop_publicBadFromBytes = property $ do
    bs <- forAll $ Gen.bytes $ Range.linear 0 (publicSize - 1)
    Nothing === publicFromBytes bs

hprop_publicBadFromBase64 :: Property
hprop_publicBadFromBase64 = property $ do
    bs <- forAll $ Gen.bytes $ Range.linear 0 (publicSize - 1)
    let b64 = SB64.encode bs
    Nothing === publicFromBase64 b64

hprop_PublicKeySerialise :: Property
hprop_PublicKeySerialise = property $ forAll genPk >>= serialising

hprop_PublicKeyJsonSerialise :: Property
hprop_PublicKeyJsonSerialise = property $ forAll genPk >>= jsoning

-----------------------
-- Signature
-----------------------

hprop_signatureBadFromBytes :: Property
hprop_signatureBadFromBytes = property $ do
    bs <- forAll $ Gen.bytes $ Range.linear 0 (signatureSize - 1)
    Nothing === signatureFromBytes bs

hprop_signAndVerify :: Property
hprop_signAndVerify = property $ do
    msg <- forAll $ Gen.bytes (Range.linear 0 1000)
    assert $ verify pk msg (sign sk msg)

hprop_verifyInvalid :: Property
hprop_verifyInvalid = property $ do
    msg <- forAll $ Gen.bytes (Range.linear 0 1000)
    sgn <- forAll genSignature
    guard $ sgn /= sign sk msg
    assert.not $ verify pk msg sgn

hprop_signAndVerifyLazy :: Property
hprop_signAndVerifyLazy = property $ do
    msg <- forAll $ BSL.fromStrict <$> Gen.bytes (Range.linear 0 1000)
    assert $ verifylazy pk msg (signlazy sk msg)

hprop_signLazySameAsStrict :: Property
hprop_signLazySameAsStrict = property $ do
    msg <- forAll $ Gen.bytes (Range.linear 0 1000)
    signlazy sk (BSL.fromStrict msg) === sign sk msg

hprop_SignatureSerialise :: Property
hprop_SignatureSerialise = property $ forAll genSignature >>= serialising

hprop_SignatureJsonSerialise :: Property
hprop_SignatureJsonSerialise = property $ forAll genSignature >>= jsoning
