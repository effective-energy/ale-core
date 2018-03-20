module Test.Ale.Core.Crypto.Signed where

import Universum

import Codec.Serialise (DeserialiseFailure (..), deserialise, deserialiseOrFail, serialise)
import Data.ByteString (ByteString)

import Hedgehog (Property, discard, failure, footnoteShow, forAll, property, success, (===))

import Ale.Core.Crypto (toPublic)
import Ale.Core.Crypto.Signed.Internal

import Test.Ale.Core.Crypto.Gen (genSignature, genSk)
import Test.Ale.Core.Message.Gen (genMessage)
import Test.Serialise (serialising)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-----------------------
-- Signed
-----------------------

hprop_signSendReceive :: Property
hprop_signSendReceive = property $ do
    sk <- forAll genSk
    msg <- forAll $ Gen.bytes (Range.linear 0 1000)
    let got = deserialise . serialise $ mkSigned sk msg
    (sPublicKey got, sData got) === (toPublic sk, msg)

hprop_invalidSignature :: Property
hprop_invalidSignature = property $ do
    sk <- forAll genSk
    msg <- forAll $ Gen.bytes (Range.linear 0 1000)
    let signed = mkSigned sk msg

    badSig <- forAll genSignature
    when (badSig == sSignature signed) discard

    let bytes = serialise signed { sSignature = badSig }
    let d = deserialiseOrFail bytes :: Either DeserialiseFailure (Signed ByteString)
    case d of
        Left (DeserialiseFailure _ "Invalid signature") -> success
        v                                               -> footnoteShow v >> failure

hprop_SignedSerialise :: Property
hprop_SignedSerialise = property $
    forAll (mkSigned <$> genSk <*> Gen.bytes (Range.linear 0 1000)) >>= serialising

hprop_SignedMessageSerialise :: Property
hprop_SignedMessageSerialise = property $
    forAll (mkSigned <$> genSk <*> genMessage) >>= serialising
