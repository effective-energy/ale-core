module Test.Ale.Core.Crypto.Gen
       ( genPk
       , genSk
       , genKeyPair
       , genSignature
       ) where


import Universum

import Hedgehog (MonadGen)

import Ale.Core.Crypto (PublicKey, SecretKey, Signature, secretFromBytes, secretSize,
                        signatureFromBytes, signatureSize, toPublic)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- | Generate a 'SecretKey'.
genSk :: MonadGen m => m SecretKey
genSk = Gen.just $ secretFromBytes <$> Gen.bytes (Range.singleton secretSize)

-- | Generate a 'PublicKey'.
genPk :: MonadGen m => m PublicKey
genPk = toPublic <$> genSk

-- | Generate a ('SecretKey', 'PublicKey') pair.
genKeyPair :: MonadGen m => m (SecretKey, PublicKey)
genKeyPair = do
    secret <- genSk
    pure (secret, toPublic secret)

-- | Generate a 'Signature'
genSignature :: MonadGen m => m Signature
genSignature = Gen.just $ signatureFromBytes <$> Gen.bytes (Range.singleton signatureSize)
