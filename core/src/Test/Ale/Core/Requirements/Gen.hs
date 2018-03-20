module Test.Ale.Core.Requirements.Gen
       ( genRequirements
       , genReqProof
       ) where

import Universum

import Hedgehog (MonadGen)

import Ale.Core.Requirements (Proof (..), ReqByteString (..), ReqProof, Reqs (..), Requirements)

import Test.Ale.Core.Crypto.Gen (genSignature)
import Test.Ale.Core.Entity.Gen (genEntity)

import qualified Ale.Data.HashMap as HM
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Generate 'Requirements'.
genRequirements :: MonadGen m => Range.Range Int -> m Requirements
genRequirements r = Gen.choice
    [ RConst <$> Gen.bool
    , RAnd <$> Gen.list r (genRequirements r)
    , RLess <$> genReqsInt <*> genReqsInt
    , RVerify <$> genReqsBS <*> genReqsPK <*> genReqsSig
    ]
  where
    genReqs gen = Gen.choice
        [ RVar <$> Gen.integral (Range.constant 0 10)
        , RConst <$> gen
        ]
    genReqsInt = genReqs $ Gen.integral (Range.constant (-10000) 10000)
    genReqsPK = genReqs genEntity
    genReqsBS = genReqs $ ReqByteString <$> Gen.bytes (Range.linear 0 128)
    genReqsSig = genReqs genSignature

-- | Generate 'ReqProof'.  Unfortunately, almost all of generated
-- proofs are useless.
genReqProof :: MonadGen m => Range.Range Int -> m ReqProof
genReqProof r = HM.fromList <$> Gen.list r ((,) <$> genKey <*> genValue)
  where
    genKey :: MonadGen m => m Int
    genKey = Gen.integral (Range.constant 0 10)
    genValue :: MonadGen m => m Proof
    genValue = Proof <$> Gen.bytes (Range.linear 0 128)

