module Test.Ale.Core.Submission.Acceptance.Gen
       ( genAcceptanceTest
       ) where

import Hedgehog (MonadGen)

import Ale.Core.Submission.Acceptance (AcceptanceTest, alwaysFail, alwaysPass, facile)

import qualified Hedgehog.Gen as Gen


-- | Generate 'AcceptanceTest'.
--
-- 'AcceptanceTest' constructor is not exported so the only possible
-- things here are just module exports.  'AcceptanceTest' will
-- probably redesigned soon and this function will be changed
-- accordingly.
genAcceptanceTest :: MonadGen m => m AcceptanceTest
genAcceptanceTest = Gen.element [facile, alwaysPass, alwaysFail]
