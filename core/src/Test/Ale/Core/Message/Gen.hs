module Test.Ale.Core.Message.Gen
       ( genMessage
       , genMessagePointer

       , genJobOffer
       , genContractorProposal
       , genSubmission
       , genEmployerAcceptance

       , genJobDescription
       , genSubmissionData
       ) where

import Universum

import Hedgehog (Gen)

import Codec.Serialise (Serialise)
import Data.Default (def)

import Ale.Core.Message (ContractorProposal (..), EmployerAcceptance (..), JobDescription (..),
                         JobOffer, Message (..), MessagePointer, Submission (..),
                         SubmissionData (..), jobOffer, mkPointer)
import Ale.Core.Requirements (Reqs (..), emptyProof)
import Ale.Core.Storage.Types (mkReference)

import Test.Ale.Core.Entity.Gen (genEntity)
import Test.Ale.Core.Submission.Acceptance.Gen (genAcceptanceTest)
import Test.Ale.Core.Tokens.Gen (genTokens)

import qualified Data.ByteString.Lazy as BSL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- | Generate a random 'Message'.
genMessage :: Gen Message
genMessage = Gen.choice
    [ MsgJobOffer <$> genJobOffer
    , MsgContractorProposal <$> genContractorProposal
    , MsgSubmission <$> genSubmission
    , MsgEmployerAcceptance <$> genEmployerAcceptance
    -- TODO deal with MsgUnknown
    ]

-- | Generate a pointer to a random 'Message'.
genMessagePointer :: Serialise a => Gen a -> Gen (MessagePointer a)
genMessagePointer gen = mkPointer <$> genEntity <*> gen

-- | Generate a 'JobOffer' message.
genJobOffer :: Gen JobOffer
genJobOffer = jobOffer
    <$> Gen.maybe (mkReference <$> genJobDescription)
    <*> pure Nothing
    <*> genTokens (Range.linear 0 100)
    <*> pure (RAnd []) -- TODO generate proper requirements
    <*> genAcceptanceTest

-- | Generate a 'JobDescription'.
genJobDescription :: Gen JobDescription
genJobDescription = JobDescription <$> BSL.fromStrict <$> Gen.utf8 (Range.linear 0 100) Gen.alphaNum

-- | Generate a 'ContractorProposal' message.
genContractorProposal :: Gen ContractorProposal
genContractorProposal = ContractorProposal
    <$> genMessagePointer genJobOffer
    <*> pure emptyProof -- TODO generate proper proof

-- | Generate a 'Submission' message.
genSubmission :: Gen Submission
genSubmission = Submission
    <$> genMessagePointer genContractorProposal
    <*> genSubmissionData
    <*> pure def

-- | Generate 'SubmissionData'.
genSubmissionData :: Gen SubmissionData
genSubmissionData = SubmissionData . BSL.fromStrict <$> Gen.utf8 (Range.linear 0 100) Gen.alphaNum

-- | Generate an 'EmployerAcceptance' message.
genEmployerAcceptance :: Gen EmployerAcceptance
genEmployerAcceptance = EmployerAcceptance
    <$> genMessagePointer genSubmission
