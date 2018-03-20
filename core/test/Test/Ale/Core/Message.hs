module Test.Ale.Core.Message where

import Universum

import Hedgehog (Property, forAll, property, (===))

import Ale.Core.Message (JobOffer, Message, joExtra)

import Test.Ale.Core.Message.Gen (genContractorProposal, genEmployerAcceptance, genJobDescription,
                                  genJobOffer, genMessage, genMessagePointer, genSubmissionData)
import Test.Json (jsoning)
import Test.Serialise (serialising)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

----------------------------------------
--- Message
----------------------------------------

hprop_MessageSerialise :: Property
hprop_MessageSerialise = property $ forAll genMessage >>= serialising

hprop_MessageJsonSerialise :: Property
hprop_MessageJsonSerialise = property $ forAll genMessage >>= jsoning

----------------------------------------
--- MessagePointer
----------------------------------------

hprop_MessagePointerSerialise :: Property
hprop_MessagePointerSerialise = property $ forAll genMp >>= serialising
  where
    genMp = genMessagePointer genJobDescription

----------------------------------------
--- JobOffer
----------------------------------------

hprop_JobDescriptionSerialise :: Property
hprop_JobDescriptionSerialise = property $ forAll genJobDescription >>= serialising

hprop_JobOfferSerialise :: Property
hprop_JobOfferSerialise = property $ forAll genJobOffer >>= serialising

hprop_JobOfferExtraGetPut :: Property
hprop_JobOfferExtraGetPut = property $ do
    jo <- forAll genJobOffer
    key <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
    val <- forAll genJobOffer -- Yo dawg we store 'JobOffer's in your 'JobOffer's
    Nothing === (jo^.joExtra key :: Maybe JobOffer)
    let jo2 = jo & joExtra key .~ Just val
    Just val === jo2^.joExtra key

hprop_JobOfferExtraPutTwice :: Property
hprop_JobOfferExtraPutTwice = property $ do
    jo <- forAll genJobOffer
    key <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
    val1 <- forAll genJobOffer
    val2 <- forAll genJobOffer
    let jo2 = jo & joExtra key .~ Just val1
    let jo3 = jo2 & joExtra key .~ Just val2
    Just val2 === jo3^.joExtra key

hprop_JobOfferExtraDelete :: Property
hprop_JobOfferExtraDelete = property $ do
    jo <- forAll genJobOffer
    key <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
    val <- forAll genJobOffer
    let jo2 = jo & joExtra key .~ Just val
    let jo3 = jo2 & joExtra key .~ (Nothing :: Maybe JobOffer)
    Nothing === (jo3^.joExtra key :: Maybe JobOffer)

hprop_joExtraPutNonDeserialisable :: Property
hprop_joExtraPutNonDeserialisable = property $ do
    jo <- forAll genJobOffer
    key <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
    val <- forAll genJobOffer
    let jo2 = jo & joExtra key .~ Just val
    Nothing === (jo2^.joExtra key :: Maybe Message)

----------------------------------------
--- ContractorProposal
----------------------------------------

hprop_ContractorProposalSerialise :: Property
hprop_ContractorProposalSerialise = property $ forAll genContractorProposal >>= serialising

----------------------------------------
--- Submission
----------------------------------------

hprop_SubmissionDataSerialise :: Property
hprop_SubmissionDataSerialise = property $ forAll genSubmissionData >>= serialising

hprop_SubmissionSerialise :: Property
hprop_SubmissionSerialise = property $ forAll genSubmissionData >>= serialising

----------------------------------------
--- EmployerAcceptance
----------------------------------------

hprop_EmployerAcceptanceSerialise :: Property
hprop_EmployerAcceptanceSerialise = property $ forAll genEmployerAcceptance >>= serialising
