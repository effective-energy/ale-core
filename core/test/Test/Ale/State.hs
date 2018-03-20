{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Ale.State where

import Universum

import Hedgehog (MonadTest, Property, assert, discard, eval, evalEither, failure, footnoteShow,
                 forAll, success, (===))

import Control.Lens (at, to)
import Control.Monad.Except (runExceptT)
import Data.Default (def)

import Ale.Core.Block as B (Block (..), BlockPointer, Bundle (..), UnsignedBlock (..), mkPointer,
                            signBlock)
import Ale.Core.Crypto (SecretKey, toPublic)
import Ale.Core.Crypto.Signed (mkSigned)
import Ale.Core.DHT.Pure ()
import Ale.Core.DHT.Types (MonadDHT (..))
import Ale.Core.Genesis.Block (initialState)
import Ale.Core.Genesis.Data (GenesisData (..))
import Ale.Core.Genesis.Distribution (mkOwnUniform)
import Ale.Core.Height (Height)
import Ale.Core.Message as Msg (ContractorProposal (..), EmployerAcceptance (..), Message (..),
                                Submission (..), SubmissionData (..), joHeight, jobOffer, mkPointer)
import Ale.Core.Requirements (Reqs (RConst), emptyProof)
import Ale.Core.Storage (mkReference)
import Ale.Core.Submission.Acceptance (alwaysFail)
import Ale.State (AleStateData, UpdateError (..), applySignedMessage, asdAccounts,
                  asdActiveProposals, asdOpenOffers, asdTakenOffers, asdWaitingSubmissions,
                  emptyData, getASBlockOrError, updateState)
import Ale.State.Pure (asBlock, asData, asHeight, emptyAleState, runPureAleStateT)

import Test.Ale.Core.Crypto.Gen (genKeyPair, genPk, genSk)
import Test.Ale.Core.DHT ()
import Test.Ale.Core.DHT.Pure (propInPureDHT, propInPureDHTState)
import Test.Ale.Core.Entity.Gen (genEntity)
import Test.Ale.Core.Genesis.Data.Gen (genGenesisData)
import Test.Ale.Core.Message.Gen (genJobOffer)
import Test.Ale.Core.Tokens.Gen (genTokenCount, genTokenCountLe)

import qualified Ale.Core.Tokens as T
import qualified Ale.State.MessageSet as MS
import qualified Data.ByteString.Lazy as BSL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

defaultValidityPeriod :: Height
defaultValidityPeriod = 5

hprop_nonFacile :: Property
hprop_nonFacile = propInPureDHT $ do
    (serverSk, serverPk) <- forAll genKeyPair
    contractor <- forAll genSk
    let contractorPk = toPublic contractor
    employer <- forAll genSk
    let employerPk = toPublic employer
    bal <- forAll genTokenCount
    cost <- forAll $ genTokenCountLe bal
    entities <- forAll $ Gen.list (Range.linear 0 10) genEntity
    let distr = mkOwnUniform (employerPk:entities) bal
    pk <- forAll genPk
    ((), std0) <- runPureAleStateT emptyAleState $ initialState $
        GenesisData distr pk defaultValidityPeriod

    let price = T.addOne (T.Transferable employerPk) cost T.empty
    let jo = jobOffer Nothing Nothing price (RConst True) alwaysFail
    let joPtr = Msg.mkPointer employerPk jo

    block1 <- mine serverSk employer (MsgJobOffer jo) (std0^.asBlock)
    footnoteShow (std0 ^. asHeight)
    (ei1, std1) <- runPureAleStateT std0 (updateState block1 serverPk)
    _ <- evalEither ei1
    MS.lookup joPtr (std1^.asData^.asdOpenOffers) === Just (employerPk, jo)
    assertEmpty std1 asdTakenOffers
    assertEmpty std1 asdActiveProposals
    assertEmpty std1 asdWaitingSubmissions
    std1^.balance employerPk (T.Transferable employerPk) === bal - cost
    std1^.asBlock === B.mkPointer block1

    let proposal = ContractorProposal joPtr emptyProof
    let proposalPtr = Msg.mkPointer contractorPk proposal

    block2 <- mine serverSk contractor (MsgContractorProposal proposal) (std1^.asBlock)
    (ei2, std2) <- runPureAleStateT std1 (updateState block2 serverPk)
    _ <- evalEither ei2
    assertEmpty std2 asdOpenOffers
    MS.lookup joPtr (std2^.asData^.asdTakenOffers) === Just (employerPk, jo)
    MS.lookup proposalPtr (std2^.asData^.asdActiveProposals) === Just (contractorPk, proposal)
    assertEmpty std2 asdWaitingSubmissions
    std2^.asBlock === B.mkPointer block2
    std2^.balance contractorPk (T.Transferable employerPk) === 0

    let submission = Submission proposalPtr (SubmissionData "lol done") def
    let submissionPtr = Msg.mkPointer contractorPk submission

    block3 <- mine serverSk contractor (MsgSubmission submission) (std2^.asBlock)
    (ei3, std3) <- runPureAleStateT std2 (updateState block3 serverPk)
    _ <- evalEither ei3
    assertEmpty std3 asdOpenOffers
    MS.lookup joPtr (std3^.asData^.asdTakenOffers) === Just (employerPk, jo)
    MS.lookup proposalPtr (std3^.asData^.asdActiveProposals) === Just (contractorPk, proposal)
    MS.lookup submissionPtr (std3^.asData^.asdWaitingSubmissions)
        === Just (contractorPk, submission)
    std3^.asBlock === B.mkPointer block3
    std3^.balance contractorPk (T.Transferable employerPk) === 0

    let acceptance = EmployerAcceptance submissionPtr

    block4 <- mine serverSk employer (MsgEmployerAcceptance acceptance) (std3^.asBlock)
    (ei4, std4) <- runPureAleStateT std3 (updateState block4 serverPk)
    _ <- evalEither ei4
    assertEmpty std4 asdOpenOffers
    assertEmpty std4 asdTakenOffers
    assertEmpty std4 asdActiveProposals
    assertEmpty std4 asdWaitingSubmissions
    std4^.asBlock === B.mkPointer block4
    std4^.balance contractorPk (T.Transferable employerPk) === cost
    std4^.balance employerPk (T.Transferable employerPk) === bal - cost
  where
    mine :: (Monad m, MonadDHT m)
         => SecretKey -- ^ Hub SK
         -> SecretKey -- ^ Message author
         -> Message
         -> BlockPointer
         -> m Block
    mine serverSk sk msg prevBlock = signBlock serverSk <$>
        (UnsignedBlock prevBlock <$> putDHT (Bundle [mkSigned sk msg]))

    assertEmpty st lens = assert (MS.size (st^.asData^.lens) == 0)

    balance who tokenType =
        asData.asdAccounts.at who.to (fmap $ T.countOf tokenType).to (fromMaybe 0)

hprop_claimedShouldBePurged :: Property
hprop_claimedShouldBePurged = propInPureDHT $ do
    employer <- forAll genSk
    contractor <- forAll genSk
    let employerPk = toPublic employer
    let contractorPk = toPublic contractor

    let jo = jobOffer Nothing Nothing T.empty (RConst True) alwaysFail
    let joPtr = Msg.mkPointer employerPk jo

    let proposal = ContractorProposal joPtr emptyProof
    let proposalPtr = Msg.mkPointer contractorPk proposal

    let submission text = Submission proposalPtr (SubmissionData text) def
    let submissionPtr = Msg.mkPointer contractorPk . submission

    let acceptance = EmployerAcceptance . submissionPtr

    -- We generate a /set/ of submissions here to make sure the replay protection
    -- is not triggered.
    submissions <- forAll $
        fmap toList . Gen.set (Range.linear 1 50) $
            BSL.fromStrict <$> Gen.bytes (Range.linear 0 50)
    winner <- forAll $ Gen.element submissions

    let msgs = [ (employer, MsgJobOffer jo)
               , (contractor, MsgContractorProposal proposal)
               ]
               ++ [ (contractor, MsgSubmission $ submission sub) | sub <- submissions]
               ++ [ (employer, MsgEmployerAcceptance $ acceptance winner) ]

    std <- foldr (>=>) pure [apply sk msg | (sk, msg) <- msgs] $ emptyData defaultValidityPeriod
    std^.asdWaitingSubmissions === MS.empty
  where
    apply :: (MonadTest m, MonadDHT m)
          => SecretKey -> Message -> AleStateData
          -> m AleStateData
    apply sk msg std = runExceptT (applySignedMessage def (mkSigned sk msg) std) >>= evalEither

hprop_updateStateBrokenChain :: Property
hprop_updateStateBrokenChain = propInPureDHT $ do
    genData <- forAll $ genGenesisData $ Range.linear 0 50

    ((), st) <- runPureAleStateT emptyAleState $ initialState genData

    genData' <- forAll $ genGenesisData $ Range.linear 0 50
    when (genData == genData') discard
    (serverSk, serverPk) <- forAll genKeyPair
    ((), st_bad) <- runPureAleStateT emptyAleState $ initialState genData'
    nextBlock <- signBlock serverSk <$>
        (UnsignedBlock (st_bad^.asBlock) <$> putDHT (Bundle []))

    (res, st') <- runPureAleStateT st (updateState nextBlock serverPk)
    case res of
        Left BrokenChain -> success
        Left e           -> footnoteShow e >> failure
        Right _          -> footnoteShow st' >> failure

hprop_updateStateNoBundle :: Property
hprop_updateStateNoBundle = propInPureDHTState $ do
    genData <- forAll $ genGenesisData $ Range.linear 0 50
    (serverSk, serverPk) <- forAll genKeyPair
    jo <- forAll genJobOffer
    sk <- forAll genSk

    lift $ initialState genData
    curBlock <- lift $ getASBlockOrError
    let nextBlock = signBlock serverSk $ UnsignedBlock curBlock
                          (mkReference $ Bundle [mkSigned sk $ MsgJobOffer jo])

    res <- lift $ updateState nextBlock serverPk
    case res of
        Left (BundleNotFound t) -> eval t >> success
        Left e                  -> footnoteShow e >> failure
        Right _                 -> failure

hprop_updateStateWrongValidity :: Property
hprop_updateStateWrongValidity = propInPureDHTState $ do
    (serverSk, serverPk) <- forAll genKeyPair
    employer <- forAll genSk
    let employerPk = toPublic employer
    balance <- forAll genTokenCount
    cost <- forAll $ genTokenCountLe balance
    entities <- forAll $ Gen.list (Range.linear 0 10) genEntity
    let distr = mkOwnUniform (employerPk:entities) balance
    pk <- forAll genPk
    ((), std0) <- runPureAleStateT emptyAleState $ initialState $
        GenesisData distr pk defaultValidityPeriod
    let price = T.addOne (T.Transferable employerPk) cost T.empty
    let jo = jobOffer Nothing Nothing price (RConst True) alwaysFail
    -- try to send message with bigger epoch to state with initial height
    block <- mine serverSk employer (pure $ MsgJobOffer $ jo & joHeight .~ 7) $ std0 ^. asBlock
    runPureAleStateT std0 (updateState block serverPk) >>= \case
        -- matching on string should be slow, but I don't know another solution
        (Left (IllegalMessage "Message in this block is out of epoch"), _) -> success
        (Left e, _) -> footnoteShow e >> failure
        _           -> failure

  where
    mine serverSk sk msgs prevBlock = signBlock serverSk <$>
        (UnsignedBlock prevBlock <$> (putDHT $ Bundle $ (mkSigned sk) <$> msgs))

hprop_updateStateReplayAttack :: Property
hprop_updateStateReplayAttack = propInPureDHT $ do
    (serverSk, serverPk) <- forAll genKeyPair
    employer <- forAll genSk
    let employerPk = toPublic employer
    balance <- forAll genTokenCount
    cost <- forAll $ genTokenCountLe balance
    entities <- forAll $ Gen.list (Range.linear 0 10) genEntity
    let distr = mkOwnUniform (employerPk:entities) balance
    pk <- forAll genPk
    ((), std0) <- runPureAleStateT emptyAleState $ initialState $
        GenesisData distr pk defaultValidityPeriod
    let price = T.addOne (T.Transferable employerPk) cost T.empty
    let jo = jobOffer Nothing Nothing price (RConst True) alwaysFail
    block <- mine serverSk employer (replicate 10 $ MsgJobOffer jo) $ std0 ^. asBlock
    -- try to send 10 equal messages
    runPureAleStateT std0 (updateState block serverPk) >>= \case
        -- matching on string should be slow, but I don't know another solution
        (Left (IllegalMessage "Replay attack detected"), _) -> success
        (Left e, _) -> footnoteShow e >> failure
        _           -> failure

  where
    mine serverSk sk msgs prevBlock = signBlock serverSk <$>
        (UnsignedBlock prevBlock <$> (putDHT $ Bundle $ (mkSigned sk) <$> msgs))

hprop_updateStateReplayAttack2 :: Property
hprop_updateStateReplayAttack2 = propInPureDHT $ do
    (serverSk, serverPk) <- forAll genKeyPair
    employer <- forAll genSk
    let employerPk = toPublic employer
    balance <- forAll genTokenCount
    cost <- forAll $ genTokenCountLe balance
    entities <- forAll $ Gen.list (Range.linear 0 10) genEntity
    let distr = mkOwnUniform (employerPk:entities) balance
    pk <- forAll genPk
    ((), std0) <- runPureAleStateT emptyAleState $ initialState $
        GenesisData distr pk defaultValidityPeriod
    let price = T.addOne (T.Transferable employerPk) cost T.empty
    let jo = jobOffer Nothing Nothing price (RConst True) alwaysFail

    -- Send a message
    block1 <- mine serverSk employer [MsgJobOffer jo] (std0^.asBlock)
    (ei1, std1) <- runPureAleStateT std0 (updateState block1 serverPk)
    _ <- evalEither ei1

    -- Now try to send it again in the next block
    block2 <- mine serverSk employer [MsgJobOffer jo] (std1^.asBlock)
    runPureAleStateT std1 (updateState block2 serverPk) >>= \case
        (Left (IllegalMessage "Replay attack detected"), _) -> success
        (Left e, _) -> footnoteShow e >> failure
        _           -> failure

  where
    mine serverSk sk msgs prevBlock = signBlock serverSk <$>
        (UnsignedBlock prevBlock <$> (putDHT $ Bundle $ (mkSigned sk) <$> msgs))


hprop_signWithWrongKey :: Property
hprop_signWithWrongKey = propInPureDHTState $ do
    genData <- forAll $ genGenesisData $ Range.linear 0 50
    serverSk <- forAll genSk
    serverPk' <- forAll genPk
    when (toPublic serverSk == serverPk') discard
    jo <- forAll genJobOffer
    sk <- forAll genSk

    lift $ initialState genData
    curBlock <- lift $ getASBlockOrError
    nextBlock <- signBlock serverSk <$>
        (UnsignedBlock curBlock <$>
            (putDHT $ Bundle [mkSigned sk $ MsgJobOffer jo]))
    res <- lift $ updateState nextBlock serverPk'
    case res of
        Left NotSignedByServer -> success
        Left e                 -> footnoteShow e >> failure
        Right diff             -> footnoteShow diff >> failure
