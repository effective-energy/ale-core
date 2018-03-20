-- | This module contains a test that transfers some tokens from one
-- account to another.
module Test.Ale.Transactions where

import Universum

import Data.Traversable (for)

import Codec.Serialise (serialise)
import Hedgehog (MonadGen, MonadTest, Property, evalEither, failure, footnoteShow, forAll, property,
                 success, (===))

import Ale.Core.Block as B (Block, BlockPointer, Bundle (Bundle), UnsignedBlock (UnsignedBlock),
                            mkPointer, signBlock)
import Ale.Core.Crypto (SecretKey, sign, toPublic)
import Ale.Core.Crypto.Signed (Signed, mkSigned)
import Ale.Core.DHT.Types (MonadDHT (..))
import Ale.Core.Genesis.Block (initialState)
import Ale.Core.Genesis.Data (GenesisData (..), defaultValidityPeriod)
import Ale.Core.Genesis.Distribution (mkUniform)
import Ale.Core.Message as Msg (ContractorProposal (..), JobOffer, Message (..), MessagePointer,
                                joRequirements, mkPointer)
import Ale.Core.Requirements (Proof (..), ReqByteString (..), Reqs (RAnd, RConst, RVar, RVerify))
import Ale.State (asdAccounts, asdOpenOffers, updateState)
import Ale.State.Pure (AleState, asBlock, asData, emptyAleState, runPureAleStateT)
import Ale.Transactions (ReceiveError (..), receive, transfer)

import Test.Ale.Core.Crypto.Gen (genKeyPair, genPk, genSignature, genSk)
import Test.Ale.Core.DHT ()
import Test.Ale.Core.DHT.Pure (propInPureDHT)
import Test.Ale.Core.Message.Gen (genJobOffer)
import Test.Ale.Core.Tokens.Gen (genTokenCount, genTokenCountLe)
import Test.Util (evalMaybe)

import qualified Ale.Core.Tokens as T
import qualified Ale.Data.HashMap as HM
import qualified Ale.State.MessageSet as MS
import qualified Data.ByteString.Lazy as BSL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

hprop_transaction :: Property
hprop_transaction = propInPureDHT $ do
    (skServer, pkServer) <- forAll genKeyPair
    (skSender, sender) <- forAll genKeyPair
    (skReceiver, receiver) <- forAll genKeyPair

    -- Initially both have the same amount of random tokens
    tokenIssuers <- forAll $ Gen.list (Range.linear 0 100) genPk
    initialCount <- forAll genTokenCount
    let distr = mkUniform [sender, receiver] (map T.Transferable tokenIssuers) initialCount

    -- We will send a random subset of available tokens
    sendIssuers <- forAll $ Gen.subsequence tokenIssuers
    pairs <- forAll $ for sendIssuers (\e -> (,) (T.Transferable e) <$> genTokenCountLe initialCount)
    let amount = T.fromHashMap . HM.fromList $ pairs

    -- Random nonce for receiver verification
    transferChallenge <- forAll genChallenge

    ---- Now perform the transaction ----

    -- Initialise internal state.
    pk <- forAll genPk
    ((), st1) <- runPureAleStateT emptyAleState $ initialState $
        GenesisData distr pk defaultValidityPeriod

    -- Create new block by issuing token transfer from 'sender' to 'receiver'.
    let transferMsg = transfer skSender amount receiver transferChallenge
    transferBlock <- mine skServer (st1^.asBlock) [transferMsg]
    let transferPtr = B.mkPointer transferBlock
    (ei2, st2) <- runPureAleStateT st1 $ updateState transferBlock pkServer
    _ <- evalEither ei2

    -- To claim tokens on the receiving side, we need to obtain
    -- 'MessagePointer' somehow.  In this test we simply extract
    -- it from internal state.
    offerPtr <- extractJobOfferPtr st2

    -- Send request to claim sent tokens.
    (_, offer) <- evalMaybe $ MS.lookup offerPtr (st2^.asData^.asdOpenOffers)
    receiveMsg <- evalEither $ receive skReceiver offerPtr offer
    receiveBlock <- mine skServer transferPtr [receiveMsg]
    (ei3, st3) <- runPureAleStateT st2 $ updateState receiveBlock pkServer
    _ <- evalEither ei3

    ---- Transaction performed ----

    let actualAccounts = st3^.asData^.asdAccounts

    -- We expect that 'sender' now has what he hand in the beginning, sans the
    -- transferred tokens. The receiver now has these extra tokens.
    let initialBal = T.fromList $ map (\e -> (T.Transferable e, initialCount)) tokenIssuers
    expectedSenderBal <- evalEither $ T.subMany amount initialBal
    let expectedReceiverBal = T.addMany amount initialBal

    let expectedAccounts = HM.fromList
            [ (sender, expectedSenderBal)
            , (receiver, expectedReceiverBal)
            ]

    expectedAccounts === actualAccounts


hprop_transactionFailure :: Property
hprop_transactionFailure = propInPureDHT $ do
    (skServer, pkServer) <- forAll genKeyPair
    (skSender, sender) <- forAll genKeyPair
    (skReceiver, receiver) <- forAll genKeyPair

    -- Initially both have the same amount of random tokens
    tokenIssuers <- forAll $ Gen.list (Range.linear 0 100) genPk
    initialCount <- forAll genTokenCount
    let distr = mkUniform [sender, receiver] (map T.Transferable tokenIssuers) initialCount

    -- We will send a random subset of available tokens and one extra token
    sendIssuers <- forAll $ Gen.subsequence tokenIssuers
    pairs <- forAll $ for sendIssuers (\e -> (,) (T.Transferable e) <$> genTokenCountLe initialCount)

    -- And, probably, some tokens that the sender does not have
    notEnoughMoney <- forAll $ Gen.frequency [(1, pure True), (5, pure False)]

    amount <- T.fromHashMap . HM.fromList <$>
        if notEnoughMoney
        then do
            extraIssuer <- forAll $ Gen.filter (not . (`elem` tokenIssuers)) genPk
            pure $ (T.Transferable extraIssuer, 1) : pairs
        else pure pairs

    -- Random nonce for receiver verification
    transferChallenge <- forAll genChallenge

    ---- Now perform the transaction ----

    -- Initialise internal state.
    pk <- forAll genPk
    ((), st1) <- runPureAleStateT emptyAleState $ initialState $
        GenesisData distr pk defaultValidityPeriod

    -- Create new block by issuing token transfer from 'sender' to 'receiver'.
    let transferMsg = transfer skSender amount receiver transferChallenge
    transferBlock <- mine skServer (st1^.asBlock) [transferMsg]
    let transferPtr = B.mkPointer transferBlock
    (res2, st2) <- runPureAleStateT st1 $ updateState transferBlock pkServer

    if notEnoughMoney then
      -- If there was not enough money, update should have failed
      case res2 of
          Right _ -> footnoteShow st2 >> failure
          _       -> success
    else do
      -- If there was enough money, try to claim it will a wrong signature
      _ <- evalEither res2
      offerPtr <- extractJobOfferPtr st2

      sgn <- forAll $ Gen.filter (/= sign skReceiver transferChallenge) genSignature
      let proof = HM.fromList [(1, Proof $ BSL.toStrict $ serialise sgn)]
      let receiveMsg = mkSigned skReceiver $ MsgContractorProposal $
              ContractorProposal offerPtr proof
      receiveBlock <- mine skServer transferPtr [receiveMsg]
      (res3, st3) <- runPureAleStateT st2 $ updateState receiveBlock pkServer

      case res3 of
          Right _ -> footnoteShow st3 >> failure
          _       -> success

hprop_receiveUnsatisfiable :: Property
hprop_receiveUnsatisfiable = property $ do
    sk <- forAll genSk
    sk2 <- forAll genSk
    challenge <- forAll $ Gen.bytes $ Range.linear 1 32
    guard $ sk /= sk2
    jo <- forAll genJobOffer
    let jo' = jo & joRequirements .~
            RVerify (RConst $ ReqByteString challenge) (RConst $ toPublic sk) (RVar 1)
    case receive sk2 (Msg.mkPointer (toPublic sk) jo') jo' of
        Left CantSign -> success
        x             -> footnoteShow x >> failure

hprop_receiveNotFromTransfer :: Property
hprop_receiveNotFromTransfer = property $ do
    sk <- forAll genSk
    sk2 <- forAll genSk
    challenge <- forAll $ Gen.bytes $ Range.linear 1 32
    guard $ sk /= sk2
    jo <- forAll genJobOffer
    let jo' = jo & joRequirements .~
            RAnd [RConst True, RVerify (RConst $ ReqByteString challenge) (RConst $ toPublic sk2) (RVar 1)]
    case receive sk2 (Msg.mkPointer (toPublic sk) jo') jo' of
        Left NotFromTransfer -> success
        x                    -> footnoteShow x >> failure

-----------------------
-- Helpers
-----------------------

-- | This (non-total) function extract some active Job Offer from
-- State.  In this case we expect only one Offer in the state so
-- this function will extract correct one.
extractJobOfferPtr :: MonadTest m => AleState -> m (MessagePointer JobOffer)
extractJobOfferPtr st = case keys $ st^.asData.asdOpenOffers of
    []  -> footnoteShow (st^.asData) >> failure
    x:_ -> pure x

-- | We don't have a way to issue new blocks yet.
mine :: (Monad m, MonadDHT m) => SecretKey -> BlockPointer -> [Signed Message] -> m Block
mine sk prev msgs = signBlock sk <$>
    (UnsignedBlock prev <$> putDHT (Bundle msgs))

-- | Generate challenge for signing requests.
genChallenge :: MonadGen m => m ByteString
genChallenge = Gen.bytes (Range.linear 0 128)
