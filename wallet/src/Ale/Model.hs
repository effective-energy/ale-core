{-# LANGUAGE DeriveAnyClass #-}

-- | Pure mode of Ale.
module Ale.Model
       ( ModelAle (..)
       , maWallets
       , maOffers
       , emptyModelAle

       , ModelWallet (..)

       , OfferState (..)

       , TargetWallets
       , targetWallet

       -- * Root endpoints
       , importWallet
       , wallets

       -- * Node endpoints
       , offersAll
       , offersAllOpen

       , moneyBalance

       -- * Wallet-specific endpoints
       , info

       -- ** Transactions
       , transactions
       , createTransaction

       -- ** Offers
       , publishOffer
       , offersCreated
       , takeOffer
       , offersTaken
       , offersFinished

       -- ** Submissions
       , submitSubmission
       , submissions
       , offerSubmissions
       , acceptSubmission

       -- * Result types
       , TakeOfferResult (..)
       , SubmitSubmissionResult (..)
       , AcceptSubmissionResult (..)
       ) where

import Universum hiding (HashMap)

import Codec.Serialise (Serialise (..))
import Control.Lens (makeLenses, to)
import Control.Lens.At (at, ix)
import Control.Lens.Fold (concatOf, filtered, folded, (^?!))
import Control.Lens.Indexed (asIndex, ifolded, withIndex)
import Control.Lens.Prism (_Just)
import Control.Lens.Setter ((%=), (+=), (-=), (.=), (?=))
import Control.Lens.Traversal (partsOf, traversed)
import Data.Default (def)
import Data.Map (Map)
import Data.Set (Set)
import Data.Time.Clock (getCurrentTime)

import Ale.Core.Crypto (PublicKey, SecretKey, toPublic)
import Ale.Core.Entity (Entity)
import Ale.Core.Message (ContractorProposal (..), JobOffer, MessagePointer, Submission (..),
                         SubmissionData, mkPointer)
import Ale.Core.Requirements (emptyProof)
import Ale.Core.Tokens (TokenCount (..))
import Ale.Data.HashMap (HashMap)
import Ale.Wallet.Types (Transaction (..), WalletInfo (..))

import qualified Data.Map as M
import qualified Data.Set as S

import qualified Ale.Data.HashMap as HM
import qualified Ale.Wallet.FrontendMessage as Frontend (JobOffer, toInnerJobOffer)


data ModelWallet = ModelWallet
    { _mwPublicKey      :: PublicKey
    , _mwBalance        :: TokenCount
    , _mwTransactions   :: [Transaction]
    , _mwCreatedOffers  :: Set (MessagePointer JobOffer)
    , _mwTakenOffers    :: Set (MessagePointer JobOffer)
    , _mwSubmissions    :: Map (MessagePointer JobOffer) [MessagePointer Submission]
    , _mwFinishedOffers :: Set (MessagePointer JobOffer)
    } deriving (Show, Generic, Serialise)

makeLenses ''ModelWallet


data OfferState = Open | Taken Entity | Finished Entity
    deriving (Show, Generic, Serialise)

data ModelAle = ModelAle
    { _maWallets     :: HashMap PublicKey ModelWallet
    , _maOffers      :: Map (MessagePointer JobOffer) (JobOffer, OfferState)
    , _maSubmissions :: Map (MessagePointer Submission)
                            (MessagePointer JobOffer, Submission)
    } deriving (Show, Generic, Serialise)

makeLenses ''ModelAle

emptyModelAle :: ModelAle
emptyModelAle = ModelAle HM.empty M.empty M.empty

------------------------------------
----          Keep away         ----
---- Below are dangerous lenses ----
----       (and traversals)     ----
------------------------------------


-- | Wallets that we perform our operation on.
type TargetWallets = Traversal' ModelAle ModelWallet

targetWallet :: PublicKey -> TargetWallets
targetWallet pk = maWallets . ix pk


-- | Import a wallet given its secret key.
-- In reality, just creates an empty wallet with the given key, potentially
-- overriding the existing one.
importWallet :: MonadState ModelAle m => SecretKey -> m WalletInfo
importWallet sk = do
    let pk = toPublic sk
    let newWallet = ModelWallet pk 100 [] S.empty S.empty M.empty S.empty
    maWallets . at pk ?= newWallet
    pure $ WalletInfo pk

-- | Get a list of imported wallets.
wallets :: MonadState ModelAle m => m [PublicKey]
wallets = gets (^.. maWallets . traversed . mwPublicKey)



------
----
-- Wallet-specific
----
------

-- | Get information about a chosen wallet.
info :: MonadState ModelAle m => TargetWallets -> m (Maybe WalletInfo)
info ws = preuse (ws . mwPublicKey . to WalletInfo)


----
-- Transactions
----

-- | List of all the transactions relevant to current user.
transactions :: MonadState ModelAle m => TargetWallets -> m (Maybe [Transaction])
transactions ws = preuse (ws . mwTransactions)


-- | Create a new money transaction.
-- Returns `Nothing` if the user is not found and `Just Nothing` if there
-- is not enough money :).
createTransaction :: (MonadIO m, MonadState ModelAle m)
                  => Entity -> TokenCount -> TargetWallets -> m (Maybe (Maybe Transaction))
createTransaction dst amount ws = withTarget ws $ \sender ->
    runMaybeT $ do
        guard (sender^.mwBalance >= amount)

        let src = sender^.mwPublicKey
        maWallets . ix src . mwBalance -= amount
        maWallets . ix dst . mwBalance += amount

        curTime <- liftIO getCurrentTime
        let t = Transaction { tSender    = src
                            , tReceiver  = dst
                            , tAmount    = amount
                            , tTimestamp = curTime
                            }
        maWallets . ix src . mwTransactions %= (t :)
        maWallets . ix dst . mwTransactions %= (t :)

        pure t

moneyBalance :: MonadState ModelAle m
             => PublicKey -> m TokenCount
moneyBalance pk = fromMaybe 0 <$> gets (^? maWallets . at pk . _Just . mwBalance)

----
-- Offers
----

-- | Publish a new job offer.
publishOffer :: MonadState ModelAle m
             => Frontend.JobOffer -> TargetWallets
             -> m (Maybe (MessagePointer JobOffer))
publishOffer offer' ws = withTarget ws $ \w -> do
    let offer = Frontend.toInnerJobOffer offer'
    let offerPtr = mkPointer (w^.mwPublicKey) offer
    maOffers %= M.insert offerPtr (offer, Open)
    ws . mwCreatedOffers %= S.insert offerPtr
    pure offerPtr

-- | List all existing offers.
offersAll :: MonadState ModelAle m => m [MessagePointer JobOffer]
offersAll = gets (^.. maOffers . ifolded . asIndex)

-- | List all existing open offers.
offersAllOpen :: MonadState ModelAle m => m [MessagePointer JobOffer]
offersAllOpen = gets (^.. maOffers . ifolded . withIndex . filtered isOpen . _1)
  where
    isOpen (_, (_, Open)) = True
    isOpen _              = False

-- | List offers created by chosen users.
offersCreated :: MonadState ModelAle m
              => TargetWallets -> m [MessagePointer JobOffer]
offersCreated ws = gets (^.. ws . mwCreatedOffers . folded)


data TakeOfferResult
    = TakeOfferOk
    | TakeOfferNotFound
    | TakeOfferIllegalState OfferState

-- | Take an offer.
takeOffer :: MonadState ModelAle m
          => MessagePointer JobOffer -> TargetWallets -> m (Maybe TakeOfferResult)
takeOffer joPtr ws = withTarget ws $ \w -> do
    mst <- gets (^? maOffers . at joPtr . _Just . _2)
    case mst of
        Just Open -> do
            ws . mwTakenOffers %= S.insert joPtr
            -- This line is weird, but we know that the offer is there
            partsOf (maOffers . at joPtr . _Just . _2) .= [Taken $ w ^. mwPublicKey]
            pure TakeOfferOk
        Just st -> pure $ TakeOfferIllegalState st
        Nothing -> pure TakeOfferNotFound

-- | List offers taken by chosen users.
offersTaken :: MonadState ModelAle m
            => TargetWallets -> m [MessagePointer JobOffer]
offersTaken ws = gets (^.. ws . mwTakenOffers . folded)

-- | List offers finished by chosen users.
offersFinished :: MonadState ModelAle m
               => TargetWallets -> m [MessagePointer JobOffer]
offersFinished ws = gets (^.. ws . mwFinishedOffers . folded)


----
-- Submissions
----

data SubmitSubmissionResult
    = SubmitSubmissionOk (MessagePointer Submission)
    | SubmitSubmissionNotFound
    | SubmitSubmissionIllegalState OfferState

-- | Submit a new submission.
submitSubmission :: MonadState ModelAle m
                 => MessagePointer JobOffer -> SubmissionData -> TargetWallets
                 -> m (Maybe SubmitSubmissionResult)
submitSubmission joPtr submData ws = withTarget ws $ \w -> do
    -- TODO: check requirements
    mst <- gets (^? maOffers . at joPtr . _Just . _2)
    case mst of
        Just (Taken contractor) -> do
            let pk = w ^. mwPublicKey
            if contractor /= pk
            then pure $ SubmitSubmissionIllegalState (Taken contractor)
            else do
                let proposalPtr = mkPointer pk $ ContractorProposal joPtr emptyProof
                let submission = Submission proposalPtr submData def
                let submissionPtr = mkPointer pk submission
                maSubmissions . at submissionPtr ?= (joPtr, submission)
                ws . mwSubmissions . at joPtr %= Just . \case
                    Nothing -> [submissionPtr]
                    Just ps -> submissionPtr : ps
                pure $ SubmitSubmissionOk submissionPtr
        Just st -> pure $ SubmitSubmissionIllegalState st
        Nothing -> pure SubmitSubmissionNotFound

-- | List submissions by chosen users.
submissions :: MonadState ModelAle m
                => TargetWallets -> m [MessagePointer Submission]
submissions ws = gets (concatOf $ ws . mwSubmissions . folded)

-- | List submissions for a given offer.
-- Returns 'Nothing' if offer was not found.
offerSubmissions :: MonadState ModelAle m
                 => MessagePointer JobOffer
                 -> m (Maybe [MessagePointer Submission])
offerSubmissions joPtr = do
    mst <- gets (^? maOffers . at joPtr . _Just . _2)
    case mst of
        Nothing         -> pure Nothing
        Just (Taken pk) -> Just <$> gets (^.. maWallets . at pk . _Just
                                            . mwSubmissions . at joPtr . _Just . folded)
        Just _          -> pure (Just [])

data AcceptSubmissionResult
    = AcceptSubmissionOk
    | AcceptSubmissionNotFound
    | AcceptSubmissionIllegalState OfferState

-- | Accept a submission.
acceptSubmission :: MonadState ModelAle m
                 => MessagePointer Submission -> TargetWallets
                 -> m (Maybe AcceptSubmissionResult)
acceptSubmission sPtr ws = withTarget ws $ \_ -> do
    msinfo <- gets (^? maSubmissions . at sPtr . _Just . _1)
    case msinfo of
        Nothing    -> pure AcceptSubmissionNotFound
        Just joPtr -> do
            -- We know that the offer is there
            st <- gets (^?! maOffers . at joPtr . _Just . _2)
            case st of
                Taken pk -> do
                    -- We know that the offer is there
                    partsOf (maOffers . at joPtr . _Just . _2) .= [Finished pk]
                    ws . mwTakenOffers %= S.delete joPtr
                    ws . mwFinishedOffers %= S.insert joPtr
                    pure AcceptSubmissionOk
                _        -> pure $ AcceptSubmissionIllegalState st



------
----
-- Internal helpers
----
------

-- | Helper which makes sure that target wallet exists.
withTarget :: MonadState ModelAle m
           => TargetWallets -> (ModelWallet -> m a) -> m (Maybe a)
withTarget ws act = preuse ws >>= \case
    Nothing     -> pure Nothing
    Just target -> Just <$> act target
