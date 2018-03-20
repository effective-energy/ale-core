{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Global state of Ale and primitives for working with it.
module Ale.State
       ( -- * MonadAleState
         AleState (..)
       , MonadAleState (..)
       , getASBlockOrError
       , getASHeightOrError
       , getASDataOrError

       , AleStateData (AleStateData)
       , Accounts
       , emptyData
       , asdAccounts
       , asdOpenOffers
       , asdTakenOffers
       , asdActiveProposals
       , asdWaitingSubmissions
       , asdSubmissionsByProposal
       , asdSeenMessages
       , asdValidity

       , AleStateDiff (AleStateDiff)

       -- * Updating state
       , updateState
       , applySignedMessage
       , UpdateError (..)
       ) where

import Universum hiding (HashMap)

import Codec.Serialise (Serialise)
import Control.Lens (at, makeLenses, (?~))
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT, withExceptT)
import Data.Default (Default (..))
import Monad.Capabilities (makeCap)

import Ale.Core.Block as B (Block, BlockPointer, Bundle (..), bBundle, bParent, mkPointer,
                            verifyBlock)
import Ale.Core.Crypto (PublicKey)
import Ale.Core.Crypto.Signed as S (Signed, sData, sPublicKey)
import Ale.Core.DHT.Types (MonadDHT (..))
import Ale.Core.Entity (Entity)
import Ale.Core.Height (Height)
import Ale.Core.Message as Msg (ContractorProposal (..), EmployerAcceptance (..), JobOffer,
                                Message (..), MessagePointer, Submission (..), getHeight, joAccept,
                                joPrice, joRequirements, mkPointer)
import Ale.Core.Requirements (validate)
import Ale.Core.Storage (Reference, Storable)
import Ale.Core.Storage.Types (HasPrefixTag (..), StoragePrefix (..))
import Ale.Core.Submission.Acceptance (isFacile)
import Ale.Core.Tokens (TokenBalance, Tokens)
import Ale.Data.HashMap (HashMap)
import Ale.Fmt ((+|), (|+))
import Ale.State.MessageSet (MessageSet)

import Data.List ((\\))

import qualified Ale.Core.Tokens as T
import qualified Ale.Data.HashMap as HM
import qualified Ale.State.MessageSet as MS
import qualified Data.Map as M

-- | The accounts of all the entities.
type Accounts = HashMap Entity TokenBalance

-- | The data that constitues the state of Ale.
data AleStateData = AleStateData
    { _asdAccounts              :: !Accounts
    , _asdOpenOffers            :: !(MessageSet JobOffer)
    , _asdTakenOffers           :: !(MessageSet JobOffer)
    , _asdActiveProposals       :: !(MessageSet ContractorProposal)
    , _asdWaitingSubmissions    :: !(MessageSet Submission)
    , _asdSubmissionsByProposal :: !(Map
                                        (MessagePointer ContractorProposal)
                                        [MessagePointer Submission])
    , _asdSeenMessages          :: HashMap Height (MessageSet Message)
    , _asdValidity              :: Height -- validate pariod from Genesis

    } deriving (Eq, Show, Generic)
makeLenses ''AleStateData

instance Serialise AleStateData

instance HasPrefixTag AleStateData where
    storagePrefix = StoragePrefix "aleStateData"

emptyData :: Height -> AleStateData
emptyData validityPeriod = AleStateData
    { _asdAccounts = HM.empty
    , _asdOpenOffers = MS.empty
    , _asdTakenOffers = MS.empty
    , _asdActiveProposals = MS.empty
    , _asdWaitingSubmissions = MS.empty
    , _asdSubmissionsByProposal = M.empty
    , _asdSeenMessages = HM.empty
    , _asdValidity = validityPeriod
    }

-- | Capability of accessing global Ale state.
data AleState m = AleState
    { _getASBlock  :: m (Either Text BlockPointer)
    , _putASBlock  :: BlockPointer -> m ()

    , _getASHeight :: m (Either Text Height)
    , _putASHeight :: Height -> m ()

    , _getASData   :: m (Either Text AleStateData)
    , _putASData   :: AleStateData -> m ()
    }
makeCap ''AleState

getASBlockOrError :: (Monad m, MonadAleState m) => m BlockPointer
getASBlockOrError = either error identity <$> getASBlock

getASHeightOrError :: (Monad m, MonadAleState m) => m Height
getASHeightOrError = either error identity <$> getASHeight

getASDataOrError :: (Monad m, MonadAleState m) => m AleStateData
getASDataOrError = either error identity <$> getASData

-- two sets for each field in AleStateData (added and deleted)
data AleStateDiff = AleStateDiff
    { _asdiffOpenOffersAdded            :: !(MessageSet JobOffer)
    , _asdiffOpenOffersDeleted          :: !(MessageSet JobOffer)
    , _asdiffTakenOffersAdded           :: !(MessageSet JobOffer)
    , _asdiffTakenOffersDeleted         :: !(MessageSet JobOffer)
    , _asdiffActiveProposalsAdded       :: !(MessageSet ContractorProposal)
    , _asdiffActiveProposalsDeleted     :: !(MessageSet ContractorProposal)
    , _asdiffWaitingSubmissionsAdded    :: !(MessageSet Submission)
    , _asdiffWaitingSubmissionsDeleted  :: !(MessageSet Submission)
    , _asdiffSubmissionsByProposalAdded :: !(Map
        (MessagePointer ContractorProposal) [MessagePointer Submission])
    , _asdiffSubmissionsByProposalDeleted :: !(Map
        (MessagePointer ContractorProposal) [MessagePointer Submission])
    } deriving Show
makeLenses ''AleStateDiff

instance Default AleStateDiff where
    def = AleStateDiff MS.empty MS.empty MS.empty MS.empty
        MS.empty MS.empty MS.empty MS.empty M.empty M.empty

{-# ANN updateState ("HLint: ignore Use ." :: Text) #-}
-- | Process a block by using all of its messages to update the state.
updateState :: forall m. (Monad m, MonadAleState m, MonadDHT m)
            => Block                           -- ^ Block to process
            -> PublicKey                       -- ^ Hub public key
            -> m (Either UpdateError AleStateDiff)
updateState block serverPk = runExceptT applyBlock
  where
    applyBlock :: ExceptT UpdateError m AleStateDiff
    applyBlock = do
        unless (verifyBlock serverPk block) $ throwError NotSignedByServer
        lift getASBlock >>= \case
            Left e -> throwError $ DBError $ "Couldn't get aleState block: " <> e
            Right curBlock -> do
                -- Check that the block follows the current one
                when (block^.bParent /= curBlock) $ throwError BrokenChain

                -- Get list of signed messages from DHT
                Bundle es <- fetch (block^.bBundle)

                lift getASData >>= \case
                    Left e -> throwError $ DBError $ "Couldn't get aleState data: " <> e
                    Right curData -> lift getASHeight >>= \case
                        Left e  -> throwError $ DBError $ "Couldn't get aleState height: " <> e
                        Right h -> do
                            let validityPeriod = curData ^. asdValidity

                            -- Apply the bundle to global state
                            newStd <- foldr (>=>) pure (map (applySignedMessage $ h + 1) es) curData

                            lift $ do
                                putASBlock (B.mkPointer block)
                                putASHeight (h + 1)
                                putASData (flushSeenMessages newStd validityPeriod h)

                            pure $ evalDiff curData newStd

    -- | Try to get data from DHT and wrap the result to make it easier to use it.
    fetch :: Storable v => Reference v -> ExceptT UpdateError m v
    fetch = withExceptT BundleNotFound . ExceptT . getDHT

    -- | Flush outdated seen messages
    flushSeenMessages :: AleStateData -> Height -> Height -> AleStateData
    flushSeenMessages std validityPeriod height = std &
        asdSeenMessages %~ (HM.delete $ height - validityPeriod)

    evalDiff :: AleStateData -> AleStateData -> AleStateDiff
    evalDiff old new = def
        & asdiffOpenOffersAdded .~ MS.difference
            (new ^. asdOpenOffers) (old ^. asdOpenOffers)
        & asdiffOpenOffersDeleted .~ MS.difference
            (old ^. asdOpenOffers) (new ^. asdOpenOffers)

        & asdiffTakenOffersAdded .~ MS.difference
            (new ^. asdTakenOffers) (old ^. asdTakenOffers)
        & asdiffTakenOffersDeleted .~ MS.difference
            (old ^. asdTakenOffers) (new ^. asdTakenOffers)

        & asdiffActiveProposalsAdded .~ MS.difference
            (new ^. asdActiveProposals) (old ^. asdActiveProposals)
        & asdiffActiveProposalsDeleted .~ MS.difference
            (old ^. asdActiveProposals) (new ^. asdActiveProposals)

        & asdiffWaitingSubmissionsAdded .~ MS.difference
            (new ^. asdWaitingSubmissions) (old ^. asdWaitingSubmissions)
        & asdiffWaitingSubmissionsDeleted .~ MS.difference
            (old ^. asdWaitingSubmissions) (new ^. asdWaitingSubmissions)

        & asdiffSubmissionsByProposalAdded .~ M.differenceWith (\x y -> Just $ x \\ y)
            (new ^. asdSubmissionsByProposal) (old ^. asdSubmissionsByProposal)
        & asdiffSubmissionsByProposalDeleted .~ M.differenceWith (\y x -> Just $ x \\ y)
            (old ^. asdSubmissionsByProposal) (new ^. asdSubmissionsByProposal)

-- | Apply a signed message.
applySignedMessage :: (MonadDHT m, MonadError UpdateError m)
                   => Height -> Signed Message -> AleStateData -> m AleStateData
applySignedMessage curHeight signed std = do
    let (entity, msg) = (sPublicKey signed, S.sData signed)
    let mPtr = Msg.mkPointer entity msg
    let mHeight = getHeight msg

    -- If the message needs replay protection
    whenJust mHeight $ \msgHeight -> do
        -- Check that the message is stil valid
        unless (checkHeight (std^.asdValidity) msgHeight) $
            throwError $ IllegalMessage "Message in this block is out of epoch"

        -- Check that the same message has not been seen before
        let found = HM.lookup msgHeight (std ^. asdSeenMessages) >>= MS.lookup mPtr
        when (isJust found) $
            throwError $ IllegalMessage "Replay attack detected"

    std' <- applyMessage entity msg std

    -- If the message needs replay protection, add it to the list of seen messages.
    pure $ case mHeight of
        Nothing -> std'
        Just msgHeight ->
            std' & asdSeenMessages %~ HM.insertWith MS.union
                                                    msgHeight
                                                    (MS.insert entity msg MS.empty)
  where
    -- target <= current < target + k
    checkHeight :: Height -> Height -> Bool
    checkHeight validityPeriod msgHeight = msgHeight <= curHeight
        && curHeight < msgHeight + validityPeriod

-- | Apply the message depending on its kind.
applyMessage :: (MonadDHT m, MonadError UpdateError m)
             => Entity  -- ^ Sender of the message
             -> Message  -- ^ Message to apply
             -> AleStateData -> m AleStateData
applyMessage sender msg std = case msg of
    MsgJobOffer offer          -> applyJobOffer sender offer std
    MsgContractorProposal prop -> applyContractorProposal sender prop std
    MsgSubmission subm         -> applySubmission sender subm std
    MsgEmployerAcceptance acc  -> applyEmployerAcceptance sender acc std


-- | Errors that can happen when updating the state with a block.
data UpdateError
    -- | A block with a different parent expected.
    = BrokenChain
    -- | The bundle of the block cannot be retrieved
    | BundleNotFound !Text
    -- | Block signature is invalid.
    | NotSignedByServer
    -- | The mesasge is illegal in current state
    | IllegalMessage !Text
    -- | Couldn't get from the DB.
    | DBError Text
    deriving (Show, Generic)


----------------------------------------------------------------------------
-- Internal logic
----------------------------------------------------------------------------


-- | An assert-like helper.
makeSure :: MonadError UpdateError m => Bool -> Text -> m ()
makeSure cond txt = unless cond $ throwError (IllegalMessage txt)

-- | Retrieve a message from a message set.
retrieve :: (Serialise msg, MonadError UpdateError m)
         => MessagePointer msg  -- ^ Pointer to the message retrieved
         -> MessageSet msg      -- ^ The set to look in
         -> Text                -- ^ Error to return on fail
         -> m (Entity, msg)
retrieve ptr ms err = case MS.lookup ptr ms of
    Nothing -> throwError $ IllegalMessage err
    Just r  -> pure r

retrieveOpenOffer :: MonadError UpdateError m
                  => MessagePointer JobOffer
                  -> AleStateData
                  -> m (Entity, JobOffer)
retrieveOpenOffer ptr std = retrieve ptr (std^.asdOpenOffers) "No such open offer"

retrieveActiveProposal :: MonadError UpdateError m
                       => MessagePointer ContractorProposal
                       -> AleStateData
                       -> m (Entity, ContractorProposal)
retrieveActiveProposal ptr std = retrieve ptr (std^.asdActiveProposals)
                                     "No such active proposal"

retrieveWaitingSubmission :: MonadError UpdateError m
                          => MessagePointer Submission
                          -> AleStateData
                          -> m (Entity, Submission)
retrieveWaitingSubmission ptr std = retrieve ptr (std^.asdWaitingSubmissions)
                                        "No such waiting submission"

retrieveTakenOffer :: MonadError UpdateError m
                   => MessagePointer JobOffer
                   -> AleStateData
                   -> m (Entity, JobOffer)
retrieveTakenOffer ptr std = retrieve ptr (std^.asdTakenOffers)
                                 "No such taken offer"

-- | A helper to credit an 'Entity' with some 'Tokens'
payTo :: Entity  -- ^ Recepient of the payment
      -> Tokens  -- ^ The amount to pay
      -> Accounts -> Accounts
payTo = HM.insertWith T.addMany


applyJobOffer :: MonadError UpdateError m
              => Entity
              -> JobOffer
              -> AleStateData -> m AleStateData
applyJobOffer sender offer std = do
    -- Try to subtract the price of the contract from the sender's account
    let oldBalance = fromMaybe T.empty $ std^.asdAccounts.at sender
    newBalance <- case T.subMany (offer^.joPrice) oldBalance of
        Left  e -> throwError $ IllegalMessage $ "Not enough tokens of " +| e |+ ""
        Right b -> pure b

    pure $ std & asdAccounts.at sender ?~ newBalance
               & asdOpenOffers %~ MS.insert sender offer

applyContractorProposal :: (MonadDHT m, MonadError UpdateError m)
                        => Entity
                        -> ContractorProposal
                        -> AleStateData -> m AleStateData
applyContractorProposal sender proposal std = do
    -- Grab the offer form the set of open offers
    let offerPtr = cpJobOfferPtr proposal
    (employer, offer) <- retrieveOpenOffer offerPtr std

    -- Check if Contractor really proved Requirements
    reqsValid <- validate (cpProof proposal) (offer^.joRequirements)
    unless reqsValid (throwError $ IllegalMessage "Proof is invalid")

    if isFacile (offer^.joAccept) then
        pure $ std & asdAccounts %~ payTo sender (offer^.joPrice)
                   & asdOpenOffers %~ MS.delete offerPtr
    else
        -- TODO: take security deposit from the contractor
        pure $ std & asdOpenOffers %~ MS.delete offerPtr
                   & asdTakenOffers %~ MS.insert employer offer
                   & asdActiveProposals %~ MS.insert sender proposal

applySubmission :: MonadError UpdateError m
                => Entity
                -> Submission
                -> AleStateData -> m AleStateData
applySubmission sender subm std = do
    let propPtr = sProposalPtr subm
    (propFrom, _) <- retrieveActiveProposal propPtr std

    makeSure (propFrom == sender) "Sender is not the one who proposed"

    let submPtr = Msg.mkPointer sender subm
    pure $ std & asdWaitingSubmissions %~ MS.insert sender subm
               & asdSubmissionsByProposal.at propPtr %~ (Just [submPtr] <>)

applyEmployerAcceptance :: MonadError UpdateError m
                        => Entity
                        -> EmployerAcceptance
                        -> AleStateData -> m AleStateData
applyEmployerAcceptance sender acc std = do
    let submPtr = eaSubmissionPtr acc
    (contractor, subm) <- retrieveWaitingSubmission submPtr std

    let proposalPtr = sProposalPtr subm
    (_, proposal) <- retrieveActiveProposal proposalPtr std

    let offerPtr = cpJobOfferPtr proposal
    (employer, offer) <- retrieveTakenOffer offerPtr std

    makeSure (employer == sender) "Sender is not the one who offered the job"

    let outdated = concat $ std^.asdSubmissionsByProposal.at proposalPtr

    pure $ std & asdAccounts %~ payTo contractor (offer^.joPrice)
               & asdTakenOffers %~ MS.delete offerPtr
               & asdActiveProposals %~ MS.delete proposalPtr
               & foldr (.) identity
                     [asdWaitingSubmissions %~ MS.delete sPtr | sPtr <- outdated]
