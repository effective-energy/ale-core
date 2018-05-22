{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | REST server implementation
module Ale.Wallet.Rest.Server
       ( walletServer
       ) where

import Universum

import Control.Lens (at, to)
import Crypto.Random (getRandomBytes)
import Data.Default (def)
import Data.Proxy (Proxy (Proxy))
import Data.Time.Clock (getCurrentTime)
import Monad.Capabilities (Capabilities, HasCaps)
import Network.Wai (Application)
import Servant (Handler (Handler), NoContent (..), ServantErr, err404, hoistServer, serve)
import Servant.Generic (toServant)
import System.Wlog (logDebug)

import Ale.Core.Crypto (PublicKey, generateSecretKey, toPublic)
import Ale.Core.Crypto.Signed (Signed (sData), mkSigned)
import Ale.Core.DHT.Types (DHT, MonadDHT (..))
import Ale.Core.Message (ContractorProposal (..), EmployerAcceptance (..), JobOffer, Message (..),
                         MessagePointer, Submission (Submission), jobOffer, mkPointer, _MsgJobOffer)
import Ale.Core.Requirements (emptyProof)
import Ale.Core.Submission.Acceptance (alwaysFail)
import Ale.Fmt ((+|), (|+))
import Ale.Node.Context.Logging (Logging)
import Ale.Node.Context.Mining (Mining, MonadMining (mine))
import Ale.Node.Rest.Server (AsServerWithEnv, nodeServer')
import Ale.State (AleState, AleStateData, asdActiveProposals, asdSubmissionsByProposal,
                  asdWaitingSubmissions, getASDataOrError, getASHeightOrError)
import Ale.Transactions (transfer)
import Ale.Wallet.Rest.Api (Api, Wallet (..), WalletRoot (..))
import Ale.Wallet.State (MonadWalletState (..), WalletState)
import Ale.Wallet.Types (Transaction (..), WalletInfo (..), WalletInfoSecret (..))

import qualified Ale.Core.Tokens as T
import qualified Ale.State.MessageSet as MS
import qualified Ale.Wallet.FrontendMessage as Frontend

-- | We use our custom Handler type because we need a 'MonadMask' instance for it.
type Handler' = ExceptT ServantErr IO

-- | @servant-generic@ implementation of the Wallet REST API.
walletServer' :: forall caps. HasCaps '[WalletState, AleState, Mining, DHT, Logging] caps
              => WalletRoot (AsServerWithEnv caps)
walletServer' = WalletRoot {..}
  where
    _wrNode = toServant nodeServer'

    _wrNewWallet = do
        sk <- liftIO generateSecretKey
        WalletInfo {..} <- _wrImportWallet sk
        pure WalletInfoSecret
            { wisPublicKey = wiPublicKey
            , wisSecretKey = sk
            }

    _wrImportWallet sk = do
        initWS sk
        pure WalletInfo
            { wiPublicKey = toPublic sk
            }

    _wrWallets = do
        logDebug "/wallets requested"
        map toPublic <$> getWSManagedSecretKeys

    _wrWallet pk = toServant wrWallet
      where
        wrWallet :: Wallet (AsServerWithEnv caps)
        wrWallet = Wallet
            { _wInfo = pure WalletInfo
                           { wiPublicKey = pk
                           }
            , _wTransactions = getWSTransactions pk
            , _wCreateTransaction = \receiver amount -> do
                    sk <- getWSSecret pk >>= unEither404
                    logDebug $ "Secret key for "+|pk|+" found"
                    challenge <- getRandomBytes 32
                    height <- getASHeightOrError
                    let msg = transfer
                                  sk
                                  height
                                  (T.fromList [(T.Money, amount)])
                                  receiver
                                  challenge
                    mine msg

                    curTime <- liftIO getCurrentTime
                    let tr = Transaction
                                 { tAmount    = amount
                                 , tReceiver  = receiver
                                 , tSender    = pk
                                 , tTimestamp = curTime
                                 }

                    addWSTransaction pk tr
                    addWSTransaction receiver tr

                    _ <- unMaybe404 $ msg^?to sData._MsgJobOffer
                    pure tr


            , _wPublishOffer = \frontendJO -> do
                    sk <- getWSSecret pk >>= unEither404
                    jo <- realJO frontendJO
                    let msg = mkSigned sk (MsgJobOffer jo)
                    mine msg
                    addWSCreatedOffer pk jo
                    pure $ mkPointer (toPublic sk) jo

            , _wOffersCreated = getWSCreatedOffers pk
            , _wOfferSubmissions = \joPtr ->
                    submissionsForJobOffer joPtr <$> getASDataOrError
            , _wAcceptSubmission = \submPtr -> do
                    sk <- getWSSecret pk >>= unEither404
                    mine $ mkSigned sk $ MsgEmployerAcceptance $ EmployerAcceptance submPtr
                    pure NoContent

            , _wOffersTaken = do
                    std <- getASDataOrError
                    pure
                        [ cpJobOfferPtr cp
                        | (_, sender, cp) <- std^.asdActiveProposals.to MS.toList
                        , sender == pk
                        ]

            , _wTakeOffer = \joPtr -> do
                    sk <- getWSSecret pk >>= unEither404
                    -- TODO proof
                    mine $ mkSigned sk $ MsgContractorProposal $ ContractorProposal joPtr emptyProof
                    pure NoContent
            , _wSubmitSubmission = \joPtr submData -> do
                    std <- getASDataOrError
                    cpPtr <- proposalForJobOffer joPtr pk std
                    sk <- getWSSecret pk >>= unEither404
                    let subm = Submission cpPtr submData def
                    mine $ mkSigned sk $ MsgSubmission subm
                    pure $ mkPointer pk subm
            , _wSubmissions = do
                    std <- getASDataOrError
                    pure
                        [ submPtr
                        | (submPtr, sender, _) <- std^.asdWaitingSubmissions.to MS.toList
                        , sender == pk
                        ]
            , _wOffersFinished = getWSFinishedOffers pk
            }


-- Wallet REST API 'Application'.
walletServer :: forall caps. HasCaps '[WalletState, AleState, Mining, DHT, Logging] caps
             => Capabilities caps Handler' -> Application
walletServer caps = serve (Proxy :: Proxy Api) $ hoistServer
    (Proxy :: Proxy Api) (Handler . usingReaderT caps) (toServant walletServer')

----------------------------------------
-- Internal
----------------------------------------

unMaybe404 :: MonadThrow m => Maybe a -> m a
unMaybe404 = maybe (throwM err404) pure

unEither404 :: MonadThrow m => Either e a -> m a
unEither404 = either (const $ throwM err404) pure

realJO :: (Monad m, MonadDHT m) => Frontend.JobOffer -> m JobOffer
realJO Frontend.JobOffer{..} = do
    joDescr <- Just <$> putDHT joDescription
    let joPr = T.fromList [(T.Money, joPrice)]
    -- TODO put here something better that 'alwaysPass' when it's
    -- invented.
    pure $ jobOffer joDescr joDeadline joPr joRequirements alwaysFail

-- | TODO this can be quite inefficient.  We need to add more
-- structures to Wallet (or even Ale) state to handle such requests.
submissionsForJobOffer :: MessagePointer JobOffer -> AleStateData -> [MessagePointer Submission]
submissionsForJobOffer joPtr std = submPtrs
  where
    props :: [MessagePointer ContractorProposal]
    props = [ cpPtr
            | (cpPtr, _, cp) <- std^.asdActiveProposals.to MS.toList
            , cpJobOfferPtr cp == joPtr
            ]

    submPtrs :: [MessagePointer Submission]
    submPtrs = concatMap concat
       [std^.asdSubmissionsByProposal.at prop | prop <- props]

-- | TODO can be faster
proposalForJobOffer :: (MonadThrow m)
                    => MessagePointer JobOffer
                    -> PublicKey
                    -> AleStateData
                    -> m (MessagePointer ContractorProposal)
proposalForJobOffer joPtr pk std = do
    let good x = x^._2 == pk && x^._3.to cpJobOfferPtr == joPtr
    let props = filter good $ std^.asdActiveProposals.to MS.toList
    unMaybe404 $ map (^._1) $ safeHead props
