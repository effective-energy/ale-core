{-# LANGUAGE TypeOperators #-}

module Main
       ( main
       ) where

import Universum

import Codec.Serialise (deserialise, serialise)
import Control.Lens (at)
import Control.Lens.Prism (_Just)
import Control.Monad.Except (MonadError (throwError))
import Data.Proxy (Proxy)
import Data.Swagger (Swagger)
import Network.Wai.Handler.Warp (run)
import Servant.API ((:<|>) ((:<|>)), (:>), Get, JSON, NoContent (NoContent), PlainText)
import Servant.Generic (AsServerT, toServant)
import Servant.Server (Handler, ServantErr (errBody), Server, ServerT, err404, err409, err412,
                       err501, hoistServer, serve)
import Servant.Swagger (toSwagger)
import System.FilePath ((</>))
import System.IO.Error (IOError)

import Ale.Core.Crypto (generateSecretKey)
import Ale.Model (AcceptSubmissionResult (..), ModelAle (..), SubmitSubmissionResult (..),
                  TakeOfferResult (..), TargetWallets, acceptSubmission, createTransaction,
                  emptyModelAle, importWallet, info, maOffers, moneyBalance, offerSubmissions,
                  offersAllOpen, offersCreated, offersFinished, offersTaken, publishOffer,
                  submissions, submitSubmission, takeOffer, targetWallet, transactions, wallets)
import Ale.Node.Rest.Api (Node (..), NodeMessage (..), NodeState (..), NodeStorage (..))
import Ale.Tools.Paths (getDataDir)
import Ale.Wallet.Rest.Api (Api, Wallet (..), WalletRoot (..))
import Ale.Wallet.Types (WalletInfo (..), WalletInfoSecret (..))

import qualified Data.ByteString.Lazy as BSL (readFile, writeFile)


main :: IO ()
main = do
    aleModel <- loadModelAle
    walletState <- newIORef aleModel
    finally (run' walletState) (readIORef walletState >>= saveModelAle)
  where
    run' :: IORef ModelAle -> IO ()
    run' ws = run 8080 $ serve (Proxy :: Proxy (SpecApi :<|> WalletStateApi :<|> Api)) (server ws)

stateCacheFile :: IO FilePath
stateCacheFile = fmap (</> "ale-wallet-model-cache") getDataDir

loadModelAle :: IO ModelAle
loadModelAle = catch readModelAle (\(_ :: IOError) -> return emptyModelAle)
  where
    readModelAle :: IO ModelAle
    readModelAle = fmap deserialise $ stateCacheFile >>= BSL.readFile

saveModelAle :: ModelAle -> IO ()
saveModelAle m = stateCacheFile >>= flip BSL.writeFile (serialise m)

-- | We implement the same API as Wallet, but in addition we serve the specification.
type SpecApi = "openapi.json" :> Get '[JSON] Swagger

-- | Wallet introspection API for debugging.
type WalletStateApi = "wallet-state" :> Get '[PlainText] Text

server :: IORef ModelAle -> Server (SpecApi :<|> WalletStateApi :<|> Api)
server wSt = pure (toSwagger (Proxy :: Proxy Api))
        :<|> hoistServer (Proxy :: Proxy (WalletStateApi :<|> Api)) nt
                 (gets show :<|> walletServer)
  where
    nt :: forall x. StateT ModelAle Handler x -> Handler x
    nt m = do
        st <- readIORef wSt
        (a, st') <- runStateT m st
        writeIORef wSt st'
        pure a


type AsWalletServer = AsServerT (StateT ModelAle Handler)

walletServer :: ServerT Api (StateT ModelAle Handler)
walletServer = toServant (WalletRoot{..} :: WalletRoot AsWalletServer)
  where
    _wrNode = toServant (Node{..} :: Node AsWalletServer)
      where
        nPing = throwError err501

        nMessage = toServant (NodeMessage{..} :: NodeMessage AsWalletServer)
          where
            nmPost _ = throwError err501

            nmJobOffer joPtr = whenNothingM (preuse $ maOffers . at joPtr . _Just . _1)
                (throwError err404 {errBody = "Job offer not found"})

        nStorage = toServant (NodeStorage{..} :: NodeStorage AsWalletServer)
          where
            postJobDescription _ = throwError err501

        nState = toServant (NodeState{..} :: NodeState AsWalletServer)
          where
            nsBlockHash = throwError err501

            nsAccount _ = throwError err501
            nsCashOf pk _ = moneyBalance pk

            nsOpenOffers = offersAllOpen

            nsProposalSubmissions _ = throwError err501

    _wrNewWallet = do
        sk <- liftIO generateSecretKey
        WalletInfo {..} <- _wrImportWallet sk
        pure WalletInfoSecret
            { wisPublicKey = wiPublicKey
            , wisSecretKey = sk
            }

    _wrImportWallet = importWallet
    _wrWallets = wallets

    _wrWallet pk = toServant (Wallet{..} :: Wallet AsWalletServer)
      where
        -- | Helper for actions that need current wallet.
        withWallet' :: MonadState ModelAle m
                    => (a -> m b) -> (TargetWallets -> m a) -> m b
        withWallet' f act = act (targetWallet pk) >>= f

        -- | Helper for actions that only fail if the wallet is not found.
        withWallet :: (MonadIO m, MonadState ModelAle m, MonadError ServantErr m)
                   => (TargetWallets -> m (Maybe a)) -> m a
        withWallet = withWallet' $ \case
            Nothing -> throwError err404 { errBody = "Wallet not found" }
            Just a  -> pure a


        _wInfo = withWallet info

        _wTransactions = withWallet transactions
        _wCreateTransaction receiver amount =
            withWallet (createTransaction receiver amount) >>= \case
                Nothing -> throwError err412 { errBody = "Not enough money" }
                Just jo -> pure jo

        _wPublishOffer jo = withWallet (publishOffer jo)
        _wOffersCreated = offersCreated (targetWallet pk)
        _wOfferSubmissions jo = offerSubmissions jo >>= \case
            Nothing -> throwError err404 { errBody = "Offer not found" }
            Just ss -> pure ss
        _wAcceptSubmission s = withWallet (acceptSubmission s) >>= \case
            AcceptSubmissionOk ->
                pure NoContent
            AcceptSubmissionIllegalState _st ->
                throwError err409 { errBody = "Offer is in wrong state" }
            AcceptSubmissionNotFound ->
                throwError err404 { errBody = "Submission not found" }

        _wOffersTaken = offersTaken (targetWallet pk)
        _wTakeOffer jo = withWallet (takeOffer jo) >>= \case
            TakeOfferOk ->
                pure NoContent
            TakeOfferIllegalState _st ->
                throwError err409 { errBody = "Offer is in wrong state" }
            TakeOfferNotFound ->
                throwError err404 { errBody = "Job offer not found" }
        _wSubmitSubmission joPtr sd = withWallet (submitSubmission joPtr sd) >>= \case
            SubmitSubmissionOk sPtr ->
                pure sPtr
            SubmitSubmissionIllegalState _st ->
                throwError err409 { errBody = "Offer is in wrong state" }
            SubmitSubmissionNotFound ->
                throwError err404 { errBody = "Job offer not found" }
        _wSubmissions = submissions (targetWallet pk)
        _wOffersFinished = offersFinished (targetWallet pk)
