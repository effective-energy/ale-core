{-# LANGUAGE TypeOperators #-}

-- | Wallet API
module Ale.Wallet.Rest.Api
       ( WalletRoot (..)
       , Wallet (..)

       , Api

       , swagger
       ) where

import Universum

import Data.Swagger (Swagger)
import Servant.API ((:>), Capture, Get, JSON, NoContent, PostCreated, PutAccepted, ReqBody)
import Servant.API.Description (Summary)
import Servant.Generic ((:-), AsApi, ToServant)
import Servant.Swagger (toSwagger)

import Ale.Core.Crypto (PublicKey, SecretKey)
import Ale.Core.Entity (Entity)
import Ale.Core.Message (JobOffer, MessagePointer, Submission, SubmissionData)
import Ale.Core.Tokens (TokenCount)
import Ale.Node.Rest.Api (Node)
import Ale.Node.Rest.Instances ()
import Ale.Wallet.Rest.Instances ()
import Ale.Wallet.Types (Transaction, WalletInfo, WalletInfoSecret)

import qualified Ale.Wallet.FrontendMessage as Frontend (JobOffer)


-- | The root of the Walet API, which includes both Wallet endpoints
-- and regular node endpoints.
data WalletRoot route = WalletRoot
    { _wrNode :: route
        :- "node"
        :> ToServant (Node AsApi)

    , _wrNewWallet :: route
        :- Summary "Create a new wallet"
        :> "newWallet"
        :> PostCreated '[JSON] WalletInfoSecret
    , _wrImportWallet :: route
        :- Summary "Import an existing wallet"
        :> "importWallet"
        -- Not taking as part of the query but in the request body
        -- in case HTTP requests are logged on the server.
        :> ReqBody '[JSON] SecretKey
        :> PutAccepted '[JSON] WalletInfo

    , _wrWallets :: route
        :- Summary "Get a list of accessible wallets"
        :> "wallets"
        :> Get '[JSON] [PublicKey]

    , _wrWallet :: route
        :- "wallets"
        :> Capture "account" PublicKey
        :> ToServant (Wallet AsApi)
    } deriving (Generic)


-- | Part of the API speicific to Wallet.
data Wallet route = Wallet
    { _wInfo :: route
        :- Summary "Get a general wallet information"
        :> "info"
        :> Get '[JSON] WalletInfo

    , _wTransactions :: route
        :- Summary "Get a list of transactions"
        :> "transactions"
        :> "list"
        :> Get '[JSON] [Transaction]

    , _wCreateTransaction :: route
        :- Summary "Create a new transaction"
        :> "transactions"
        :> "create"
        :> Capture "to_address" Entity
        :> Capture "amount" TokenCount
        :> PostCreated '[JSON] Transaction


-- Employer API

    , _wPublishOffer :: route
        :- Summary "Publish a job offer"
        :> "offers"
        :> "publish"
        :> ReqBody '[JSON] Frontend.JobOffer
        :> PostCreated '[JSON] (MessagePointer JobOffer)

    , _wOffersCreated :: route
        :- Summary "Get a list of job offers created by user"
        :> "offers"
        :> "list"
        :> "created"
        :> Get '[JSON] [MessagePointer JobOffer]

    , _wOfferSubmissions :: route
        :- Summary "Get a list of submissions for this job offer"
        :> "submissions"
        :> "list"
        :> "for"
        :> Capture "offer" (MessagePointer JobOffer)
        :> Get '[JSON] [MessagePointer Submission]

    , _wAcceptSubmission :: route
        :- Summary "Accept this submission"
        :> "submissions"
        :> "accept"
        :> Capture "submission" (MessagePointer Submission)
        :> PutAccepted '[JSON] NoContent


-- Contractor API

    , _wOffersTaken :: route
        :- Summary "Get a list of job offers taken by user"
        :> "offers"
        :> "list"
        :> "taken"
        :> Get '[JSON] [MessagePointer JobOffer]

    , _wTakeOffer :: route
        :- Summary "Take this offer"
        :> "offers"
        :> "take"
        :> Capture "offer" (MessagePointer JobOffer)
        :> PutAccepted '[JSON] NoContent

    , _wSubmitSubmission :: route
        :- Summary "Submit a result of work (submission)"
        :> "submissions"
        :> "submit"
        :> Capture "offer" (MessagePointer JobOffer)
        :> ReqBody '[JSON] SubmissionData
        :> PutAccepted '[JSON] (MessagePointer Submission)

    , _wSubmissions :: route
        :- Summary "Get a list of user's submissions"
        :> "submissions"
        :> "list"
        :> Get '[JSON] [MessagePointer Submission]

    , _wOffersFinished :: route
        :- Summary "Get a list of job offers done by the user"
        :> "offers"
        :> "list"
        :> "finished"
        :> Get '[JSON] [MessagePointer JobOffer]
    } deriving Generic


-- | @Servant@ API type.
type Api = ToServant (WalletRoot AsApi)


-- | Swagger specification generator.
swagger :: Swagger
swagger = toSwagger (Proxy :: Proxy Api)
