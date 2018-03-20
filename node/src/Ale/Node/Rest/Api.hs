{-# LANGUAGE TypeOperators #-}

-- | REST API
module Ale.Node.Rest.Api
       ( Node        (..)
       , NodeMessage (..)
       , NodeStorage (..)
       , NodeState   (..)
       , Api

       , swagger
       ) where

import Universum

import GHC.Generics (Generic)

import Data.Swagger (Swagger)
import Servant.API ((:>), Capture, Get, JSON, NoContent, OctetStream, PlainText, Post, PutAccepted,
                    ReqBody)
import Servant.API.Description (Summary)
import Servant.Generic ((:-), AsApi, ToServant)
import Servant.Swagger (toSwagger)

import Ale.Core.Block (BlockPointer)
import Ale.Core.Crypto.Signed (Signed)
import Ale.Core.Entity (Entity)
import Ale.Core.Message (ContractorProposal, JobDescription, JobOffer, Message, MessagePointer,
                         Submission)
import Ale.Core.Storage (Reference)
import Ale.Core.Tokens (TokenBalance, TokenCount, TokenKind)
import Ale.Node.Rest.Instances ()


-- | @servant-generic@ API specification.
data Node route = Node
    { nPing :: route
        :- Summary "Ping message to ensure REST works"
        :> "ping"
        :> Get '[PlainText] Text
    , nMessage :: route
        :- "message"
        :> ToServant (NodeMessage AsApi)
    , nStorage :: route
        :- "storage"
        :> ToServant (NodeStorage AsApi)
    , nState :: route
        :- "state"
        :> ToServant (NodeState AsApi)
    } deriving Generic

data NodeMessage route = NodeMessage
    { nmPost :: route
        :- Summary "Post new message"
        :> ReqBody '[JSON] (Signed Message)
        :> Post '[JSON] NoContent

    , nmJobOffer :: route
        :- Summary "Obtain the job offer from a pointer"
        :> Capture "jobOffer" (MessagePointer JobOffer)
        :> Get '[JSON] JobOffer
    } deriving Generic

data NodeStorage route = NodeStorage
    { postJobDescription :: route
        :- Summary "Store a new job description in the Storage"
        :> "jobDescription"
        :> ReqBody '[OctetStream] JobDescription
        :> PutAccepted '[JSON] (Reference JobDescription)
    } deriving Generic

data NodeState route = NodeState
    { nsBlockHash :: route
        :- Summary "Get the last block pointer"
        :> "block"
        :> Get '[JSON] BlockPointer

    , nsAccount :: route
        :- Summary "Get balance information for this account"
        :> "account"
        :> Capture "account" Entity
        :> Get '[JSON] (Maybe TokenBalance)

    , nsCashOf :: route
        :- Summary "Get amount of these tokens for this account"
        :> "account"
        :> Capture "account" Entity
        :> Capture "token-kind" TokenKind
        :> Get '[JSON] TokenCount

    , nsOpenOffers :: route
        :- Summary "Get a list of open job offers"
        :> "offers"
        :> "list"
        :> "open"
        :> Get '[JSON] [MessagePointer JobOffer]

    , nsProposalSubmissions :: route
        :- Summary "Get a list of submissions for this contractor proposal"
        :> "submissions"
        :> "list"
        :> "for"
        :> Capture "proposal" (MessagePointer ContractorProposal)
        :> Get '[JSON] [MessagePointer Submission]
    } deriving (Generic)

-- | @Servant@ API type.
type Api = ToServant (Node AsApi)


-- | Swagger specification generator.
swagger :: Swagger
swagger = toSwagger (Proxy :: Proxy Api)
