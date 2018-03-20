-- | Functions to access REST API on the client.
-- Ideally, @servant-generic@ should generate all these data types for us,
-- but it does not for some reason.
module Ale.Node.Rest.Client
       ( Node (..)
       , NodeMessage (..)
       , NodeStorage (..)
       , NodeState (..)
       , node
       ) where

import Universum

import Data.Proxy (Proxy (Proxy))

import Servant.API ((:<|>) (..), NoContent)
import Servant.Client (ClientM, client)

import Ale.Core.Block (BlockPointer)
import Ale.Core.Crypto.Signed (Signed)
import Ale.Core.Entity (Entity)
import Ale.Core.Message (ContractorProposal, JobDescription, JobOffer, Message, MessagePointer,
                         Submission)
import Ale.Core.Storage (Reference)
import Ale.Core.Tokens (TokenBalance, TokenCount, TokenKind)
import Ale.Node.Rest.Api (Api)


-- | All REST client functions.
data Node = Node
    { nPing    :: ClientM Text
    , nMessage :: NodeMessage
    , nStorage :: NodeStorage
    , nState   :: NodeState
    }

-- | REST functions that work with messages.
data NodeMessage = NodeMessage
    { nmPost     :: Signed Message -> ClientM NoContent
    , nmJobOffer :: MessagePointer JobOffer -> ClientM JobOffer
    }

-- | REST functions that work with storage.
data NodeStorage = NodeStorage
    { postJobDescription :: JobDescription -> ClientM (Reference JobDescription)
    }

data NodeState = NodeState
    { nsBlockHash           :: ClientM BlockPointer
    , nsAccount             :: Entity -> ClientM (Maybe TokenBalance)
    , nsCashOf              :: Entity -> TokenKind -> ClientM TokenCount
    , nsOpenOffers          :: ClientM [MessagePointer JobOffer]
    , nsProposalSubmissions :: MessagePointer ContractorProposal -> ClientM [MessagePointer Submission]
    }

-- | Client REST API implementation.
--   Generated for free by @servant@.
node :: Node
node = Node {..}
  where
    nPing    = nPing'
    nMessage = NodeMessage {..}
    nStorage = NodeStorage {..}
    nState   = NodeState   {..}

    (nPing' :<|> nMessage') :<|> (storage' :<|> state') = client (Proxy :: Proxy Api)

    (nmPost :<|> nmJobOffer) = nMessage'

    postJobDescription = storage'

    (nsBlockHash :<|> nsAccount) :<|> (nsCashOf :<|> (nsOpenOffers :<|> nsProposalSubmissions)) = state'
