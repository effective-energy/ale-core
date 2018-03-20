-- | Frontend specific (simplified) protocol messages.
module Ale.Wallet.FrontendMessage
       ( JobOffer (..)
       , toInnerJobOffer
       ) where

import Universum

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))

import Ale.Core.Message (Deadline, JobDescription)
import Ale.Core.Requirements (Requirements)
import Ale.Core.Storage.Types (mkReference)
import Ale.Core.Submission.Acceptance (alwaysFail)
import Ale.Core.Tokens (TokenCount, TokenKind (..), fromHashMap)
import Ale.Data.HashMap (singleton)

import qualified Ale.Core.Message as Msg (JobOffer, jobOffer)


-- | Simplified 'JobOffer'.
data JobOffer = JobOffer
    { joDescription  :: !JobDescription
    , joDeadline     :: !(Maybe Deadline)
    , joPrice        :: !TokenCount
    , joRequirements :: !Requirements
    } deriving Generic

instance ToJSON JobOffer where
    toJSON JobOffer{..} = object [ "descr"    .= joDescription
                                 , "deadline" .= joDeadline
                                 , "price"    .= joPrice
                                 , "reqs"     .= joRequirements
                                 ]

instance FromJSON JobOffer where
    parseJSON = withObject "FrontendJobOffer" $ \o -> JobOffer
        <$> o .:  "descr"
        <*> o .:? "deadline"
        <*> o .:  "price"
        <*> o .:  "reqs"

-- TODO: change mkReference on getting real reference from DHT
-- TODO: think about acceptance test (?)
toInnerJobOffer :: JobOffer -> Msg.JobOffer
toInnerJobOffer (JobOffer desc deadline price reqs) =
    Msg.jobOffer (Just $ mkReference desc) deadline
        (fromHashMap $ singleton Money price) reqs alwaysFail
