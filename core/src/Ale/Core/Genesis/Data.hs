-- | Data that contributes to the Genesis Block parent hash
module Ale.Core.Genesis.Data
       ( GenesisData(..)
       , gdDistribution
       , gdServerKey
       , gdValidityPeriod
       , initialStateData
       , defaultValidityPeriod
       ) where

import Universum

import Codec.Serialise (Serialise)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (parseJSON), ToJSON (..), genericParseJSON, genericToEncoding,
                   genericToJSON)
import Serokell.Aeson.Options (defaultOptions)

import Ale.Core.Crypto (PublicKey)
import Ale.Core.Height (Height)
import Ale.Core.Genesis.Distribution (BalanceDistribution, toAccounts)
import Ale.Core.Storage (HasPrefixTag (..), Storable, StoragePrefix (..))
import Ale.State (AleStateData (..), asdAccounts, emptyData)


-- | All the information conatined in this data type gets hashed
-- and the resulting hash is used as the parent of the Genesis Block.
data GenesisData = GenesisData
    { _gdDistribution   :: !BalanceDistribution
    , _gdServerKey      :: !PublicKey
    , _gdValidityPeriod :: !Height
    } deriving (Eq, Generic, Show)

makeLenses ''GenesisData

instance Serialise GenesisData where

instance ToJSON GenesisData where
    toJSON     = genericToJSON defaultOptions
    toEncoding = genericToEncoding defaultOptions

instance FromJSON GenesisData where
    parseJSON = genericParseJSON defaultOptions

instance HasPrefixTag GenesisData where
    storagePrefix = StoragePrefix "gendat"

instance Storable GenesisData where

initialStateData :: GenesisData -> AleStateData
initialStateData gd = emptyData (gd ^. gdValidityPeriod)
    & asdAccounts .~ toAccounts (gd ^. gdDistribution)

defaultValidityPeriod :: Height
defaultValidityPeriod =  5
