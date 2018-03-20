{-# LANGUAGE TypeFamilies #-}

-- | Initial distribution of tokens
module Ale.Core.Genesis.Distribution
       ( BalanceDistribution (BalanceDistribution)
       , toAccounts

       , mkUniform
       , mkOwnUniform
       , mkMoneyUniform
       ) where

import Universum hiding (HashMap)

import Codec.Serialise (Serialise (..))
import Control.Lens (At (at), Index, IxValue, Ixed (ix), iso)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Serokell.Aeson.Options (defaultOptions)

import Ale.Core.Entity (Entity)
import Ale.Core.Tokens (TokenBalance, TokenCount, TokenKind)
import Ale.Data.HashMap (HashMap)
import Ale.State (Accounts)

import qualified Ale.Core.Tokens as T
import qualified Ale.Data.HashMap as HM

-- | Records the initial balances of the entities.
newtype BalanceDistribution = BalanceDistribution
    { getBalanceDistribution :: HashMap Entity TokenBalance
    } deriving (Eq, Generic, Show)

instance Serialise BalanceDistribution where

instance ToJSON BalanceDistribution where
    toJSON = genericToJSON defaultOptions
    toEncoding = genericToEncoding defaultOptions

instance FromJSON BalanceDistribution where
    parseJSON = genericParseJSON defaultOptions

instance Ixed BalanceDistribution where
    ix i = iso getBalanceDistribution BalanceDistribution . ix i

instance At BalanceDistribution where
    at i = iso getBalanceDistribution BalanceDistribution . at i

type instance Index BalanceDistribution = Entity
type instance IxValue BalanceDistribution = TokenBalance

toAccounts :: BalanceDistribution -> Accounts
toAccounts (BalanceDistribution m) = m

-- | Generates a 'BalanceDistribution' which gives every entity an equal amount
-- of tokens issued by someone else.
mkUniform :: [Entity]     -- ^ Entities that receive the tokens
          -> [TokenKind]  -- ^ Issuers of the tokens given
          -> TokenCount   -- ^ The number of tokens to give
          -> BalanceDistribution
mkUniform es is share = BalanceDistribution $ HM.fromList balances
  where
    balances = map (\e -> (e, balance)) es
    balance = T.fromList $ map (\e -> (e, share)) is

-- | Generates a 'BalanceDistribution' which gives every entity an equal amount
-- of tokens issued by themselves.
mkOwnUniform :: [Entity] -> TokenCount -> BalanceDistribution
mkOwnUniform es = mkUniform es $ map T.Transferable es

-- | Generates a 'BalanceDistribution' which gives every entity an equal amount
-- of uncoloured money.
mkMoneyUniform :: [Entity] -> TokenCount -> BalanceDistribution
mkMoneyUniform es = mkUniform es [T.Money]
