-- | This module provides helper function for 'wallet' tool.  This
-- functions are intended to make claiming money for multiple keys
-- simpler.

module Ale.Wallet.Claim
       ( claimAll
       ) where

import Universum

import Ale.Core.Crypto (SecretKey)
import Ale.Core.Crypto.Signed (Signed)
import Ale.Core.Message (Message)
import Ale.State (AleStateData, asdOpenOffers)
import Ale.Transactions (receive)

import qualified Ale.State.MessageSet as MS

-- | Extract everything we are able to claim from 'st' while having
-- 'keys' secret keys.
claimAll :: [SecretKey] -> AleStateData -> [Signed Message]
claimAll sKeys std = concatMap (`claimOne` std) sKeys

----------------------------------------
-- Internal
----------------------------------------

claimOne :: SecretKey -> AleStateData -> [Signed Message]
claimOne key std = rights [receive key offerPtr offer | (offerPtr, _, offer) <- offers]
  where
    offers = MS.toList $ std^.asdOpenOffers
