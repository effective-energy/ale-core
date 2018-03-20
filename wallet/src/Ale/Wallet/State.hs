{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides 'AleWalletState' class that represents
-- (surprise!) Wallet internal state.
module Ale.Wallet.State
       ( WalletState (..)
       , MonadWalletState (..)
       , walletStateImpl
       ) where

import Universum

import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.List (consume)
import Monad.Capabilities (CapImpl (CapImpl), makeCap)

import Ale.Capabilities (CapImplIODB)
import Ale.Core.Crypto (PublicKey, SecretKey, toPublic)
import Ale.Core.Message (JobOffer, MessagePointer, mkPointer)
import Ale.Node.DB.Types (Collection (..), CollectionPrefix (..), DB, MyNat (..), getDBCollection,
                          iterDBCollection, listDBCollectionPrefix, putDBCollection)
import Ale.Wallet.Crypto (StorableSecretKey (..))
import Ale.Wallet.Types (Transaction)

import qualified Data.Conduit.List as CL


walletSecretsColl :: Collection StorableSecretKey '[PublicKey] StorableSecretKey
walletSecretsColl = Collection
    { cName = "secretKeys"
    , cToKeys = toPublic . getStorableSecretKey
    , cToItem = identity
    , cRecollect = flip const
    }

walletTransactionsColl :: Collection (PublicKey, Transaction) '[PublicKey] Transaction
walletTransactionsColl = Collection
    { cName = "walletTransactions"
    , cToKeys = fst
    , cToItem = snd
    , cRecollect = (,)
    }

walletFinishedOffersColl :: Collection (PublicKey, MessagePointer JobOffer, JobOffer)
                                       '[PublicKey, MessagePointer JobOffer]
                                       JobOffer
walletFinishedOffersColl = Collection
    { cName = "walletFinishedOffers"
    , cToKeys = \(pk, joPtr, _jo) -> (pk, joPtr)
    , cToItem = (^._3)
    , cRecollect = \(pk, joPtr) jo -> (pk, joPtr, jo)
    }

walletCreatedOffersColl :: Collection (PublicKey, MessagePointer JobOffer, JobOffer)
                                       '[PublicKey, MessagePointer JobOffer]
                                       JobOffer
walletCreatedOffersColl = Collection
    { cName = "walletCreatedOffers"
    , cToKeys = \(pk, joPtr, _jo) -> (pk, joPtr)
    , cToItem = (^._3)
    , cRecollect = \(pk, joPtr) jo -> (pk, joPtr, jo)
    }

-- | 'WalletState' manipulation functions.  Currently, 'WalletState'
-- stores all lists/sets for each 'PublicKey' separately from each
-- other.
data WalletState m = WalletState
    { _getWSSecret            :: PublicKey -> m (Either Text SecretKey)
      -- ^ Retrieve a 'SecretKey' that was previously stored in
      -- Wallet.
    , _getWSTransactions      :: PublicKey -> m [Transaction]
      -- ^ Retrieve 'Transaction's related to the given 'PublicKey'.
    , _addWSTransaction       :: PublicKey -> Transaction -> m ()
      -- ^ Update 'Transaction's issued by given 'PublicKey'.
    , _getWSFinishedOffers    :: PublicKey -> m [MessagePointer JobOffer]
      -- ^ Retrieve Offers that were done by given 'PublicKey'.
    , _addWSFinishedOffer     :: PublicKey -> JobOffer -> m ()
      -- ^ Update Offers that were done by given 'PublicKey'.
    , _getWSCreatedOffers     :: PublicKey -> m [MessagePointer JobOffer]
      -- ^ Retrieve Offers that were created by given 'PublicKey'.
    , _addWSCreatedOffer      :: PublicKey -> JobOffer -> m ()
      -- ^ Update Offers that were created by given 'PublicKey'.
    , _getWSManagedSecretKeys :: m [SecretKey]
      -- ^ List all 'SecretKey's registered in this Wallet.
    , _initWS                 :: SecretKey -> m ()
      -- ^ Register new 'SecretKey' in wallet.
    }
makeCap ''WalletState

walletStateImpl :: CapImplIODB WalletState '[DB]
walletStateImpl = CapImpl WalletState
    { _getWSSecret = fmap (fmap getStorableSecretKey) . getDBCollection walletSecretsColl
    , _getWSTransactions = \pk -> map snd <$>
        listDBCollectionPrefix (CollectionPrefix @('S 'Z) walletTransactionsColl pk)
    , _addWSTransaction = curry (putDBCollection walletTransactionsColl)
    , _getWSFinishedOffers = \pk -> map (^._2) <$>
        listDBCollectionPrefix (CollectionPrefix @('S 'Z) walletFinishedOffersColl pk)
    , _addWSFinishedOffer = \pk offer -> putDBCollection walletFinishedOffersColl
        (pk, mkPointer pk offer, offer)
    , _getWSCreatedOffers = \pk -> map (^._2) <$>
        listDBCollectionPrefix (CollectionPrefix @('S 'Z) walletCreatedOffersColl pk)
    , _addWSCreatedOffer = \pk offer -> putDBCollection walletCreatedOffersColl
        (pk, mkPointer pk offer, offer)
    , _getWSManagedSecretKeys = do
            src <- iterDBCollection walletSecretsColl
            runConduitRes (src .| CL.map getStorableSecretKey .| consume)
    , _initWS = \sk -> whenLeftM (getDBCollection walletSecretsColl (toPublic sk)) $ \_ ->
            -- Check if given 'sk' already exists.  If exists, do nothing.
            putDBCollection walletSecretsColl (StorableSecretKey sk)
    }
