{-# LANGUAGE DeriveAnyClass #-}

-- | Wallet-specific data types.
module Ale.Wallet.Types
       ( WalletInfo (..)
       , WalletInfoSecret (..)
       , Transaction (..)
       ) where

import Universum

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime)

import Ale.Core.Crypto (SecretKey(..))
import Ale.Core.Entity (Entity)
import Ale.Core.Storage.Types (HasPrefixTag (..), StoragePrefix (..))
import Ale.Core.Tokens (TokenCount)
import Ale.Fmt (Buildable (..), (+|), (|+))
import Ale.Node.DB.Types (Persistable)


-- | Information about a wallet managed by backend.
data WalletInfo = WalletInfo
    { wiPublicKey :: !Entity
    } deriving (Generic, Show)

instance ToJSON WalletInfo

instance FromJSON WalletInfo


-- | Information (also secret) about a wallet managed by backend.
data WalletInfoSecret = WalletInfoSecret
    { wisPublicKey :: !Entity
    , wisSecretKey :: !SecretKey
    } deriving (Generic, Show)

instance ToJSON WalletInfoSecret

instance FromJSON WalletInfoSecret


-- | Information about a token transaction.
data Transaction = Transaction
    { tSender    :: !Entity
    , tReceiver  :: !Entity
    , tAmount    :: !TokenCount
    , tTimestamp :: !UTCTime
    } deriving (Generic, Show, Serialise)

instance ToJSON Transaction

instance FromJSON Transaction

instance Buildable Transaction where
    build Transaction{..} =
        "Send "+|tAmount|+" from "+|tSender|+" to "+|tReceiver|+" at "+|tTimestamp|+""

instance HasPrefixTag Transaction where
    storagePrefix = StoragePrefix "transaction"

instance Persistable Transaction
