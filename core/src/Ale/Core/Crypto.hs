{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module exports cryptographic primitives used throughout the system.
module Ale.Core.Crypto
       ( PublicKey
       , publicSize
       , publicToBytes
       , publicFromBytes
       , publicToBase64
       , publicFromBase64
       , pkBase64F

       , SecretKey
       , secretSize
       , generateSecretKey
       , secretToBytes
       , secretFromBytes
       , toPublic
       , secretToBase64
       , secretFromBase64
       , skBase64F

       , Signature
       , signatureSize
       , signatureToBytes
       , signatureFromBytes
       , signatureToBase64
       , signatureFromBase64
       , sign
       , signlazy
       , verify
       , verifylazy
       ) where

import Prelude (Show (show))
import Universum hiding (show)

import Codec.Serialise (Serialise (..))
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (Blake2b_256, Digest, hash, hashlazy)
import Crypto.Random (MonadRandom (..))
import Data.Aeson.Encoding (text)
import Data.Aeson.Types (FromJSON (..), FromJSONKey (..),
                         FromJSONKeyFunction (FromJSONKeyTextParser), ToJSON (..), ToJSONKey (..),
                         Value (String), toJSONKeyText, withText)
import Data.ByteArray (ByteArrayAccess, convert)
import Data.Hashable (Hashable)
import Data.Text (Text, unpack)
import Data.Text.Buildable (Buildable (..))
import Data.Text.Lazy.Builder (Builder, fromText)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

import Ale.Core.Storage (HasPrefixTag (..), StoragePrefix (..))
import Ale.Fmt ((+|), (|+))

import qualified Crypto.PubKey.Ed25519 as PK
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Hashable as Hashable
import qualified Serokell.Util.Base64 as SB64

-- | Ale public key
newtype PublicKey = PublicKey { getPublicKey :: PK.PublicKey }
    deriving (Eq, Generic, Show)

instance Ord PublicKey where
    compare = compare `on` publicToBytes

instance Hashable PublicKey where
    hashWithSalt s = Hashable.hashWithSalt s . publicToBytes

instance Buildable PublicKey where
    build pk = "pub:"+|pkBase64F pk|+""

instance Serialise PublicKey where
    encode = encode . publicToBytes
    decode = decode >>= publicFromBytes

instance HasPrefixTag PublicKey where
    storagePrefix = StoragePrefix "publicKey"

instance ToJSON PublicKey where
    toJSON = String . publicToBase64
    toEncoding = text . publicToBase64

instance ToJSONKey PublicKey where
    toJSONKey = toJSONKeyText publicToBase64

instance FromJSON PublicKey where
    parseJSON = withText "PublicKey" publicFromBase64

instance FromJSONKey PublicKey where
    fromJSONKey = FromJSONKeyTextParser publicFromBase64

instance ToHttpApiData PublicKey where
    toUrlPiece = publicToBase64

instance FromHttpApiData PublicKey where
    parseUrlPiece = maybeToRight "Bad PublicKey" . publicFromBase64

-- | Size of 'PublicKey' in bytes
publicSize :: Int
publicSize = PK.publicKeySize

-- | Convert a public key to a raw 'ByteString'
publicToBytes :: PublicKey -> BS.ByteString
publicToBytes = convert . getPublicKey

-- | Try to convert a 'ByteString' to a public key
publicFromBytes :: MonadFail m => BS.ByteString -> m PublicKey
publicFromBytes bytes = case PK.publicKey bytes of
    CryptoPassed k -> pure $ PublicKey k
    CryptoFailed e -> fail $ show e

-- | Convert a public key to base64 encoded 'Text'
publicToBase64 :: PublicKey -> Text
publicToBase64 = SB64.encode . publicToBytes

-- | Try to read a base64 encoded public key
publicFromBase64 :: MonadFail m => Text -> m PublicKey
publicFromBase64 txt = case SB64.decode txt of
    Left e   -> fail $ unpack e
    Right bs -> publicFromBytes bs

-- | Format 'PublicKey' as base64
pkBase64F :: PublicKey -> Builder
pkBase64F = fromText . publicToBase64


-- | Ale secret key
newtype SecretKey = SecretKey { getSecretKey :: PK.SecretKey }
    deriving (Eq, Generic)

instance Show SecretKey where
    show _ = "<SecretKey>"

-- | Size of 'SecretKey' in bytes
secretSize :: Int
secretSize = PK.secretKeySize

instance Buildable SecretKey where
    build sk = "sec:"+|pkBase64F (toPublic sk)+|""

{-
instance ToJSON SecretKey where
    toJSON = error "Do not write a secret key to a file!*"
    -- * Unless you know what you are doing.
    --   If you know what you are doing, use 'secretToBase64' explicitly.
    --   Sorry!
-}
instance ToJSON SecretKey where
    toJSON = String . secretToBase64
    toEncoding = text . secretToBase64

instance FromJSON SecretKey where
    parseJSON = withText "SecretKey" secretFromBase64

{-
instance ToHttpApiData SecretKey where
    toUrlPiece = error "Do not do this!"
    -- toUrlPiece = secretToBase64
-}

instance FromHttpApiData SecretKey where
    parseUrlPiece = maybeToRight "Bad SecretKey" . secretFromBase64

-- | Generate a secret key
generateSecretKey :: MonadRandom m => m SecretKey
generateSecretKey = SecretKey <$> PK.generateSecretKey

-- | Convert a secret key to a raw 'ByteString'
secretToBytes :: SecretKey -> BS.ByteString
secretToBytes = convert . getSecretKey

secretFromBytes :: MonadFail m => BS.ByteString -> m SecretKey
secretFromBytes bytes = case PK.secretKey bytes of
    CryptoPassed k -> pure $ SecretKey k
    CryptoFailed e -> fail $ show e

-- | Convert a secret key to base64 encoded 'Text'
secretToBase64 :: SecretKey -> Text
secretToBase64 = SB64.encode . secretToBytes

-- | Try to read a base64 encoded public key
secretFromBase64 :: MonadFail m => Text -> m SecretKey
secretFromBase64 txt = case SB64.decode txt of
    Left e   -> fail $ unpack e
    Right bs -> secretFromBytes bs

-- | Format 'SecretKey' as base64.
-- MAKE SURE YOU KNOW WHAT YOU ARE DOING!
skBase64F :: SecretKey -> Builder
skBase64F = fromText . secretToBase64

-- | Derive a public key from a secret key
toPublic :: SecretKey -> PublicKey
toPublic (SecretKey sk) = PublicKey $ PK.toPublic sk


-- | Ale signature
newtype Signature = Signature { getSignature :: PK.Signature }
    deriving (Eq, Generic, Show)

instance Serialise Signature where
    encode = encode . signatureToBytes
    decode = decode >>= signatureFromBytes

instance Buildable Signature where
    build pkSig = "sig:"+|signatureBase64 pkSig|+""

instance ToJSON Signature where
    toJSON = String . signatureToBase64
    toEncoding = text . signatureToBase64

instance FromJSON Signature where
    parseJSON = withText "Signature" signatureFromBase64

signatureBase64 :: Signature -> Text
signatureBase64 = SB64.encode . signatureToBytes

-- | Size of 'Signature' in bytes
signatureSize :: Int
signatureSize = PK.signatureSize

-- | Convert a signature to a raw 'ByteString'
signatureToBytes :: Signature -> BS.ByteString
signatureToBytes = convert . getSignature

-- | Try to convert a 'ByteString' to a signature
signatureFromBytes :: MonadFail m => BS.ByteString -> m Signature
signatureFromBytes bytes = case PK.signature bytes of
    CryptoPassed s -> pure $ Signature s
    CryptoFailed e -> fail $ show e

-- | Convert a signature to base64 encoded 'Text'
signatureToBase64 :: Signature -> Text
signatureToBase64 = SB64.encode . signatureToBytes

-- | Try to read a base64 encoded signature
signatureFromBase64 :: MonadFail m => Text -> m Signature
signatureFromBase64 txt = case SB64.decode txt of
    Left e   -> fail $ unpack e
    Right bs -> signatureFromBytes bs

-- | Sign data using a secret key
sign :: ByteArrayAccess a => SecretKey -> a -> Signature
sign (SecretKey sk) bytes = Signature $ PK.sign sk (PK.toPublic sk) bytesHash
  where
    -- There is actually no need for hashing, but we hash anyway to make sure that
    -- 'sign' and 'signlazy' produce equal signatures.
    bytesHash = hash bytes :: Digest Blake2b_256

-- | Sign a lazy 'ByteString' using a secret key
signlazy :: SecretKey -> BSL.ByteString -> Signature
signlazy (SecretKey sk) bytes = Signature $ PK.sign sk (PK.toPublic sk) bytesHash
  where
    bytesHash = hashlazy bytes :: Digest Blake2b_256

-- | Verify a signature given a public key
verify :: ByteArrayAccess a => PublicKey -> a -> Signature -> Bool
verify (PublicKey pk) bytes (Signature sig) = PK.verify pk bytesHash sig
  where
    bytesHash = hash bytes :: Digest Blake2b_256

-- | Verify a signature for a lazy 'ByteString'
verifylazy :: PublicKey -> BSL.ByteString -> Signature -> Bool
verifylazy (PublicKey pk) bytes (Signature sig) = PK.verify pk bytesHash sig
  where
    bytesHash = hashlazy bytes :: Digest Blake2b_256

instance MonadRandom m => MonadRandom (ReaderT r m) where
    getRandomBytes = lift . getRandomBytes

instance MonadRandom m => MonadRandom (ExceptT e m) where
    getRandomBytes = lift . getRandomBytes
