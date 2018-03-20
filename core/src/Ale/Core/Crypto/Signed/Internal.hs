-- | Container for signed data internals.
module Ale.Core.Crypto.Signed.Internal
       ( Signed (..)
       , mkSigned
       ) where

import Universum

import Codec.Serialise (Serialise (decode, encode), serialise)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), genericParseJSON,
                   genericToEncoding, genericToJSON)
import Serokell.Aeson.Options (defaultOptions)

import Ale.Core.Crypto (PublicKey, SecretKey, Signature, signlazy, toPublic, verifylazy)
import Ale.Core.Storage (HasPrefixTag (..), StoragePrefix (..))
import Ale.Fmt (Buildable (..), (+|), (|+))

import qualified Data.ByteString.Lazy as BSL

-- | A /signed/ is a container for data that makes sure that the contained data
-- is signed by a public key.
--
-- There are exactly two ways to get a value of this type:
--
--   1. Use 'mkSigned' to sign some data with your secret key.
--
--   2. Use the 'Serialise' instance to deserialise data signed by someone else.
data Signed a = UnsafeSigned
    { sPublicKey :: !PublicKey
    , sSignature :: !Signature
    , sData      :: !a
    } deriving (Eq, Show, Generic)

-- | Sign some data with your secret key.
mkSigned :: Serialise a
         => SecretKey  -- ^ Signer secret key
         -> a          -- ^ Data to sign
         -> Signed a
mkSigned sk x = UnsafeSigned (toPublic sk) (signlazy sk bytes) x
  where
    bytes :: BSL.ByteString
    bytes = serialise x

-- | This instance makes sure that the data is properly signed when deserialising it.
instance Serialise a => Serialise (Signed a) where
    encode (UnsafeSigned pk sig x) = mconcat [encode pk, encode sig, encode x]
    decode = do
        pk  <- decode
        sig <- decode
        x   <- decode
        -- It is somewhat stupid that we have to serialise it back to bytes right after
        -- we deserialised it, but there doesn't seem to be an easier way.
        let bytes = serialise x :: BSL.ByteString
        unless (verifylazy pk bytes sig) $ fail "Invalid signature"
        pure $ UnsafeSigned pk sig x

instance HasPrefixTag a => HasPrefixTag (Signed a) where
    storagePrefix = StoragePrefix $ "signed:" <> getStoragePrefix (storagePrefix @a)

instance (Buildable a) => Buildable (Signed a) where
    build (UnsafeSigned pk _ dt) = "Signed by "+|pk|+" "+|dt|+""

instance ToJSON a => ToJSON (Signed a) where
    toJSON = genericToJSON defaultOptions
    toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Signed a) where
    parseJSON = genericParseJSON defaultOptions
