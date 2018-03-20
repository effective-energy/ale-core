-- | Internal helpers for specifying Requirements.
module Ale.Core.Requirements.Internal
       ( ReqByteString (..)
       , Constant (..)
       , Requirements (..)
       ) where

import Universum

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), genericParseJSON,
                   genericToEncoding, genericToJSON)
import Data.Aeson.Types (Options (..), SumEncoding (ObjectWithSingleField), defaultOptions)
import Data.ByteArray (ByteArray, ByteArrayAccess)
import Data.ByteString (ByteString)

import Ale.Core.Crypto (PublicKey, Signature)
import Ale.Util.Json (bytesFromJson64, bytesToJson64)

-- | An untyped constant used in 'Requirements'.  It was once called
-- @Challenge@ because the only place where 'Requirements' needed a
-- 'ByteString' was a challenge bytes one needed to sign to verify
-- they have particular 'PrivateKey' access.  Now 'ByteString's are
-- also used in hashing and it has little sense to use different types
-- here and there.
newtype ReqByteString = ReqByteString ByteString
    deriving (Eq, Ord, Monoid, Generic, Show, IsString, ByteArrayAccess, ByteArray)

instance ToJSON ReqByteString where
    toJSON (ReqByteString bs) = toJSON $ bytesToJson64 bs
    toEncoding (ReqByteString bs) = toEncoding $ bytesToJson64 bs

instance FromJSON ReqByteString where
    parseJSON v = ReqByteString <$> (parseJSON v >>= bytesFromJson64)

instance Serialise ReqByteString

data Constant
    = RInt !Int
    | RString !Text
    | RBool !Bool
    | RByteString !ReqByteString
    | RSignature !Signature
    | RPublicKey !PublicKey
    deriving (Generic, Show)

-- | Untyped 'Reqs' for JSON marshalling.
data Requirements
    = RConst Constant
    | RVar Int
    | RAnd [Requirements]
    | RLess Requirements Requirements
    | RVerify Requirements Requirements Requirements
    | RSha256 Requirements
    | REqBs Requirements Requirements
    deriving (Generic, Show)

instance ToJSON Constant where
    toJSON = genericToJSON options
    toEncoding = genericToEncoding options

instance FromJSON Constant where
    parseJSON = genericParseJSON options

instance ToJSON Requirements where
    toJSON = genericToJSON options
    toEncoding = genericToEncoding options

instance FromJSON Requirements where
    parseJSON = genericParseJSON options


----
--
----

options :: Options
options = defaultOptions
    { sumEncoding = ObjectWithSingleField
    }
