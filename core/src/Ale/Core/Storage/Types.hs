-- | This module provides the 'Storable' class for things that can be stored
-- in various databases as binary data.
module Ale.Core.Storage.Types
       ( PrefixTag
       , HasPrefixTag (..)
       , Storable
       , StoragePrefix (..)

       , Reference (unsafeUnpackReference)
       , mkReference
       , unsafeMkReferenceFromRaw
       , unsafeReferenceToBytes

       , unsafeReferenceToBase16
       , unsafeReferenceFromBase16
       ) where


import Universum

import Codec.Serialise (Serialise (..), serialise)
import Crypto.Hash (Blake2b_256, Digest, digestFromByteString, hash)
import Data.Aeson.Encoding (text)
import Data.Aeson.Types (FromJSON (..), FromJSONKey (..),
                         FromJSONKeyFunction (FromJSONKeyTextParser), ToJSON (..), ToJSONKey (..),
                         Value (String), toJSONKeyText, withText)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.Text (unpack)

import Ale.Core.Height (Height)
import Ale.Fmt (Buildable (build), (+||), (||+))
import Ale.Util.Digest (decodeDigest, encodeDigest)

import qualified Data.ByteString.Lazy as BSL
import qualified Serokell.Util.Base16 as SB16

-- | The class for things that can be stored in some database.  If
-- some datatype is expected to be stored in various storages (DHT and
-- Persistent Block Storage) then it's safe (and even convinient!) to
-- use the same prefix for different storages.
class Serialise a => HasPrefixTag a where
    -- | The prefix that will be added to the key when a value
    -- of a given type is stored to ensure type safety.
    storagePrefix :: StoragePrefix a

-- | A wrapper for datatypes to be stored in DHT.
class HasPrefixTag a => Storable a where

-- | Convenient prefix type alias.
type PrefixTag = ByteString

-- | A wrapper used to provide a storage prefix in the 'HasPrefixTag'
-- class that adds a necessary phantom type.
newtype StoragePrefix a = StoragePrefix { getStoragePrefix :: PrefixTag }
    deriving (Show)

instance HasPrefixTag ByteString where
    storagePrefix = StoragePrefix "bs"

instance Storable ByteString where

instance HasPrefixTag () where
    storagePrefix = StoragePrefix "unit"

instance HasPrefixTag BSL.ByteString where
    storagePrefix = StoragePrefix "bs"

instance HasPrefixTag Height where
    storagePrefix = StoragePrefix "asHeight"

instance HasPrefixTag a => HasPrefixTag [a] where
    storagePrefix = StoragePrefix $ "listOf" <> getStoragePrefix (storagePrefix @a)

instance Storable BSL.ByteString where

-- | A reference to a stored object.
newtype Reference a = Reference { unsafeUnpackReference :: Digest Blake2b_256 }
    deriving (Show, Eq, Ord)

instance Buildable (Reference a) where
    build (Reference h) = ""+||h||+""

instance Serialise (Reference a) where
    encode = encodeDigest . unsafeUnpackReference
    decode = Reference <$> decodeDigest

instance ToJSON (Reference a) where
    toJSON = String . unsafeReferenceToBase16
    toEncoding = text . unsafeReferenceToBase16

instance ToJSONKey (Reference a) where
    toJSONKey = toJSONKeyText unsafeReferenceToBase16

instance FromJSON (Reference a) where
    parseJSON = withText "Reference" unsafeReferenceFromBase16

instance FromJSONKey (Reference a) where
    fromJSONKey = FromJSONKeyTextParser unsafeReferenceFromBase16


-- | Constructor for 'Reference'.
mkReference :: Serialise a => a -> Reference a
mkReference = Reference . hash . BSL.toStrict . serialise

-- | Construct 'Reference' from already serialised value.  Don't use
-- unless you really understand what are you doing.
--
-- This function is needed because while doing distributed storage
-- business a client sends a Put prefix item message and the server
-- doesn't trust client so the key this item should be saved on is
-- calculated server-side. This function is used to calculate this
-- key.
unsafeMkReferenceFromRaw :: ByteString -> Reference a
unsafeMkReferenceFromRaw = Reference . hash

-- | Convert a reference to a raw 'ByteString'.
unsafeReferenceToBytes :: Reference a -> ByteString
unsafeReferenceToBytes = convert . unsafeUnpackReference

-- | Try to convert a 'ByteString' to a reference.
unsafeReferenceFromBytes :: MonadFail m => ByteString -> m (Reference a)
unsafeReferenceFromBytes bytes = case digestFromByteString bytes of
    Just k  -> pure $ Reference k
    Nothing -> fail "Invalid digest vaue"

-- | Convert a reference to base16 encoded 'Text'.
unsafeReferenceToBase16 :: Reference a -> Text
unsafeReferenceToBase16 = SB16.encode . unsafeReferenceToBytes

-- | Try to read a base16 encoded reference.
unsafeReferenceFromBase16 :: MonadFail m => Text -> m (Reference a)
unsafeReferenceFromBase16 txt = case SB16.decode txt of
    Left e   -> fail $ unpack e
    Right bs -> unsafeReferenceFromBytes bs
