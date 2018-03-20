{-# LANGUAGE RankNTypes #-}

-- | The basic structure of an Ale block.
module Ale.Core.Block
       ( UnsignedBlock (UnsignedBlock)
       , ubParent
       , ubBundle

       , Block (Block)
       , bParent
       , bBundle
       , bSignature

       , signBlock
       , verifyBlock

       , BlockPointer ()
       , mkPointer
       , unsafeGenesisPointer

       , Bundle (..)
       ) where

import Universum hiding (toText)

import Codec.Serialise (Serialise (..), serialise)
import Control.Lens (makeLenses)
import Crypto.Hash (Blake2b_256, Digest, digestFromByteString, hash)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.ByteArray (convert)
import Data.Hashable (Hashable (hashWithSalt))

import Ale.Core.Crypto (PublicKey, SecretKey, Signature, signlazy, verifylazy)
import Ale.Core.Crypto.Signed (Signed)
import Ale.Core.Message (Message)
import Ale.Core.Storage (HasPrefixTag (..), Reference (unsafeUnpackReference), Storable,
                         StoragePrefix (..))
import Ale.Fmt (Buildable (..), Builder, (+|), (|+))
import Ale.Util.Digest (decodeDigest, encodeDigest)

import qualified Data.ByteString.Base64 as Base64 (decode, encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)

-- | 'Block' candidate to be signed by server.
data UnsignedBlock = UnsignedBlock
    { _ubParent :: BlockPointer
    , _ubBundle :: Reference Bundle
    } deriving (Generic)

instance Serialise UnsignedBlock where

-- | Block of data stored in the blockchain.
data Block = Block
     { _bParent    :: BlockPointer
     , _bBundle    :: Reference Bundle
     , _bSignature :: Signature
     }
    deriving (Eq, Show, Generic)

-- | A pointer to a block stored previously in the blockchain.
newtype BlockPointer = BlockPointer (Digest Blake2b_256)
    deriving (Show, Eq, Ord)

-- | A list of signed messages referred from a block.
newtype Bundle = Bundle [Signed Message]
    deriving (Generic)

-- Template Haskell
makeLenses ''UnsignedBlock
makeLenses ''Block

instance Serialise Block where

instance Buildable Block where
    build b = "block#"+|blockPtrF (mkPointer b)|+""

instance HasPrefixTag Block where
    storagePrefix = StoragePrefix "block"

instance Serialise BlockPointer where
    encode (BlockPointer h) = encodeDigest h
    decode = BlockPointer <$> decodeDigest

instance Hashable BlockPointer where
    hashWithSalt salt (BlockPointer h) = hashWithSalt salt (convert h :: ByteString)

instance HasPrefixTag BlockPointer where
    storagePrefix = StoragePrefix "blockPointer"

instance Buildable BlockPointer where
    build = blockPtrF

instance FromJSON BlockPointer where
    parseJSON = \case
        String text ->
            maybe empty return (fromText text)
        _ ->
            empty

instance ToJSON BlockPointer where
    toJSON = String . Ale.Core.Block.toText

instance Serialise Bundle where

instance HasPrefixTag Bundle where
    storagePrefix = StoragePrefix "bundle"

instance Storable Bundle where

-- | Smart ctor for conversion to/from 'Text' required by @Servant@.
fromText :: Text -> Maybe BlockPointer
fromText text =
    let bstr = Text.encodeUtf8 text
    in case Base64.decode bstr of
        Left _ ->
            Nothing
        Right digestBstr ->
            BlockPointer <$> digestFromByteString digestBstr

-- | Conversion to 'Text' required by @Servant@.
toText :: BlockPointer -> Text
toText (BlockPointer digest) =
    let bstr = convert digest
        b64  = Base64.encode bstr
    in  Text.decodeUtf8 b64

-- | 'BlockPointer' smart constructor.
mkPointer :: Block -> BlockPointer
mkPointer = BlockPointer . hash . BSL.toStrict . serialise

-- | This function should be used /only/ for genesis block generation.
unsafeGenesisPointer :: Reference a -> BlockPointer
unsafeGenesisPointer = BlockPointer . unsafeUnpackReference

-- | Format 'BlockPointer' as hex.
blockPtrF :: BlockPointer -> Builder
blockPtrF (BlockPointer h) = show h

-- | Construct 'Block' from 'UnsignedBlock'.
signBlock :: SecretKey -> UnsignedBlock -> Block
signBlock sk ub = Block
    { _bParent = ub^.ubParent
    , _bBundle = ub^.ubBundle
    , _bSignature = signlazy sk (serialise ub)
    }

-- | Check 'Block' signature against server 'PublicKey'.
verifyBlock :: PublicKey -> Block -> Bool
verifyBlock pk b = verifylazy
    pk
    (serialise $ UnsignedBlock
        { _ubParent = b^.bParent
        , _ubBundle = b^.bBundle
        })
    (b^.bSignature)


