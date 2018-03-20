-- | Helpers for working with digests
module Ale.Util.Digest
       ( encodeDigest
       , decodeDigest
       ) where

import           Universum

import           Codec.Serialise          (Serialise (..))
import           Codec.Serialise.Decoding (Decoder)
import           Codec.Serialise.Encoding (Encoding)
import           Crypto.Hash              (Digest, HashAlgorithm, digestFromByteString)
import           Data.ByteArray           (convert)
import           Data.ByteString          (ByteString)

encodeDigest :: HashAlgorithm a => Digest a -> Encoding
encodeDigest = encode @ByteString . convert

decodeDigest :: HashAlgorithm a => Decoder s (Digest a)
decodeDigest = do
    bytes <- decode
    case digestFromByteString (bytes :: ByteString) of
        Just h  -> pure h
        Nothing -> fail "Invalid envelope hash"
