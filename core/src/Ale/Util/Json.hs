-- | Utilities for working with JSON values.
module Ale.Util.Json
       ( bytesToJson16
       , bytesFromJson16

       , bytesToJson64
       , bytesFromJson64
       ) where

import Universum

import Data.Aeson (Value (String), withText)
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)

import qualified Serokell.Util.Base16 as SB16
import qualified Serokell.Util.Base64 as SB64


-- | Encode a 'ByteString' as a JSON 'Value'.
bytesToJson16 :: ByteString -> Value
bytesToJson16 = String . SB16.encode

-- | Try to decode a 'ByteString' from a JSON 'Value'.
bytesFromJson16 :: Value -> Parser ByteString
bytesFromJson16 = withText "base16-encoded string" $ \t ->
    case SB16.decode t of
        Left  e  -> fail $ show e
        Right bs -> pure bs


-- | Encode a 'ByteString' as a JSON 'Value'.
bytesToJson64 :: ByteString -> Value
bytesToJson64 = String . SB64.encode

-- | Try to decode a 'ByteString' from a JSON 'Value'.
bytesFromJson64 :: Value -> Parser ByteString
bytesFromJson64 = withText "base64-encoded string" $ \t ->
    case SB64.decode t of
        Left  e  -> fail $ show e
        Right bs -> pure bs
