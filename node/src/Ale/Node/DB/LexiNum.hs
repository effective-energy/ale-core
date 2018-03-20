-- | Serialisation of numbers in lexicographic order.
module Ale.Node.DB.LexiNum
      ( LexiInt (..)
      ) where

import Universum

import Codec.Serialise (Serialise (decode, encode))
import Codec.Serialise.Decoding (Decoder)
import Data.ByteString (ByteString)
import Data.Char (chr, ord)

import qualified Data.ByteString.Char8 as C8


-- | This wrapper has a special 'Serialise' instance which guarantees that
-- if @a <= b@ then @serialise a <= serialise b@.
newtype LexiInt = LexiInt { unLexiInt :: Int }
    deriving (Eq, Show, Ord, Num)

instance Serialise LexiInt where
    -- Here we rely on the fact that converting a 'ByteString' into 'Encoding'
    -- is monotonic. Hopefully, it's true.
    encode = encode . encodeBs
    decode = decode >>= decodeBs

-- | We simply encode the int as a decimal number, prefixing it with a marker
-- which indicates the length of the decimal representation.
encodeBs :: LexiInt -> ByteString
encodeBs (LexiInt n) = marker `C8.cons` bs
  where
    bs = show n
    len = length bs
    marker = if n >= 0
             then chr $ ord 'a' - 1 + len
             else error "Lexi encoding does not work for negative numbers"

-- | Strip the marker and decode the decimal number.
decodeBs :: ByteString -> Decoder s LexiInt
decodeBs bs = case C8.uncons bs of
    Nothing       -> fail "No marker"
    Just (_, bs') -> case C8.readInt bs' of
        Just (n, "") -> pure $ LexiInt n
        _            -> fail "Not a number"
