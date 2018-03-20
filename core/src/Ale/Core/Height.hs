-- | Measure of height of state and epoch of messages, calculated by blocks
module Ale.Core.Height
       ( Height (..)
       ) where

import Universum

import Codec.Serialise (Serialise (..))
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Default (Default (..))
import Data.Word (Word64)
import Serokell.Aeson.Options (defaultOptions)

import Ale.Fmt (Buildable (..))

newtype Height = Height Word64
    deriving (Eq, Ord, Generic, Enum, Num, Show, Hashable)

instance Default Height where
    def = Height (minBound :: Word64)

instance Serialise Height

instance Buildable Height where
    build (Height epoch) = build epoch

instance ToJSON Height where
    toJSON = genericToJSON defaultOptions
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Height where
    parseJSON = genericParseJSON defaultOptions
