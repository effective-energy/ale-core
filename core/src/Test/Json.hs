module Test.Json
      ( jsoning
      ) where

import Universum

import Hedgehog (MonadTest, tripping)

import Data.Aeson (FromJSON, ToJSON (toEncoding, toJSON), decode, fromEncoding, fromJSON)
import Data.Binary.Builder (toLazyByteString)
import GHC.Stack (HasCallStack)


-- | Test that JSON serialisation and deserialisation are compatible.
jsoning :: (MonadTest m, ToJSON a, FromJSON a, Show a, Eq a, HasCallStack) => a -> m ()
jsoning val = do
    tripping val toJSON fromJSON
    tripping val (toLazyByteString . fromEncoding . toEncoding) decode
