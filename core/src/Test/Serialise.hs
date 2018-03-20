module Test.Serialise
      ( serialising
      ) where

import Universum

import Hedgehog (MonadTest, tripping)

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import GHC.Stack (HasCallStack)


-- | Test that serialisation and deserialisation are compatible.
serialising :: (MonadTest m, Serialise a, Show a, Eq a, HasCallStack) => a -> m ()
serialising val = tripping val serialise deserialiseOrNothing
  where
    deserialiseOrNothing x = either (const Nothing) Just $ deserialiseOrFail x
