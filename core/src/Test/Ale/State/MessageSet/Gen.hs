module Test.Ale.State.MessageSet.Gen
       ( genMessageSet
       ) where

import Universum

import Hedgehog (MonadGen, Range)

import Codec.Serialise (Serialise)

import Ale.State.MessageSet (MessageSet)

import Test.Ale.Core.Entity.Gen (genEntity)

import qualified Ale.State.MessageSet as MS
import qualified Hedgehog.Gen as Gen


{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Generate 'MessageSet' with given size.
genMessageSet :: (MonadGen m, Serialise a) => Range Int -> m a -> m (MessageSet a)
genMessageSet range gen = do
    items <- Gen.list range $ (,) <$> genEntity <*> gen
    pure $ foldr (uncurry MS.insert) MS.empty items
