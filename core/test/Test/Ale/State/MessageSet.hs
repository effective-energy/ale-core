module Test.Ale.State.MessageSet where

import Universum

import Hedgehog (Property, forAll, property, (===))

import Ale.Core.Message (mkPointer)

import Test.Ale.Core.Entity.Gen (genEntity)
import Test.Ale.Core.Message.Gen (genJobOffer)
import Test.Ale.State.MessageSet.Gen (genMessageSet)

import qualified Ale.State.MessageSet as MS
import qualified Hedgehog.Range as Range


{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

----------------------------------------
--- MessageSet
----------------------------------------

hprop_InsertLookup :: Property
hprop_InsertLookup = property $ do
    msgSet <- forAll $ genMessageSet (Range.linear 0 5) genJobOffer
    ent <- forAll genEntity
    jo <- forAll genJobOffer
    Just (ent, jo) === MS.lookup (mkPointer ent jo) (MS.insert ent jo msgSet)

hprop_DeleteDeletes :: Property
hprop_DeleteDeletes = property $ do
    msgSet <- forAll $ genMessageSet (Range.linear 0 5) genJobOffer
    ent <- forAll genEntity
    jo <- forAll genJobOffer
    let ptr = mkPointer ent jo
    Nothing === MS.lookup ptr (MS.delete ptr (MS.insert ent jo msgSet))

hprop_EmptyIsEmpty :: Property
hprop_EmptyIsEmpty = property $ do
    ent <- forAll genEntity
    jo <- forAll genJobOffer
    Nothing === MS.lookup (mkPointer ent jo) MS.empty
