module Test.Ale.Core.Tokens where

import Universum hiding (empty)

import Hedgehog (Property, failure, forAll, property, success, (===))
import Test.HUnit (Assertion, (@?=))

import Ale.Core.Tokens as Tokens

import Test.Ale.Core.Entity (entity)
import Test.Ale.Core.Tokens.Gen (genTokenCount, genTokenKind, genTokens)
import Test.Json (jsoning)
import Test.Serialise (serialising)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-----------------------
-- TokenCount
-----------------------

hprop_TokenCountSerialise :: Property
hprop_TokenCountSerialise = property $ forAll genTokenCount >>= serialising

hprop_TokenCountJsonSerialise :: Property
hprop_TokenCountJsonSerialise = property $ forAll genTokenCount >>= jsoning


-----------------------
-- TokenKind
-----------------------

hprop_TokenKindSerialise :: Property
hprop_TokenKindSerialise = property $ forAll genTokenKind >>= serialising

hprop_TokenKindJsonSerialise :: Property
hprop_TokenKindJsonSerialise = property $ forAll genTokenKind >>= jsoning


-----------------------
-- Tokens
-----------------------

unit_entitiesOfEmpty :: Assertion
unit_entitiesOfEmpty = entities empty @?= []

unit_countOfSingleton :: Assertion
unit_countOfSingleton = countOf tk ts @?= 42
  where
    ts = fromList [(tk, 42)]
    tk = Transferable entity

hprop_addTokensNew :: Property
hprop_addTokensNew = property $ do
    e  <- forAll genTokenKind
    tc <- forAll genTokenCount
    ts <- forAll $ genTokens (Range.linear 0 50)
    guard $ countOf e ts == 0
    countOf e (addOne e tc ts) === tc

hprop_addTokensExisting :: Property
hprop_addTokensExisting = property $ do
    ts <- forAll $ genTokens (Range.linear 1 50)
    e  <- forAll $ Gen.element (entities ts)
    tc <- forAll genTokenCount
    countOf e (addOne e tc ts) === tc + countOf e ts

hprop_subOneExisting :: Property
hprop_subOneExisting = property $ do
    ts <- forAll $ genTokens $ Range.linear 1 50
    e  <- forAll $ Gen.element $ entities ts
    tc <- forAll genTokenCount
    -- sum may overflow
    let amount = max maxBound (tc + countOf e ts)
    Nothing === subOne e amount ts

hprop_subManyNotEnough :: Property
hprop_subManyNotEnough = property $ do
    ts <- forAll $ genTokens $ Range.linear 1 50
    e <- forAll $ Gen.element $ entities ts
    -- sum may overflow
    let toAdd = maxBound - countOf e ts
    guard $ toAdd /= 0
    let ts' = addOne e toAdd ts
    Left e === subMany ts' ts

hprop_TokensSerialise :: Property
hprop_TokensSerialise = property $ forAll (genTokens (Range.linear 0 50)) >>= serialising

hprop_TokensJsonSerialise :: Property
hprop_TokensJsonSerialise = property $ forAll (genTokens (Range.linear 0 50)) >>= jsoning

hprop_TokenCountNeverIsZero :: Property
hprop_TokenCountNeverIsZero = property $ do
    tokens <- forAll $ genTokens $ Range.linear 0 50
    let pairs = toPairs tokens
    zeroValues pairs === []

hprop_SutractionNeverMakesZeroTokenCount :: Property
hprop_SutractionNeverMakesZeroTokenCount = property $ do
    tokens <- forAll $ genTokens $ Range.linear 1 50
    if nonZeroValues (toPairs tokens) /= []
    then do
        ent <- forAll $ Gen.element (Tokens.entities tokens)
        let count = countOf ent tokens
        case subOne ent count tokens of
            Just tokens' -> do
                let pairs = toPairs tokens'
                zeroValues pairs === []
            Nothing ->
                failure
    else
        success

hprop_AdditionNeverMakesZeroTokenCount :: Property
hprop_AdditionNeverMakesZeroTokenCount = property $ do
    tokens <- forAll $ genTokens $ Range.linear 1 50
    ent    <- forAll   genTokenKind
    addOne ent 0 tokens === tokens

nonZeroValues :: [(TokenKind, TokenCount)] -> [(TokenKind, TokenCount)]
nonZeroValues = filter ((/= 0) . snd)

zeroValues :: [(TokenKind, TokenCount)] -> [(TokenKind, TokenCount)]
zeroValues = filter ((== 0) . snd)
