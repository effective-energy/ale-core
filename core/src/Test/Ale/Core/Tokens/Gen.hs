module Test.Ale.Core.Tokens.Gen
       ( genTokens
       , genTokenBalance

       , genTokenCount
       , genTokenCountLe

       , genTokenKind
       ) where


import Universum

import Hedgehog (MonadGen)

import Ale.Core.Tokens (TokenBalance, TokenCount, TokenKind (..), Tokens, fromList, getTokenCount,
                        tokenCount)

import Test.Ale.Core.Entity.Gen (genEntity)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- | Generate 'TokenKind'.
genTokenKind :: MonadGen m => m TokenKind
genTokenKind = Gen.choice
    [ pure Money
    ,      Fuel         <$> genEntity
    ,      Transferable <$> genEntity
    ]

-- | Generate 'TokenCount'.
genTokenCount :: MonadGen m => m TokenCount
genTokenCount = tokenCount <$> Gen.word64 Range.constantBounded

-- | Generate 'TokenCount' which is less or equal to the given one.
genTokenCountLe :: MonadGen m => TokenCount -> m TokenCount
genTokenCountLe up = tokenCount <$> Gen.word64 (Range.constant 0 count)
  where
    count = getTokenCount up

-- | Generate 'Tokens' using a 'Range' to determine the number of issuing entities.
genTokens :: MonadGen m => Range.Range Int -> m Tokens
genTokens range = fromList <$> Gen.list range genPair
  where
    genPair = (,) <$> genTokenKind <*> genTokenCount

-- | Generate 'TokenBalance' using a 'Range' to determine the number of issuing entities.
genTokenBalance :: MonadGen m => Range.Range Int -> m TokenBalance
genTokenBalance = genTokens

