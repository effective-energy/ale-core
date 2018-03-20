module Test.Ale.Core.Entity.Gen
       ( genEntity
       ) where

import Hedgehog (MonadGen)

import Ale.Core.Entity (Entity)

import Test.Ale.Core.Crypto.Gen (genPk)


-- | Generate a random 'Entity'.
genEntity :: MonadGen m => m Entity
genEntity = genPk
