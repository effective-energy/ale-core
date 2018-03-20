module Test.Ale.Core.Genesis.Block where

import Universum

import Data.Default (def)
import Hedgehog (Property, forAll, property, (===))

import Ale.Core.Block (unsafeGenesisPointer)
import Ale.Core.Genesis.Block (initialState)
import Ale.Core.Genesis.Data (GenesisData (..))
import Ale.Core.Genesis.Distribution (BalanceDistribution (..))
import Ale.Core.Storage (mkReference)
import Ale.State (asdAccounts)
import Ale.State.Pure (asBlock, asData, emptyAleState, runPureAleStateT)

import Test.Ale.Core.Crypto.Gen (genPk)
import Test.Ale.Core.Entity.Gen (genEntity)
import Test.Ale.Core.Tokens.Gen (genTokenBalance)

import qualified Ale.Data.HashMap as HM
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

hprop_initialState :: Property
hprop_initialState = property $ do
    let r = Range.linear 0 20
    distr <- forAll $ HM.fromList <$> Gen.list r ((,) <$> genEntity <*> genTokenBalance r)
    pk <- forAll genPk

    let genData = GenesisData (BalanceDistribution distr) pk def
    let genDataRef = mkReference genData

    ((), st) <- runPureAleStateT emptyAleState $ initialState genData

    distr === st ^. asData.asdAccounts
    unsafeGenesisPointer genDataRef === st ^. asBlock
