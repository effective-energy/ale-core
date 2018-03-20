module Test.Ale.Core.DHT
       ( putGet
       , getBeforePut
       , putGetMany
       ) where

import Universum

import Hedgehog (MonadTest, assert, evalEither, (===))

import Data.Either (isLeft)

import Ale.Core.DHT.Types (MonadDHT (..))
import Ale.Core.Storage.Types (Storable, mkReference)

import Test.Ale.Core.DHT.Instances ()

putGet :: (Show a, Eq a, Storable a, MonadTest m, MonadDHT m) => a -> m ()
putGet item = do
    ref <- putDHT item
    item' <- join $ evalEither <$> getDHT ref
    item === item'

getBeforePut :: (Show a, Eq a, Storable a, MonadTest m, MonadDHT m) => a -> m ()
getBeforePut item = do
    res <- getDHT $ mkReference item
    assert $ isLeft res

putGetMany :: (Show a, Eq a, Storable a, MonadTest m, MonadDHT m) => [a] -> m ()
putGetMany items = do
    refs <- forM items putDHT
    forM_ (zip items refs) $ \(item, ref) -> do
        item' <- join $ evalEither <$> getDHT ref
        item === item'

