{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Ale.Core.DHT.Instances where

import Universum

import Hedgehog (GenT, PropertyT)

import Ale.Core.DHT.Types (MonadDHT (..), getDHTDefault, putDHTDefault)


instance (Monad m, MonadDHT m) => MonadDHT (PropertyT m) where
    getDHT = getDHTDefault
    putDHT = putDHTDefault

instance (Monad m, MonadDHT m) => MonadDHT (GenT m) where
    getDHT = getDHTDefault
    putDHT = putDHTDefault
