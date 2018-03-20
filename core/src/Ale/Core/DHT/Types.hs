{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides (not yet) Distributed Hash Table interface.
-- The general idea is that DHT is a structure that stores objects and
-- provides a way to retrieve the object if it's stored and we know
-- its hash.
module Ale.Core.DHT.Types
       ( DHT (..)
       , MonadDHT (..)

       , getDHTDefault
       , putDHTDefault
       ) where

import Universum

import Monad.Capabilities (makeCap)
import System.Wlog (LoggerNameBox, NamedPureLogger)

import Ale.Core.Storage.Types (Reference, Storable)


data DHT m = DHT
    { _getDHT :: forall a. Storable a => Reference a -> m (Either Text a)
    , _putDHT :: forall a. Storable a => a -> m (Reference a)
    }
makeCap ''DHT


getDHTDefault :: (MonadTrans t, t n ~ m, Monad n, MonadDHT n, Storable item)
                => Reference item
                -> m (Either Text item)
getDHTDefault = lift . getDHT

putDHTDefault :: (MonadTrans t, t n ~ m, Monad n, MonadDHT n, Storable item)
                => item
                -> m (Reference item)
putDHTDefault = lift . putDHT


instance (Monad m, MonadDHT m) => MonadDHT (ExceptT e m) where
    getDHT = getDHTDefault
    putDHT = putDHTDefault

instance (Monad m, MonadDHT m) => MonadDHT (NamedPureLogger m) where
    getDHT = getDHTDefault
    putDHT = putDHTDefault

instance (Monad m, MonadDHT m) => MonadDHT (LoggerNameBox m) where
    getDHT = getDHTDefault
    putDHT = putDHTDefault

instance (Monad m, MonadDHT m) => MonadDHT (StateT s m) where
    getDHT = getDHTDefault
    putDHT = putDHTDefault
