{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Missing crypto-related instances.

module Ale.Wallet.Crypto.Instances where

import Universum

import Crypto.Random (MonadRandom (..))
import Servant (Handler (..))

import Ale.Core.Crypto ()

deriving instance MonadRandom Handler
