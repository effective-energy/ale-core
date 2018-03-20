-- | Capabilities helpers.
--
-- We use the @caps@ framework instead of monad stacks to manage monadic effects.
module Ale.Capabilities
       ( CapImplIO
       , CapImplIODB
       ) where

import Universum

import Control.Monad.Trans.Resource (MonadBaseControl)
import Monad.Capabilities (CapImpl)

-- | Implementation of a capability that works in arbitrary monad that
-- has an instance of 'MonadIO'.
type CapImplIO cap icaps = forall m. MonadIO m => CapImpl cap icaps m

-- | Like 'CapImplIO' but has additional instances that allow handling
-- exceptions.
type CapImplIODB cap icaps =
    forall m. (MonadIO m, MonadMask m, MonadBaseControl IO m) => CapImpl cap icaps m
