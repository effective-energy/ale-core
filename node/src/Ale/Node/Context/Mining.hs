{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Node mining context.
module Ale.Node.Context.Mining
      ( Mining (..)
      , MonadMining (..)
      ) where

import Monad.Capabilities (makeCap)

import Ale.Core.Crypto.Signed (Signed)
import Ale.Core.Message (Message)


-- | Capability required to support mining.
data Mining m = Mining
    { _mine :: Signed Message -> m ()
    }
makeCap ''Mining
