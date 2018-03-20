-- | Container for signed data.
module Ale.Core.Crypto.Signed
       ( Signed (sPublicKey, sData)
       , mkSigned
       ) where

import           Ale.Core.Crypto.Signed.Internal
