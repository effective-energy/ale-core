-- | Entities and operations on them
module Ale.Core.Entity
       ( Entity
       ) where

import           Ale.Core.Crypto (PublicKey)

-- | An /entity/ is someone participating in the Ale network,
-- e.g. a person or a company. Entities are identified by their public keys.
type Entity = PublicKey
