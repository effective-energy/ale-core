-- | This module contains some functions useful for testing.
module Test.Util
       ( evalMaybe
       ) where

import Universum

import Hedgehog (MonadTest, failure)

-- | Similar to 'evalEither' but for 'Maybe'.  Fails the test if
-- 'Nothing' provided, unpacks 'Maybe' otherwise.
evalMaybe :: MonadTest m => Maybe a -> m a
evalMaybe ma = case ma of
    Nothing -> failure
    Just a  -> pure a
