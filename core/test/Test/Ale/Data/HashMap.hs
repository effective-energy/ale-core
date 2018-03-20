module Test.Ale.Data.HashMap where

import Universum

import Test.HUnit (Assertion, assertBool)

import Data.Hashable (Hashable (..))

import qualified Data.HashMap.Strict as StdHM

-- | Some datatype with lots of controlled hash collisions.
data D = D Int String
    deriving (Eq, Show)

instance Hashable D where
    hashWithSalt salt (D x _) = hashWithSalt salt x

-- | This test is not guaranteed to pass all the time in the future.
-- It is left here as a reminder that standard HashMap has problems we
-- want to deal with.  In case this test fails our attention should be
-- brought back here.
unit_StandardHashMapIsNotDeterministic :: Assertion
unit_StandardHashMapIsNotDeterministic = assertBool
    "Standard HashMap became deterministic on this particular test"
    (buildMap [k1, k2] /= buildMap [k2, k1])
  where
    k1 = D 1 "lol"
    k2 = D 1 "kek"

    buildMap :: [D] -> [(D, D)]
    buildMap xs = StdHM.toList $ StdHM.fromList $ zip xs xs
