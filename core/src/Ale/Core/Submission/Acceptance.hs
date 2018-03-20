module Ale.Core.Submission.Acceptance
       ( AcceptanceTest(..)

       , facile
       , alwaysPass
       , alwaysFail

       , isFacile
       ) where

import Universum

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), genericParseJSON,
                   genericToEncoding, genericToJSON)
import Serokell.Aeson.Options (defaultOptions)


-- | A more advanced (but still stub) type for 'AcceptanceTest'.
data AcceptanceTest
    = ShouldAccept Bool  -- ^ Determines whether submission should be
                         -- accepted.
    deriving (Eq, Generic, Show)

instance Serialise AcceptanceTest

instance ToJSON AcceptanceTest where
    toJSON = genericToJSON defaultOptions
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AcceptanceTest where
    parseJSON = genericParseJSON defaultOptions

facile :: AcceptanceTest
facile = ShouldAccept True

-- | 'AcceptanceTest' that is not facile but is still always accepted.
alwaysPass :: AcceptanceTest
alwaysPass = ShouldAccept True

-- | Never-accepted submission.
alwaysFail :: AcceptanceTest
alwaysFail = ShouldAccept False

isFacile :: AcceptanceTest -> Bool
isFacile (ShouldAccept True) = True
isFacile _                   = False
