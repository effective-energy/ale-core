{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

-- | Node configuration.
--
-- This module provides two types: 'PartialAleNodeConfig' and 'AleNodeConfig'.
-- Partial configs form a monoid where the binary operation prefers the value
-- that comes last. This allows you to compose multiple partial configs in such
-- a way that the more recent options take precedence.
-- After your partial config is assembled, you can use 'finalise', which will
-- make sure that all options that do not have default values were specified
-- and then it will return the final config.
module Ale.Node.Config
       ( AleNodeConfigP (..)

       , AleNodeConfig
       , PartialAleNodeConfig
       , finalise

       , loadNodeConfig
       ) where

import Universum

import Data.Aeson (withObject, (.:?))
import Data.Bifunctor (first)
import Data.Default (Default (def))
import Data.Yaml (FromJSON (..))
import Generics.Deriving.Monoid (gmemptydefault)
import Generics.Deriving.Semigroup (gsappenddefault)
import Network.Socket (HostName)

import qualified Data.Yaml as Y (decodeFileEither)

data Phase = Partial | Final

-- | Potentially incomplete configuration of a node.
data AleNodeConfigP (p :: Phase) = AleNodeConfig
    { ancHost    :: p :- HostName
    , ancPort    :: p :- Word16
    , ancTimeOut :: p :- Int
    , ancGenesis :: p :- FilePath
    , ancDBDir   :: p :- FilePath
    } deriving (Generic)

infixl 3 :-
type family phase :- field where
    'Partial :- field = Last field
    'Final   :- field = field

-- | Incomplete configuration of a node.
type PartialAleNodeConfig = AleNodeConfigP 'Partial
-- | Complete configuration of a node.
type AleNodeConfig = AleNodeConfigP 'Final

instance Semigroup PartialAleNodeConfig where
    (<>) = gsappenddefault

instance Monoid PartialAleNodeConfig where
    mempty = gmemptydefault
    mappend = (<>)

instance FromJSON PartialAleNodeConfig where
    parseJSON = withObject "ale node config" $ \o -> do
        ancHost    <- Last <$> o .:? "host"
        ancPort    <- Last <$> o .:? "port"
        ancTimeOut <- Last <$> o .:? "timeout"
        ancGenesis <- Last <$> o .:? "genesis"
        ancDBDir   <- Last <$> o .:? "db"
        pure AleNodeConfig{..}

instance Default PartialAleNodeConfig where
    def = mempty { ancPort    = Last (Just 12347)
                 , ancTimeOut = Last (Just 1000000)
                 , ancGenesis = Last Nothing
                 }

-- TODO: derive from Generic
-- | Make sure that all the required configurations options were specified.
finalise :: PartialAleNodeConfig -> Either Text AleNodeConfig
finalise AleNodeConfig{..} = AleNodeConfig <$> fin "host" ancHost <*> fin "port" ancPort
                                           <*> fin "timeout" ancTimeOut <*> fin "genesis" ancGenesis
                                           <*> fin "db" ancDBDir
  where
    fin name = maybe (Left $ "Missing field: " <> name) Right . getLast

-- | Read configuration from the given file and return it in data type.
loadNodeConfig :: MonadIO m => FilePath -> m (Either Text PartialAleNodeConfig)
loadNodeConfig = liftIO . fmap (first show) . Y.decodeFileEither
