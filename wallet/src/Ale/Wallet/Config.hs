-- | This module provides an 'AleWalletConfig' that represents
-- configuration for ALE wallet.  Currently, it's quite empty and
-- doen't provide any useful options.

module Ale.Wallet.Config
       ( AleWalletConfig (..)
       , awcMOTD

       , loadWalletConfig
       ) where

import Universum

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Yaml (decodeFileEither)

-- | ALE Wallet config.  Currently, the only provided option 'awcMOTD'
-- specifies greeting message that is shown on wallet start.  More
-- useful options to come.
data AleWalletConfig = AleWalletConfig
    { _awcMOTD :: Text
    } deriving (Generic)

makeLenses ''AleWalletConfig

instance FromJSON AleWalletConfig where
    parseJSON = withObject "ale wallet config" $ \o -> do
        _awcMOTD <- o .: "motd"
        pure AleWalletConfig{..}

-- | Load config from file, throw exception if anything go wrong.
loadWalletConfig :: (MonadIO m, MonadThrow m) => FilePath -> m AleWalletConfig
loadWalletConfig path = do
    e <- liftIO $ decodeFileEither path
    case e of
        Left err  -> throwM err
        Right res -> pure res
