-- | Paths used by the tools.
module Ale.Tools.Paths
       ( getDataDir

       , genesisFile
       , genesisHashFile
       , serverKeyFile
       , serverSecretKeyFile
       , keychainFile
       , nodeConfigFile
       , walletConfigFile
       , nodeDBDir
       ) where

import Universum

import System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import System.FilePath ((</>))
import System.IO (FilePath)


-- | Path to the directory used by Ale to store data.
getDataDir :: MonadIO m => m FilePath
getDataDir = liftIO $ getXdgDirectory XdgData dataDirName
  where
    dataDirName :: String
    dataDirName = "ale"

-- | Path to the file with Genesis data.
genesisFile :: FilePath  -- ^ Data directory
            -> FilePath
genesisFile = (</> "genesis.json")

genesisHashFile :: FilePath -> FilePath
genesisHashFile = (</> "genesis.hash")

-- | Path to the server public key.
serverKeyFile :: FilePath  -- ^ Data directory
              -> FilePath
serverKeyFile = (</> "server.key.pub")

-- | Path to the server secret key.
serverSecretKeyFile :: FilePath  -- ^ Data directory
                    -> FilePath
serverSecretKeyFile = (</> "server.key")

-- | Path to the default keychain.
keychainFile :: FilePath  -- ^ Data directory
             -> FilePath
keychainFile = (</> "keychain.json")

-- | Path to the default node config.
nodeConfigFile :: FilePath
nodeConfigFile = "node/node-config.yaml"

-- | Path to the default wallet config.
walletConfigFile :: FilePath
walletConfigFile = "wallet/wallet-config.yaml"

nodeDBDir :: FilePath -> FilePath
nodeDBDir = (</> "db" </> "node")
