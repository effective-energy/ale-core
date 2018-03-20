-- | Common command line parsers.
module Ale.Tools.Options
       ( genesisOption
       , keychainOption
       , serverSecretKeyOption
       , nodeConfigOption
       , walletConfigOption
       , restPortArgument
       ) where

import Universum

import Options.Applicative (Parser, argument, auto, help, long, metavar, short, showDefault,
                            strOption, value)

import Ale.Tools.Paths (genesisFile, keychainFile, nodeConfigFile, serverSecretKeyFile,
                        walletConfigFile)

-- | Parser that gets a path to the keychain.
keychainOption :: FilePath  -- ^ Data directory
               -> Parser FilePath
keychainOption dataDir = strOption
    ( long "keychain" <> short 'c'
   <> metavar "KEYCHAIN_PATH"
   <> value kcPath
   <> showDefault
   <> help "Path to the file with keychain."
    )
  where
    kcPath = keychainFile dataDir

-- | Parser that gets a path to server key.
serverSecretKeyOption :: FilePath -> Parser FilePath
serverSecretKeyOption dataDir = strOption
    ( long "server-sk" <> short 's'
   <> metavar "SERVER_SK_PATH"
   <> value (serverSecretKeyFile dataDir)
   <> showDefault
   <> help "Path to the file with server secret key."
    )

-- | Parser that gets a path to Genesis data.
genesisOption :: FilePath
              -> Parser FilePath
genesisOption dataDir = strOption
    ( long "genesis" <> short 'g'
   <> metavar "GENESIS_DATA_PATH"
   <> value (genesisFile dataDir)
   <> showDefault
   <> help "Path to the file with Genesis data."
    )

-- | Parser that gets a path to a file with node configuration.
nodeConfigOption :: Parser FilePath
nodeConfigOption = strOption
    ( long "config"
   <> metavar "CONFIG_PATH"
   <> value nodeConfigFile
   <> showDefault
   <> help "Path to the file with configuration."
    )

-- | Parser that gets a path to a file with wallet configuration.
walletConfigOption :: Parser FilePath
walletConfigOption = strOption
    ( long "wconfig"
   <> metavar "CONFIG_PATH"
   <> value walletConfigFile
   <> showDefault
   <> help "Path to the file with wallet configuration."
    )

-- | Port which REST server will listen on.
restPortArgument :: Parser Word16
restPortArgument = argument auto ( metavar "REST_PORT"
                                <> help "Port to start rest server on"
                                 )
