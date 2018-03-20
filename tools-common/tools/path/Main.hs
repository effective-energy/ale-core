module Main where

import Universum

import Options.Applicative (Parser, command, execParser, fullDesc, helper, hsubparser, info,
                            progDesc)

import Ale.Tools.Paths (genesisFile, getDataDir, keychainFile, serverKeyFile, serverSecretKeyFile)


data Config = Data | Genesis | Keychain | ServerSecretKey | ServerPublicKey

config :: Parser Config
config = hsubparser (
             command "data" (info
                 (pure Data)
                 (progDesc "Root of the data directory"))
          <> command "genesis" (info
                 (pure Genesis)
                 (progDesc "Genesis file"))
          <> command "keychain" (info
                 (pure Keychain)
                 (progDesc "Keychain file"))
          <> command "server-key" (info
                 (pure ServerPublicKey)
                 (progDesc "Server public key file"))
          <> command "server-secret-key" (info
                 (pure ServerSecretKey)
                 (progDesc "Server secret key file"))
         )

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (config <**> helper)
        ( fullDesc
       <> progDesc "Find out the default paths of Ale data files"
        )

run :: Config -> IO ()
run mode = do
    dataDir <- getDataDir
    let f = case mode of
                Data            -> identity
                Genesis         -> genesisFile
                Keychain        -> keychainFile
                ServerSecretKey -> serverSecretKeyFile
                ServerPublicKey -> serverKeyFile
    putStrLn $ f dataDir
