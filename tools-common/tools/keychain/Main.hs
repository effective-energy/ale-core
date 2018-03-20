module Main where

import Universum

import Crypto.Random (MonadRandom)
import Options.Applicative (Parser, auto, command, execParser, fullDesc, help, helper, hsubparser,
                            info, metavar, option, progDesc, short, value)
import System.IO (FilePath)
import System.Wlog (WithLoggerIO, logInfo)

import Ale.Core.Crypto (generateSecretKey)
import Ale.Fmt (blockMapF, (+|), (|+))
import Ale.Log (setupAleLoggingWithName)
import Ale.Tools.Keychain (Key (Sec), loadKeychain, newKeychain, publicKeys, saveKeychain,
                           secretKeys)
import Ale.Tools.Options (keychainOption)
import Ale.Tools.Paths (getDataDir)

import qualified Ale.Data.HashMap as HM

data Config = Config FilePath Mode

data Mode = Generate Int | Show

config :: FilePath -> Parser Config
config dataDir = Config
    <$> keychainOption dataDir
    <*> hsubparser (
            command "generate" (info
                (Generate <$> option auto (short 'n' <> metavar "COUNT"
                                        <> value 9
                                        <> help "Number of keys to generate"))
                (progDesc "Generate a new keychain and fill it with fresh keys"))
         <> command "show" (info
                (pure Show)
                (progDesc "Show keychain"))
        )

main :: IO ()
main = do
    dataDir <- getDataDir
    conf <- execParser (opts dataDir)
    setupAleLoggingWithName "ale-keychain" $ run conf
  where
    opts dataDir = info (config dataDir <**> helper)
        ( fullDesc
        <> progDesc "Manage your keychains"
        )

run :: (MonadMask m, MonadRandom m, WithLoggerIO m)
    => Config -> m ()
run (Config kcp mode) = case mode of
    Generate count -> do
        logInfo $ "Generating "+|count|+" keys"
        skeys <- zip [1 ..] <$> replicateM count generateSecretKey

        let named = map (\(i, k) -> ("key"+|(i :: Int)|+"", Sec k)) skeys
        kc <- newKeychain kcp (HM.fromList named)

        logInfo $ "Saving keychain to "+|kcp|+""
        saveKeychain kc
    Show -> do
        logInfo $ "Loading keychain from "+|kcp|+""
        kc <- loadKeychain kcp
        secKeys <- secretKeys kc
        putTextLn $ "Secret keys:\n"+|blockMapF secKeys|+""
        pubKeys <- publicKeys kc
        putTextLn $ "Public keys:\n"+|blockMapF pubKeys|+""
