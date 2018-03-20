module Main
       ( main
       ) where

import Universum

import Crypto.Random.Types (MonadRandom)
import Data.Aeson.Encode.Pretty (encodePretty)
import Options.Applicative (Parser, argument, auto, execParser, fullDesc, help, helper, info,
                            metavar, option, progDesc, short)
import System.IO (FilePath)
import System.Wlog (WithLoggerIO)

import Ale.Core.Crypto (PublicKey)
import Ale.Core.Genesis.Distribution (mkMoneyUniform)
import Ale.Core.Tokens (TokenCount)
import Ale.Log (setupAleLoggingWithName)
import Ale.Tools.Keychain (ParsableKey (..), loadKeychain)
import Ale.Tools.Options (keychainOption)
import Ale.Tools.Paths (getDataDir)


data Config = Config
    { cKeychain  :: FilePath
    , cKeys      :: [Named PublicKey]
    , cNumTokens :: !TokenCount
    }

options :: FilePath -> Parser Config
options dataDir = do
    cKeychain <- keychainOption dataDir
    cKeys <- many $ argument keyReader ( metavar "RECEIVER_PK"
                                      <> help "Public key that gets the tokens" )
    cNumTokens <- option auto ( short 'n' <> metavar "NUM_TOKENS"
                            <>  help "The number of tokens each party gets" )
    pure Config{..}


main :: IO ()
main = do
    dataDir <- getDataDir
    cfg <- execParser (opts dataDir)
    setupAleLoggingWithName "ale-distribute" $ run cfg
  where
    opts dataDir = info (options dataDir <**> helper)
        ( fullDesc
       <> progDesc "Generate a uniform money distribution" )

run :: (MonadRandom m, WithLoggerIO m, MonadThrow m) => Config -> m ()
run Config{..} = do
    kc <- loadKeychain cKeychain
    pkeys <- forM cKeys (`resolveKey` kc)

    let distr = mkMoneyUniform pkeys cNumTokens
    putTextLn $ decodeUtf8 $ encodePretty distr
