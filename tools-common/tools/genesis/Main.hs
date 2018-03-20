module Main
       ( main
       ) where

import Universum

import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, long, metavar,
                            progDesc, short, strArgument, strOption, value)
import System.IO (FilePath)
import System.Wlog (WithLoggerIO, logError, logInfo)

import Ale.Core.Crypto (publicFromBytes)
import Ale.Core.Genesis.Data (GenesisData (..), defaultValidityPeriod)
import Ale.Core.Storage.Types (mkReference, unsafeReferenceToBase16)
import Ale.Fmt ((+|), (|+))
import Ale.Log (setupAleLoggingWithName)
import Ale.Tools.IO (openFileOrStdin)
import Ale.Tools.Paths (genesisFile, genesisHashFile, getDataDir, serverKeyFile)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data Config = Config
    { cServerPublicKey :: !FilePath
    , cDistrFile       :: !FilePath
    }

options :: FilePath -> Parser Config
options dataDir = do
    let defaultPk = serverKeyFile dataDir

    cServerPublicKey <- strOption ( long "server-pk" <> short 'p'
                                 <> metavar "SERVER_PK_FILE"
                                 <> value defaultPk
                                 <> help ("Path to the server public key "
                                         <> "(default: "+|defaultPk|+")") )
    cDistrFile <- strArgument ( metavar "DISTRIBUTION_FILE"
                             <> help ("Path to initial balance distribution "
                                   <> "(or `-` to read from stdin)") )
    pure Config{..}


main :: IO ()
main = do
    dataDir <- getDataDir
    cfg <- execParser $ opts dataDir
    setupAleLoggingWithName "ale-genesis" $ run cfg
  where
    opts dataDir = info (options dataDir <**> helper)
      ( fullDesc
     <> progDesc "Generate a Genesis block for Ale" )

run :: (WithLoggerIO m, MonadFail m) => Config -> m ()
run Config{..} = do
    distrFile <- openFileOrStdin cDistrFile
    edistr <- liftIO $ eitherDecode <$> BSL.hGetContents distrFile
    logInfo $ show edistr
    case edistr of
        Left e -> logError $ "Can't parse distribution: "+|e|+""
        Right distribution -> do
            serverPk <- liftIO $ BS.readFile cServerPublicKey >>= publicFromBytes
            let genesis = GenesisData distribution serverPk defaultValidityPeriod
            let genesisRef = unsafeReferenceToBase16 $ mkReference genesis

            dataDir <- getDataDir

            -- save genesis
            let gfPath = genesisFile dataDir
            liftIO $ BSL.writeFile gfPath (encodePretty genesis)

            -- save hash of genesis
            let ghPath = genesisHashFile dataDir
            writeFile ghPath genesisRef
{-# ANN run ("HLint: ignore Avoid restricted function" :: Text) #-}
