module Main
       ( main
       ) where

import Universum

import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Monad.Capabilities (CapsT)
import Options.Applicative (Parser, execParser, fullDesc, helper, info, progDesc)
import System.Wlog (logDebug, logError, logInfo)

import Ale.Core.Crypto (secretFromBytes, toPublic)
import Ale.Core.DHT.Types (MonadDHT (putDHT))
import Ale.Core.Genesis.Block (genesisBlockPointer)
import Ale.Core.Genesis.Data (gdServerKey)
import Ale.Fmt (fmt, (+|), (|+))
import Ale.Hub.Blockchain.Server (MonadBlockchain (bSaveBlock), withBlockchain)
import Ale.Hub.DHT.Server (withStorage)
import Ale.Hub.Mining.Server (MonadMining (mGetNextBlock), withMining)
import Ale.Node.Context.Logging (Logging, setupAleLoggingWithName)
import Ale.Node.Context.State (withAleState)
import Ale.Node.DB.Rocks (withDBImpl)
import Ale.Node.Networking.Server (ServerContext (..), withServer)
import Ale.Tools.Options (genesisOption, serverSecretKeyOption)
import Ale.Tools.Paths (getDataDir)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data Config = Config
    { cGenesisFile         :: FilePath
    , cServerSecretKeyFile :: FilePath
    }

config :: FilePath -> Parser Config
config dataDir = do
    cGenesisFile <- genesisOption dataDir
    cServerSecretKeyFile <- serverSecretKeyOption dataDir
    pure Config{..}

main :: IO ()
main = do
    dataDir <- getDataDir
    conf <- execParser (opts dataDir)
    setupAleLoggingWithName "ale-hub" $ run conf
  where
    opts dataDir = info (config dataDir <**> helper)
        ( fullDesc
        <> progDesc "ALE Hub"
        )

run :: Config -> CapsT '[Logging] IO ()
run conf = do
    logInfo "ALE Hub starting..."
    withServer "127.0.0.1" 12347
        [ "dht", "blockchain", "mining" ] 1000000 $ run' conf

run' :: Config -> ServerContext -> CapsT '[Logging] IO ()
run' Config{..} sCtx = do
    let [sc', bc', mc'] = scComponents sCtx

    eitherDecode <$> liftIO (BSL.readFile cGenesisFile) >>= \case
        Left err -> do
            logError $ "Failed to decode Genesis data: "+|err|+""
            exitFailure
        Right genesisData -> do
            logInfo "Genesis data was decoded successfully"
            logDebug $ fmt $ encodePrettyToTextBuilder genesisData

            serverSk <- liftIO $ secretFromBytes =<< BS.readFile cServerSecretKeyFile
            when (genesisData^.gdServerKey /= toPublic serverSk) $ do
                logError $ "Genesis server key "+|genesisData^.gdServerKey|+
                    " doesn't match server key "+|serverSk|+""
                exitFailure
            logDebug $ "Server uses key "+|serverSk|+""

            withDBImpl "db/hub" $ \_ ->
                withStorage sc' $ \_ ->
                    withAleState genesisData $ \_ -> do
                        void $ putDHT genesisData
                        let genBPtr = genesisBlockPointer genesisData
                        withBlockchain serverSk genBPtr bc' $ \_ ->
                            withMining serverSk miningDelay mc' $ \_ ->
                                -- All we do is just forward mined blocks
                                -- from Mining to Blockchain.
                                forever $ do
                                    b <- mGetNextBlock
                                    bSaveBlock b

miningDelay :: Int
miningDelay = 10000000  -- 10s
