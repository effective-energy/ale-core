module Main
       ( main
       ) where

import Universum

import Control.Monad.Reader (withReaderT)
import Data.Default (def)
import Monad.Capabilities (addCap)
import Network.Socket (PortNumber)
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Options.Applicative (Parser, execParser, fullDesc, helper, info, progDesc)
import System.Wlog (LoggerNameBox, logDebug, logInfo)
import UnliftIO.Async (withAsync)

import Ale.Fmt (listF, (+|), (|+))
import Ale.Log (setupAleLoggingWithName)
import Ale.Node (foreverBlocks)
import Ale.Node.Context.Mining (MonadMining (..))
import Ale.Node.Startup (Components (..), IOCaps (..), withNode')
import Ale.State (getASDataOrError)
import Ale.Tools.Options (nodeConfigOption, restPortArgument, walletConfigOption)
import Ale.Wallet.Claim (claimAll)
import Ale.Wallet.Config (awcMOTD, loadWalletConfig)
import Ale.Wallet.Rest.Server (walletServer)
import Ale.Wallet.State (getWSManagedSecretKeys, walletStateImpl)

data Options = Options
    { oConfigFile       :: !FilePath
    , oWalletConfigFile :: !FilePath
    , oRestPort         :: !Word16
    }

options :: Parser Options
options = do
    oConfigFile <- nodeConfigOption
    oWalletConfigFile <- walletConfigOption
    oRestPort <- restPortArgument
    pure Options {..}

main :: IO ()
main = do
    conf <- liftIO $ execParser opts
    setupAleLoggingWithName "ale-wallet" $ do
        logInfo "Logging started"
        run conf
  where
    opts = info (options <**> helper) (fullDesc <> progDesc "Ale Wallet")

run :: Options -> LoggerNameBox IO ()
run Options{..} = do
    logInfo "Logging started"
    startRestServer (fromIntegral oRestPort)
  where
    startRestServer :: PortNumber -> LoggerNameBox IO ()
    startRestServer p = withNode' oConfigFile def $ \(Components bcc) iocaps -> do
        walletCfg <- loadWalletConfig oWalletConfigFile
        logInfo $ "MOTD: "+|walletCfg^.awcMOTD|+""

        let onUpdate = do
                std <- getASDataOrError
                secretKeys <- getWSManagedSecretKeys
                let toClaim = claimAll secretKeys std
                logDebug $ "Claiming "+|listF toClaim|+""
                forM_ toClaim mine

        withReaderT (addCap walletStateImpl) $
            withAsync (foreverBlocks bcc onUpdate) $ \_ -> do
                logInfo $ "Starting REST server on localhost:" <> show p
                liftIO $ runSettings warpSettings $ walletServer (addCap walletStateImpl $ unIOCaps iocaps)
      where
        warpSettings :: Settings
        warpSettings = defaultSettings & setPort (fromIntegral p)
                                       & setBeforeMainLoop (putTextLn httpWelcome)

        httpWelcome :: Text
        httpWelcome = "Navigate to http://localhost:" <> show p <> "/ping"
