{-# LANGUAGE FlexibleContexts #-}

module Main
       ( main
       ) where

import Universum

import Data.Default (def)
import Network.Socket (PortNumber)
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Options.Applicative (Parser, execParser, fullDesc, helper, info, progDesc)
import System.Wlog (LoggerNameBox, logInfo, logWarning)
import UnliftIO.Async (withAsync)

import Ale.Log (setupAleLoggingWithName)
import Ale.Node (foreverBlocks)
import Ale.Node.Rest.Server (nodeServer)
import Ale.Node.Startup (Components (..), IOCaps (..), withNode')
import Ale.Tools.Options (nodeConfigOption, restPortArgument)


data Options = Options
    { oConfigFile :: !FilePath
    , oRestPort   :: !(Maybe Word16)
    }

options :: Parser Options
options = do
    oConfigFile <- nodeConfigOption
    oRestPort <- optional restPortArgument
    pure Options {..}


main :: IO ()
main = do
    conf <- execParser opts
    setupAleLoggingWithName "ale-node" $ do
        logInfo "Logging started"
        run conf
  where
    opts = info (options <**> helper) (fullDesc <> progDesc "Ale Node")

run :: Options -> LoggerNameBox IO ()
run Options{..} = maybe
    (logWarning "REST port not specified, exiting...")
    (startRestServer . fromIntegral)
    oRestPort
  where
    startRestServer :: PortNumber -> LoggerNameBox IO ()
    startRestServer p = withNode' oConfigFile def $ \(Components bcc) iocaps ->
        withAsync (usingReaderT (unIOCaps iocaps) $ foreverBlocks bcc pass) $ \_ -> do
            logInfo $ "Starting REST server on localhost:" <> show p
            liftIO $ runSettings warpSettings $ nodeServer (unIOCaps iocaps)
      where

        warpSettings :: Settings
        warpSettings = defaultSettings & setPort (fromIntegral p)
                                       & setBeforeMainLoop (putTextLn httpWelcome)

        httpWelcome :: Text
        httpWelcome = "Navigate to http://localhost:" <> show p <> "/ping"
