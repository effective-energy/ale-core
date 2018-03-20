module Main
       ( main
       ) where

import Universum

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, long, progDesc, short,
                            switch)
import System.IO (hFlush, stdout)
import System.Wlog (WithLoggerIO, logError, logInfo)

import Ale.Fmt ((+||), (||+))
import Ale.Log (setupAleLoggingWithName)
import Ale.Node.Networking.Client (ClientContext (..), withClient)
import Ale.Node.PingPong.Client (PingComponent (..), withPing)

data Options = Options
    { oOnlyStats :: Bool
    }

options :: Parser Options
options = Options
    <$> switch ( short 's'
                 <> long "only-stats"
                 <> help "Check if you don't want \"> \" to be printed")

main :: IO ()
main = setupAleLoggingWithName "ale-node-ping"
     $ liftIO (execParser opts) >>= ping
  where
    opts = info (options <**> helper)
           ( fullDesc
             <> progDesc "Send pings to Pong")

ping :: (MonadMask m, MonadUnliftIO m, WithLoggerIO m) => Options -> m ()
ping Options{..} = do
    logInfo "Starting... "

    let run = withClient "127.0.0.1" 12345 [ "ping-pong" ] 1000000 $ \cCtx -> do
            let [cc] = ccComponents cCtx
            void $ withPing cc $ \pc -> do
                input <- getContents
                forM_ input $ \_ -> do
                    unless oOnlyStats $ do
                        putText "> "
                        liftIO (hFlush stdout)
                    pcPing pc

                stats <- pcPingStats pc
                putStrLn $ encodePretty stats

    run `catchAny` (\e -> logError $ "Error in network: "+||e||+"")
