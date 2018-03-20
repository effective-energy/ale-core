module Main where

import Universum

import Crypto.Random (MonadRandom)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, long, metavar,
                            progDesc, short, strArgument, switch)
import System.IO (FilePath)
import System.Wlog (WithLoggerIO, logInfo)

import Ale.Core.Crypto (generateSecretKey, publicToBytes, secretToBytes, toPublic)
import Ale.Fmt ((+|), (|+))
import Ale.Log (setupAleLoggingWithName)

import qualified Data.ByteString as BS


data Config = Config Bool FilePath

config :: Parser Config
config = Config
    <$> switch ( long "public" <> short 'p'
              <> help "Also write public key to OUTPUT_FILE.pub" )
    <*> strArgument ( metavar "OUTPUT_FILE"
                   <> help "File to write secret key to" )


main :: IO ()
main = execParser opts >>= setupAleLoggingWithName "ale-keygen" . run
  where
    opts = info (config <**> helper)
        ( fullDesc
       <> progDesc "Generate a key"
        )

run :: (MonadRandom m, WithLoggerIO m) => Config -> m ()
run (Config writePublic skPath) = do
    sk <- generateSecretKey
    logInfo $ "Writing secret key to "+|skPath|+""
    liftIO $ BS.writeFile skPath $ secretToBytes sk

    when writePublic $ do
        let pkPath = skPath ++ ".pub"
        logInfo $ "Writing public key to "+|pkPath|+""
        liftIO $ BS.writeFile pkPath $ publicToBytes (toPublic sk)
