module Main where

import Universum

import Crypto.Random (MonadRandom, getRandomBytes)
import Data.Word (Word16)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Options.Applicative (Parser, argument, auto, execParser, fullDesc, help, helper, info, long,
                            metavar, option, progDesc, short)
import Servant.API (NoContent (NoContent))
import Servant.Client (BaseUrl (BaseUrl), ClientEnv (ClientEnv), Scheme (Http), runClientM)
import System.Wlog (WithLoggerIO, logError)

import Ale.Core.Crypto (PublicKey, SecretKey)
import Ale.Core.Tokens (TokenCount, TokenKind (..))
import Ale.Log (setupAleLoggingWithName)
import Ale.Node.Rest.Client (Node (nMessage), NodeMessage (nmPost), node)
import Ale.Tools.Keychain (ParsableKey (..), loadKeychain)
import Ale.Tools.Options (keychainOption, restPortArgument)
import Ale.Tools.Paths (getDataDir)
import Ale.Transactions (transfer)

import qualified Ale.Core.Tokens as T


-- TODO: This is pretty much a copypaste from the transfer tool.
--       We can probably have a library of common tool utilities.
--       Or we can make one main file with cli subcommands like
--       `ale-transfer -- simple ...options for ale-transfer exec`
--       `ale-transfer -- rest   ...options for ale-transfer-rest exec`

data Config = Config
    { cKeychain :: !FilePath
    , cRestPort :: !Word16
    , cMode     :: !Mode
    }

data Mode = Transfer
    { mSender   :: Named SecretKey
    , mReceiver :: Named PublicKey
    , mCount    :: TokenCount
    , mIssuer   :: Maybe (Named PublicKey)
    }

transferParser :: Parser Mode
transferParser = Transfer
    <$> argument keyReader (metavar "SENDER_SK" <> help "Sender secret key")
    <*> argument keyReader (metavar "RECEIVER_PK" <> help "Receiver public key")
    <*> argument auto (metavar "NUM_TOKENS"
                    <> help "Number of tokens to transfer")
    <*> optional (option keyReader (long "issuer" <> short 'i'
                                  <> metavar "ISSUER"
                                  <> help "Issuer of tokens (do not specify for money)"))

options :: FilePath -> Parser Config
options dataDir = Config
    <$> keychainOption dataDir
    <*> restPortArgument
    <*> transferParser

main :: IO ()
main = do
    dataDir <- getDataDir
    conf <- execParser (opts dataDir)
    setupAleLoggingWithName "transfer-rest-tool" $ run conf
  where
    opts dataDir = info (options dataDir <**> helper)
        ( fullDesc
        <> progDesc "Transfer tokens via REST API"
        )

run :: (MonadThrow m, MonadRandom m, WithLoggerIO m) => Config -> m ()
run Config{..} = do
    manager <- liftIO $ newManager defaultManagerSettings
    let baseUrl = BaseUrl Http "localhost" (fromIntegral cRestPort) ""
    let clientEnv = ClientEnv manager baseUrl

    kc <- loadKeychain cKeychain

    case cMode of
        Transfer{..} -> do
            sk <- resolveKey mSender kc
            to <- resolveKey mReceiver kc
            issuer <- case mIssuer of
                    Nothing -> pure Money
                    Just i  -> Transferable <$> resolveKey i kc
            challenge <- getRandomBytes 32
            let msg = transfer sk (T.fromList [(issuer, mCount)]) to challenge
            res <- liftIO $ runClientM (nmPost (nMessage node) msg) clientEnv
            case res of
                Left e          -> logError (show e) >> exitFailure
                Right NoContent -> pure ()
