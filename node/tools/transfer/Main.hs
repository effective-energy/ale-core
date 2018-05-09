module Main where

import Universum

import Crypto.Random (getRandomBytes)
import Data.Default (def)
import Options.Applicative (Parser, argument, auto, command, execParser, fullDesc, help, helper,
                            hsubparser, info, long, metavar, option, progDesc, short)
import System.IO (FilePath)
import System.Wlog (LoggerNameBox, logInfo)

import Ale.Core.Crypto (PublicKey, SecretKey)
import Ale.Core.Crypto.Signed ()
import Ale.Core.Message ()
import Ale.Core.Tokens (TokenCount)
import Ale.Fmt (blockListF, (+|), (|+))
import Ale.Log (setupAleLoggingWithName)
import Ale.Node (foreverBlocks)
import Ale.Node.Context.Mining (MonadMining (..))
import Ale.Node.Startup (Components (..), withNode)
import Ale.State (asdOpenOffers, getASBlockOrError, getASDataOrError, getASHeightOrError)
import Ale.Tools.Keychain (ParsableKey (..), loadKeychain)
import Ale.Tools.Options (keychainOption, nodeConfigOption)
import Ale.Tools.Paths (getDataDir)
import Ale.Transactions (receive, transfer)

import qualified Ale.Core.Tokens as T
import qualified Ale.State.MessageSet as MS

data Config = Config
    { cKeychain   :: !FilePath
    , cNodeConfig :: !FilePath
    , cMode       :: !Mode
    }

data Mode
    = Transfer (Named SecretKey) (Named PublicKey) TokenCount (Maybe (Named PublicKey))
    | Receive (Named SecretKey)

transferParser :: Parser Mode
transferParser = Transfer
    <$> argument keyReader (metavar "SENDER_SK" <> help "Sender secret key")
    <*> argument keyReader (metavar "RECEIVER_PK" <> help "Receiver public key")
    <*> argument auto (metavar "NUM_TOKENS"
                    <> help "Number of tokens to transfer")
    <*> optional (option keyReader (long "issuer" <> short 'i'
                                  <> metavar "ISSUER"
                                  <> help "Issuer of tokens (do not specify for money)"))

receiveParser :: Parser Mode
receiveParser = Receive
    <$> argument keyReader (metavar "RECEIVER_SK" <> help "Receiver secret key")

config :: FilePath -> Parser Config
config dataDir = do
    cKeychain <- keychainOption dataDir
    cNodeConfig <- nodeConfigOption
    cMode <- hsubparser
        ( command "move" (info
              transferParser
              (progDesc "Initiate tokens transfer from one account to another"))
       <> command "claim" (info
              receiveParser
              (progDesc "Claim all sent but not yet claimed transactions"))
        )
    pure Config{..}

main :: IO ()
main = do
    dataDir <- getDataDir
    conf <- execParser (opts dataDir)
    setupAleLoggingWithName "transfer-tool" $ run conf
  where
    opts dataDir = info (config dataDir <**> helper)
        ( fullDesc
        <> progDesc "Transfer and receive tokens"
        )

run :: Config -> LoggerNameBox IO ()
run Config{..} = withNode cNodeConfig def $ \(Components bcc) -> do
    kc <- loadKeychain cKeychain
    case cMode of
        Transfer sk' to' cnt missuer -> do
            sk <- resolveKey sk' kc
            to <- resolveKey to' kc
            tkind <- case missuer of
                Nothing      -> pure T.Money
                Just issuer' -> T.Transferable <$> resolveKey issuer' kc

            challenge <- liftIO $ getRandomBytes 32
            height <- getASHeightOrError
            let env = transfer sk height (T.fromList [(tkind, cnt)]) to challenge
            mine env

        Receive sk'                  -> do
            sk <- resolveKey sk' kc
            foreverBlocks bcc (claimEverything sk)
          where
            claimEverything sk = do
                asBlock <- getASBlockOrError
                logInfo $ "Investigating new block: "+|asBlock|+""
                asData  <- getASDataOrError
                let offers = MS.toList (asData^.asdOpenOffers)
                let msgs = rights [receive sk joPtr jo | (joPtr, _, jo) <- offers]
                logInfo $ "Claiming "+|blockListF msgs|+""
                forM_ msgs mine
