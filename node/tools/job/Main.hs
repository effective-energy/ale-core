module Main
       ( main
       ) where

import Universum

import Control.Lens (Getter)
import Data.Aeson (FromJSON (..), eitherDecode, parseJSON, withObject, (.:), (.:?))
import Data.Default (def)
import Data.Time (UTCTime)
import Options.Applicative (Parser, argument, command, execParser, fullDesc, help, helper,
                            hsubparser, info, metavar, progDesc, strArgument)
import System.Wlog (LoggerNameBox, WithLoggerIO, logDebug, logError, logInfo)

import Ale.Core.Crypto (PublicKey, SecretKey, toPublic)
import Ale.Core.Crypto.Signed (mkSigned)
import Ale.Core.DHT.Types (MonadDHT (..))
import Ale.Core.Message (ContractorProposal (..), EmployerAcceptance (..), JobDescription (..),
                         JobOffer, Message (..), MessagePointer, Submission (..),
                         SubmissionData (..), jobOffer, mkPointer)
import Ale.Core.Requirements (Requirements, emptyProof)
import Ale.Core.Storage.Types (Reference, mkReference)
import Ale.Core.Submission.Acceptance (alwaysPass)
import Ale.Core.Tokens (Tokens)
import Ale.Fmt ((+|), (|+))
import Ale.Log (setupAleLoggingWithName)
import Ale.Node (waitFor)
import Ale.Node.Blockchain.Client (BlockchainClientComponent (..))
import Ale.Node.Context.Mining (MonadMining (..))
import Ale.Node.Startup (Components (..), withNode)
import Ale.State (AleStateData, MonadAleState (..), asdActiveProposals, asdOpenOffers,
                  asdWaitingSubmissions, getASDataOrError)
import Ale.State.MessageSet (MessageSet)
import Ale.Tools.IO (openFileOrStdin)
import Ale.Tools.Keychain (Keychain, ParsableKey (Named, keyReader, resolveKey), loadKeychain)
import Ale.Tools.Options (keychainOption, nodeConfigOption)
import Ale.Tools.Paths (getDataDir)

import qualified Ale.State.MessageSet as MS
import qualified Data.ByteString.Lazy as BSL

----------------------------------------
-- Datatypes
----------------------------------------

data TestJobOffer = TestJobOffer
    { jobDescription  :: Maybe Text
    , jobDeadline     :: Maybe UTCTime
    , jobRequirements :: Requirements
    , jobTokens       :: Tokens
    } deriving Show

instance FromJSON TestJobOffer where
    parseJSON = withObject "offer" $ \o -> TestJobOffer
        <$> o .:? "description"
        <*> (parseUTC <$> o .:? "deadline")
        <*> o .: "requirements"
        <*> o .: "tokens"
          where
            parseUTC :: Maybe Text -> Maybe UTCTime
            parseUTC = join . map (readMaybe . toString)

data Config = Config
    { cKeychain   :: FilePath
    , cNodeConfig :: FilePath
    , cMode       :: Mode
    }

data Mode
    = Offer
      { mEmployerSK :: Named SecretKey
      , mOfferData  :: FilePath
      }
    | Propose
      { mContractorSK :: Named SecretKey
      , mEmployerPK   :: Named PublicKey
      , mOfferData    :: FilePath
      }
    | Submit
      { mContractorSK   :: Named SecretKey
      , mEmployerPK     :: Named PublicKey
      , mOfferData      :: FilePath
      , mSubmissionData :: FilePath
      }
    | Accept
      { mEmployerSK     :: Named SecretKey
      , mContractorPK   :: Named PublicKey
      , mOfferData      :: FilePath
      , mSubmissionData :: FilePath
      }

----------------------------------------
-- Parsers
----------------------------------------

config :: FilePath -> Parser Config
config dataDir = do
    cKeychain <- keychainOption dataDir
    cNodeConfig <- nodeConfigOption
    cMode <- hsubparser
                 ( command "offer" (info
                     offerParser
                     (progDesc "Send Job Offer"))
                <> command "propose" (info
                     proposeParser
                     (progDesc "Send Job Proposal"))
                <> command "submit" (info
                     submitParser
                     (progDesc "Send Submission"))
                <> command "accept" (info
                     acceptParser
                     (progDesc "Send Acceptance"))
                 )
    pure Config{..}

offerParser :: Parser Mode
offerParser = Offer <$> employerSkArg <*> offerJsonArg

proposeParser :: Parser Mode
proposeParser = Propose <$> contractorSkArg <*> employerPkArg <*> offerJsonArg

submitParser :: Parser Mode
submitParser = Submit
    <$> contractorSkArg <*> employerPkArg <*> offerJsonArg <*> submDataArg

acceptParser :: Parser Mode
acceptParser = Accept
    <$> employerSkArg <*> contractorPkArg <*> offerJsonArg <*> submDataArg

----------------------------------------
-- Subprogram handlers
----------------------------------------

runOffer :: (MonadAleState m, WithLoggerIO m, MonadDHT m, MonadMining m)
         => BlockchainClientComponent
         -> SecretKey                  -- ^ Employer SK
         -> TestJobOffer
         -> m ()
runOffer bcc sk tjo = do
    jo <- mkJobOffer
    mine $ mkSigned sk $ MsgJobOffer jo


    let jop = mkPointer (toPublic sk) jo
    logInfo "Waiting for the job offer to get published..."
    asData <- getASDataOrError
    waitFor bcc ( msgIn jop asData asdOpenOffers)
    logInfo "The job offer was published!"
  where
    mkJobOffer :: (Monad m, MonadDHT m) => m JobOffer
    mkJobOffer = do
        mJobDescRef <- traverse (putDHT . toJobDescription) (jobDescription tjo)
        pure $ jobOffer
            mJobDescRef
            (jobDeadline tjo)
            (jobTokens tjo)
            (jobRequirements tjo)
            alwaysPass

    toJobDescription :: Text -> JobDescription
    toJobDescription = JobDescription . BSL.fromStrict . encodeUtf8

runPropose :: (MonadAleState m, WithLoggerIO m, MonadDHT m, MonadMining m)
           => BlockchainClientComponent
           -> SecretKey                  -- ^ Conctractor SK
           -> PublicKey                  -- ^ Employer PK
           -> TestJobOffer
           -> m ()
runPropose bcc sk pk tjo = do
    let jop = jobOfferPtr tjo pk
    logDebug "Looking for the job offer..."
    asData <- getASDataOrError
    waitFor bcc (msgIn jop asData asdOpenOffers)
    logDebug "Found the job offer"

    let proposal = ContractorProposal jop emptyProof
    mine $ mkSigned sk (MsgContractorProposal proposal)


    let pp = mkPointer (toPublic sk) proposal
    logInfo "Waiting for the proposal to get published..."
    waitFor bcc (msgIn pp asData asdActiveProposals)
    logInfo "The proposal was published!"

runSubmit :: (MonadAleState m, WithLoggerIO m, MonadDHT m, MonadMining m)
          => BlockchainClientComponent
          -> SecretKey                  -- ^ Contractor SK
          -> PublicKey                  -- ^ Employer PK
          -> TestJobOffer
          -> BSL.ByteString             -- ^ Submission data
          -> m ()
runSubmit bcc sk pk tjo blob = do
    let pp = proposalPtr tjo (toPublic sk) pk
    logDebug "Looking for the proposal..."
    asData <- getASDataOrError
    waitFor bcc (msgIn pp asData asdActiveProposals)
    logDebug "Found the proposal"

    let submission = Submission pp (SubmissionData blob) def
    mine $ mkSigned sk (MsgSubmission submission)

    let sp = mkPointer (toPublic sk) submission
    logInfo "Waiting for the submission to get published..."
    waitFor bcc ( msgIn sp asData asdWaitingSubmissions)
    logInfo "The submission was published!"

runAccept :: (MonadAleState m, WithLoggerIO m, MonadDHT m, MonadMining m)
          => BlockchainClientComponent
          -> SecretKey                  -- ^ Employer SK
          -> PublicKey                  -- ^ Contractor PK
          -> TestJobOffer
          -> BSL.ByteString             -- ^ Submission data
          -> m ()
runAccept bcc sk pk tjo blob = do
    let sp = submissionPtr tjo blob pk (toPublic sk)
    logDebug "Looking for the submission..."
    asData <- getASDataOrError
    waitFor bcc (msgIn sp asData asdWaitingSubmissions)
    logDebug "Found the submission"

    let acceptance = EmployerAcceptance sp
    mine $ mkSigned sk (MsgEmployerAcceptance acceptance)

    logInfo "Waiting for the acceptance to get published..."
    waitFor bcc (submissionNotWaiting sp asData)
    logInfo "The acceptance was published!"
  where
    submissionNotWaiting :: MessagePointer Submission -> AleStateData -> Maybe ()
    submissionNotWaiting sp asData =
        case MS.lookup sp (asData^.asdWaitingSubmissions) of
            Just _  -> Nothing
            Nothing -> Just ()

-- | Check that a message with the given pointer is present in the given
-- 'MessageSet' inside 'AleState'.
msgIn :: MessagePointer m
      -> AleStateData
      -> Getter AleStateData (MessageSet m)
      -> Maybe ()
msgIn p asData l = void $ MS.lookup p (asData^.l)


resolveTJO :: (WithLoggerIO m)
           => FilePath -> m TestJobOffer
resolveTJO path = do
    jsonH <- openFileOrStdin path
    etjo <- eitherDecode <$> liftIO (BSL.hGetContents jsonH)
    case etjo of
        Left err -> do
            logError $ "Failed to get JSON: "+|err|+""
            exitFailure
        Right tjo -> pure tjo

resolveKeys :: (ParsableKey k1, ParsableKey k2, MonadIO m, MonadThrow m)
            => (Named k1, Named k2)
            -> Keychain
            -> m (k1, k2)
resolveKeys (k1, k2) kc = (,) <$> resolveKey k1 kc <*> resolveKey k2 kc

run :: Config -> LoggerNameBox IO ()
run Config{..} = withNode cNodeConfig def $ \(Components bcc) -> do
    kc <- loadKeychain cKeychain
    case cMode of
        Offer{..} -> do
            sk <- resolveKey mEmployerSK kc
            tjo <- resolveTJO mOfferData
            runOffer bcc sk tjo
        Propose{..} -> do
            (sk, pk) <- resolveKeys (mContractorSK, mEmployerPK) kc
            tjo <- resolveTJO mOfferData
            runPropose bcc sk pk tjo
        Submit{..} -> do
            (sk, pk) <- resolveKeys (mContractorSK, mEmployerPK) kc
            tjo <- resolveTJO mOfferData
            blob <- liftIO $ BSL.hGetContents =<< openFileOrStdin mSubmissionData
            runSubmit bcc sk pk tjo blob
        Accept{..} -> do
            (sk, pk) <- resolveKeys (mEmployerSK, mContractorPK) kc
            tjo <- resolveTJO mOfferData
            blob <- liftIO $ BSL.hGetContents =<< openFileOrStdin mSubmissionData
            runAccept bcc sk pk tjo blob

main :: IO ()
main = do
    dataDir <- getDataDir
    cfg <- execParser $ opts dataDir
    setupAleLoggingWithName "ale-job-offer" $ run cfg
  where
    opts dataDir = info (config dataDir <**> helper)
        ( fullDesc
       <> progDesc "Send Job Offer"
        )

----------------------------------------
-- Parsing helpers
----------------------------------------

employerSkArg :: Parser (Named SecretKey)
employerSkArg = argument keyReader
    ( metavar "EMPLOYER_SK" <> help "Employer secret key")

employerPkArg :: Parser (Named PublicKey)
employerPkArg = argument keyReader
    (metavar "EMPLOYER_PK" <> help "Employer public key")

contractorSkArg :: Parser (Named SecretKey)
contractorSkArg = argument keyReader
    (metavar "CONTRACTOR_SK" <> help "Contractor secret key")

contractorPkArg :: Parser (Named PublicKey)
contractorPkArg = argument keyReader
    (metavar "CONTRACTOR_PK" <> help "Contractor public key")

offerJsonArg :: Parser FilePath
offerJsonArg = strArgument
    (metavar "JSON_FILE" <> help "JSON with Job Offer details")

submDataArg :: Parser FilePath
submDataArg = strArgument
    (metavar "BLOB_FILE" <> help "Data to be submitted")


----------------------------------------
-- Internal conversion functions
----------------------------------------

jobOfferPtr :: TestJobOffer -> PublicKey -> MessagePointer JobOffer
jobOfferPtr tjo pk = mkPointer pk $ jobOffer
    (parseJobDescription <$> jobDescription tjo)
    (jobDeadline tjo)
    (jobTokens tjo)
    (jobRequirements tjo)
    alwaysPass
      where
        parseJobDescription :: Text -> Reference JobDescription
        parseJobDescription = mkReference . JobDescription .
            BSL.fromStrict . encodeUtf8 . toText

proposalPtr :: TestJobOffer -> PublicKey -> PublicKey
            -> MessagePointer ContractorProposal
proposalPtr tjo pkc pke = mkPointer pkc $ ContractorProposal (jobOfferPtr tjo pke)
                                                              emptyProof

submissionPtr :: TestJobOffer -> BSL.ByteString -> PublicKey -> PublicKey
              -> MessagePointer Submission
submissionPtr tjo sBlob pkc pke = mkPointer pkc $ Submission
    (proposalPtr tjo pkc pke) (SubmissionData sBlob) def
