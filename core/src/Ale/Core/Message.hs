{-# LANGUAGE Rank2Types #-}

-- | Basic Ale protocol messages.
module Ale.Core.Message
       ( Message (..)
       , getHeight

       , MessagePointer ()
       , mkPointer
       , _MsgJobOffer

       -- * Job offer
       , JobOffer
       , jobOffer
       , joDescription
       , joDeadline
       , joPrice
       , joRequirements
       , joAccept
       , joHeight
       , joExtra

       -- * joExtra guts
       , ExtraFieldValue(..)

       , JobDescription (JobDescription)
       , Deadline

       -- * Contractor proposal
       , ContractorProposal (..)

       -- * Submission
       , Submission (..)
       , SubmissionData (SubmissionData)

       -- * Employer acceptance
       , EmployerAcceptance (..)
       ) where

import Universum hiding (HashMap)

import Codec.Serialise (Serialise (..), deserialiseOrFail, serialise)
import Control.Lens (at, lens)
import Control.Lens.TH (makeLenses, makePrisms)
import Crypto.Hash (Blake2b_256, Digest, digestFromByteString, hash)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), genericParseJSON, genericToEncoding,
                   genericToJSON, object, withObject, withText, (.:), (.:?), (.=))
import Data.Aeson.Encoding (text)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Text (unpack)
import Data.Time (UTCTime)
import Serokell.Aeson.Options (defaultOptions)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

import Ale.Core.Entity (Entity)
import Ale.Core.Height (Height)
import Ale.Core.Requirements (ReqProof, Requirements)
import Ale.Core.Storage (HasPrefixTag (..), Reference, Storable, StoragePrefix (..))
import Ale.Core.Submission.Acceptance (AcceptanceTest)
import Ale.Core.Tokens (Tokens)
import Ale.Data.HashMap (HashMap)
import Ale.Fmt (Buildable (..), Builder, (+|), (|+))
import Ale.Util.Digest (decodeDigest, encodeDigest)
import Ale.Util.Json (bytesFromJson64, bytesToJson64)

import qualified Ale.Data.HashMap as HM
import qualified Data.ByteString.Lazy as BSL
import qualified Serokell.Util.Base16 as SB16

-- I wanted to disable this warning for `parseJSON` only but hlint
-- doesn't treat class methods as functions.
{-# ANN module ("HLint: ignore Use <$>" :: Text) #-}

-- | Represents a basic Ale protocol message.
data Message = MsgJobOffer !JobOffer
             | MsgContractorProposal !ContractorProposal
             | MsgSubmission !Submission
             | MsgEmployerAcceptance !EmployerAcceptance
    deriving (Eq, Generic, Show)

instance Serialise Message where

instance HasPrefixTag Message where
    storagePrefix = StoragePrefix "message"

instance Buildable Message where
    build (MsgJobOffer msg)           = build msg
    build (MsgContractorProposal msg) = build msg
    build (MsgSubmission msg)         = build msg
    build (MsgEmployerAcceptance msg) = build msg

instance ToJSON Message where
    toJSON = genericToJSON defaultOptions
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Message where
    parseJSON = genericParseJSON defaultOptions

getHeight :: Message -> Maybe Height
getHeight (MsgJobOffer m)   = Just $ _joHeight m
getHeight (MsgSubmission m) = Just $ sHeight m
getHeight _                 = Nothing

-- | A pointer to a message published previously on the blockchain.
newtype MessagePointer m = MessagePointer { unsafeUnpackMsgPtr :: Digest Blake2b_256 }
    deriving (Eq, Ord, Show)

-- | 'MessagePointer' constructor
mkPointer :: Serialise m
          => Entity   -- ^ Sender of the message
          -> m        -- ^ Content of the message
          -> MessagePointer m
mkPointer pk msg = MessagePointer . hash . BSL.toStrict . serialise $ (pk, msg)

instance Serialise m => Serialise (MessagePointer m) where
    encode (MessagePointer ptr) = encodeDigest ptr
    decode = MessagePointer <$> decodeDigest

instance Buildable (MessagePointer m) where
    build (MessagePointer x) = show x

instance ToJSON (MessagePointer m) where
    toJSON = String . unsafeMsgPtrToBase16
    toEncoding = text . unsafeMsgPtrToBase16

instance FromJSON (MessagePointer m) where
    parseJSON = withText "MessagePointer" unsafeMsgPtrFromBase16

instance ToHttpApiData (MessagePointer m) where
    toUrlPiece = unsafeMsgPtrToBase16

instance FromHttpApiData (MessagePointer m) where
    parseUrlPiece = maybeToRight "Bad MessagePointer" . unsafeMsgPtrFromBase16

-- | Convert a reference to a base16 encoded 'Text'.
unsafeMsgPtrToBase16 :: MessagePointer a -> Text
unsafeMsgPtrToBase16 = SB16.encode . convert . unsafeUnpackMsgPtr

-- | Try to convert a base16 encoded 'Text' to a message pointer.
unsafeMsgPtrFromBase16 :: MonadFail m => Text -> m (MessagePointer a)
unsafeMsgPtrFromBase16 txt = case SB16.decode txt of
    Left e   -> fail $ unpack e
    Right bs -> case digestFromByteString bs of
        Nothing -> fail "Invalid digest value"
        Just k  -> pure $ MessagePointer k


----------------------------------------------------------------------------
-- Job offer
----------------------------------------------------------------------------

-- | A /job offer/ contains the description and various metadata
-- for a job posted by an employer.
data JobOffer = JobOffer
    { _joDescription  :: !(Maybe (Reference JobDescription))
    , _joDeadline     :: !(Maybe Deadline)
    , _joPrice        :: !Tokens
    , _joRequirements :: !Requirements
    , _joAccept       :: !AcceptanceTest
    , _joHeight       :: !Height
    , _joExtraFields  :: !(HashMap Text ExtraFieldValue)
    } deriving (Eq, Generic, Show)

instance Serialise JobOffer where

instance HasPrefixTag JobOffer where
    storagePrefix = StoragePrefix "jobOffer"

instance Buildable JobOffer where
    build JobOffer{..} = "<JobOffer "+|descr|+" "+|deadline|+">"
      where
        descr :: Builder
        descr = case _joDescription of
            Nothing -> "[]"
            Just r  -> build r
        deadline :: Builder
        deadline = case _joDeadline of
            Nothing -> "∞"
            Just d  -> show d

instance ToJSON JobOffer where
    toJSON JobOffer{..} = object [ "descr"    .= _joDescription
                                 , "deadline" .= _joDeadline
                                 , "price"    .= _joPrice
                                 , "reqs"     .= _joRequirements
                                 , "acc"      .= _joAccept
                                 , "epoch"    .= _joHeight
                                 , "extra"    .= _joExtraFields
                                 ]

instance FromJSON JobOffer where
    parseJSON = withObject "JobOffer" $ \o -> JobOffer
        <$> o .:? "descr"
        <*> o .:? "deadline"
        <*> o .:  "price"
        <*> o .:  "reqs"
        <*> o .:  "acc"
        <*> o .:  "epoch"
        <*> o .:  "extra"


-- | Smart constructor for 'JobOffer' that fills '_joExtraFields'
-- itself.
jobOffer
    :: Maybe (Reference JobDescription)
    -> Maybe UTCTime
    -> Tokens
    -> Requirements
    -> AcceptanceTest
    -> JobOffer
jobOffer desc deadline toks reqs acc = JobOffer desc deadline toks reqs acc def HM.empty

-- | The description of a job is just a sequence of bytes
-- with unspecified meaning.
newtype JobDescription = JobDescription BSL.ByteString
    deriving (Eq, Serialise, Storable, Show)

instance HasPrefixTag JobDescription where
    storagePrefix = StoragePrefix "jobdesc"

instance ToJSON JobDescription where
    toJSON (JobDescription bs) = toJSON $ bytesToJson64 $ BSL.toStrict bs
    toEncoding (JobDescription bs) = toEncoding $ bytesToJson64 $ BSL.toStrict bs

instance FromJSON JobDescription where
    parseJSON v = JobDescription . BSL.fromStrict <$> (parseJSON v >>= bytesFromJson64)


type Deadline = UTCTime


newtype ExtraFieldValue = ExtraFieldValue ByteString
    deriving (Eq, Serialise, Show)

instance ToJSON ExtraFieldValue where
    toJSON (ExtraFieldValue efv) = toJSON $ bytesToJson64 efv
    toEncoding (ExtraFieldValue efv) = toEncoding $ bytesToJson64 efv

instance FromJSON ExtraFieldValue where
    parseJSON v = ExtraFieldValue <$> (bytesFromJson64 =<< parseJSON v)

----------------------------------------------------------------------------
-- Contractor proposal
----------------------------------------------------------------------------

-- | A contractor sends a /contractor proposal/ when they are willing
-- to accept a job offer.
data ContractorProposal = ContractorProposal
    { cpJobOfferPtr :: !(MessagePointer JobOffer)
    , cpProof       :: !ReqProof
    } deriving (Eq, Generic, Show)

instance Serialise ContractorProposal where

instance Buildable ContractorProposal where
    build ContractorProposal{..} =
        "<ContractorProposal "+|cpJobOfferPtr|+">"

instance ToJSON ContractorProposal where
    toJSON ContractorProposal{..} = object [ "jobOfferPtr" .= cpJobOfferPtr
                                           , "proof"       .= cpProof
                                           ]

instance FromJSON ContractorProposal where
    parseJSON = withObject "ContractorProposal" $ \o -> ContractorProposal
        <$> o .: "jobOfferPtr"
        <*> o .: "proof"


----------------------------------------------------------------------------
-- Submission
----------------------------------------------------------------------------

-- | A /submission/ is what the contractor sends to the employer as the result
-- of their work.
data Submission = Submission
    { sProposalPtr :: !(MessagePointer ContractorProposal)
    , sData        :: !SubmissionData
    , sHeight      :: !Height
    } deriving (Eq, Generic, Show)

instance Serialise Submission where

instance Buildable Submission where
    build Submission{..} =
        "<Submission "+|sProposalPtr|+">"

instance ToJSON Submission where
    toJSON Submission{..} = object [ "proposalPtr" .= sProposalPtr
                                   , "data"        .= sData
                                   , "epoch"        .= sHeight
                                   ]

instance FromJSON Submission where
    parseJSON = withObject "Submission" $ \o -> Submission
        <$> o .: "proposalPtr"
        <*> o .: "data"
        <*> o .: "epoch"

-- | The contractor submits a binary blob.
newtype SubmissionData = SubmissionData BSL.ByteString
    deriving (Eq, Serialise, Storable, Show)

instance HasPrefixTag SubmissionData where
    storagePrefix = StoragePrefix "submdata"

instance ToJSON SubmissionData where
    toJSON (SubmissionData bs) = bytesToJson64 . BSL.toStrict $ bs

instance FromJSON SubmissionData where
    parseJSON v = parseJSON v
              >>= bytesFromJson64
              >>= pure . SubmissionData . BSL.fromStrict

----------------------------------------------------------------------------
-- Employer acceptance
----------------------------------------------------------------------------

-- | If the employer is satisfied with a submission, they confirm this
-- by publishing an /employer acceptance/.
data EmployerAcceptance = EmployerAcceptance
    { eaSubmissionPtr :: !(MessagePointer Submission)
    } deriving (Eq, Generic, Show)

instance Serialise EmployerAcceptance where

instance Buildable EmployerAcceptance where
    build EmployerAcceptance{..} =
        "<EmployerAcceptance "+|eaSubmissionPtr|+">"

instance ToJSON EmployerAcceptance where
    toJSON EmployerAcceptance{..} = object [ "submissionPtr" .= eaSubmissionPtr
                                           ]

instance FromJSON EmployerAcceptance where
    parseJSON = withObject "EmployerAcceptance" $ \o -> EmployerAcceptance
        <$> o .: "submissionPtr"

----------------------------------------
--- Template Haskell
----------------------------------------

makePrisms ''Message

makeLenses ''JobOffer

-- | This lens allows one to retrieve, modify or delete 'JobOffer'
-- extra fields.  Removal can be done with setting 'Nothing'.
joExtra :: Serialise a => Text -> Lens' JobOffer (Maybe a)
joExtra key = joExtraFields.at key.conv
  where
    conv :: Serialise a => Lens' (Maybe ExtraFieldValue) (Maybe a)
    conv = lens (>>= deserialiseOrNothing) (\_ -> fmap $ ExtraFieldValue . BSL.toStrict . serialise)

    deserialiseOrNothing :: Serialise a => ExtraFieldValue -> Maybe a
    deserialiseOrNothing (ExtraFieldValue bs) = case deserialiseOrFail (BSL.fromStrict bs) of
        Left _  -> Nothing
        Right a -> Just a
