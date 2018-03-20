{-# LANGUAGE GADTs #-}

-- | This module provides basic functions for sending and receiving
-- tokens between users.
module Ale.Transactions
       ( transfer
       , receive
       , ReceiveError(..)
       ) where

import Universum

import Codec.Serialise (serialise)
import Control.Monad.Except (Except, MonadError, runExcept, throwError)

import Ale.Core.Crypto (SecretKey, sign, toPublic)
import Ale.Core.Crypto.Signed (Signed, mkSigned)
import Ale.Core.Entity (Entity)
import Ale.Core.Message (ContractorProposal (..), JobOffer, Message (..), MessagePointer,
                         joRequirements, jobOffer)
import Ale.Core.Requirements (Proof (..), ReqByteString (..), ReqProof, Reqs (..), Requirements)
import Ale.Core.Submission.Acceptance (facile)
import Ale.Core.Tokens (Tokens)

import qualified Ale.Data.HashMap as HM
import qualified Data.ByteString.Lazy as BSL

-- | Send tokens from one user to another.  This function creates
-- dummy 'JobOffer' with no deadline, no acceptance testing and
-- Contractor Requirements requiring a signature to accept money.
transfer :: SecretKey   -- ^ Secret key of the sender
         -> Tokens      -- ^ Amount of tokens sending now
         -> Entity      -- ^ Recepient
         -> ByteString  -- ^ Bytes that the receiver will have to sign
         -> Signed Message
transfer sk what whom challenge = mkSigned sk . MsgJobOffer $ jobOffer
    Nothing       -- No JobDescription TODO description
    Nothing       -- No deadline
    what          -- Tokens to transfer
    (RVerify (RConst (ReqByteString challenge)) (RConst whom) (RVar 1))
    facile

-- | Receive data sent by preceding 'transfer'
receive :: SecretKey                 -- ^ Receiver's secret key
        -> MessagePointer JobOffer   -- ^ Pointer to offer one tries to claim tokens from
        -> JobOffer                  -- ^ Offer this pointer points to
        -> Either ReceiveError (Signed Message)
receive sk ptr jo = runExcept run
  where
    run :: Except ReceiveError (Signed Message)
    run = envelope <$> satisfy sk (jo^.joRequirements)

    envelope :: ReqProof -> Signed Message
    envelope proof = mkSigned sk $ MsgContractorProposal $
        ContractorProposal ptr proof


-- | Try to satisfy 'Requirements' generated by 'transfer'.
satisfy :: forall m. MonadError ReceiveError m
        => SecretKey
        -> Requirements
        -> m ReqProof
satisfy sk req = case req of
    RVerify (RConst (ReqByteString bs)) (RConst ent) (RVar v) -> do
        when (toPublic sk /= ent) $ throwError CantSign
        pure $ HM.fromList [(v, Proof . BSL.toStrict $ serialise $ sign sk bs)]
    _ -> throwError NotFromTransfer

-- | Datatype for various errors that can occur during 'receive'.
data ReceiveError
    -- | Requested 'Requirements' didn't come from 'transfer'.
    = NotFromTransfer
    -- | We don't have secret key needed.
    | CantSign
    deriving (Show)