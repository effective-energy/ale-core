{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE StandaloneDeriving      #-}

-- | This module defines Contractor Requirements and tools to work
-- with them.

module Ale.Core.Requirements
       ( ReqByteString(..)
       , Requirements
       , Reqs(..)
       , ReqProof
       , Proof(..)

       , emptyProof

       , validate
       , eval
       ) where

import Universum hiding (HashMap)

import Codec.Serialise as Serialise (Serialise (decode, encode), deserialiseOrFail)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson as Aeson (FromJSON (parseJSON), ToJSON (toJSON), decode, encode)
import Data.Aeson.Types (Parser)
import Data.ByteArray (convert)

import Ale.Core.Crypto (PublicKey, Signature, verify)
import Ale.Core.DHT.Types (MonadDHT (..))
import Ale.Core.Requirements.Internal (ReqByteString (..))
import Ale.Data.HashMap (HashMap)
import Ale.Fmt ((+|), (+||), (|+), (||+))
import Ale.Util.Json (bytesFromJson64, bytesToJson64)

import qualified Ale.Core.Requirements.Internal as RI
import qualified Ale.Data.HashMap as HM
import qualified Data.ByteString.Lazy as BSL

-- | 'AllowedInReqs' restricts what types can be used in
-- 'Requirements'.  Currently, only 'Bool', 'Int', 'ReqByteString',
-- 'PublicKey' and 'Signature' are in use.
class ( Show a, Eq a, Typeable a
      , Serialise a, FromRI a
      , FromJSON a, ToJSON a) => AllowedInReqs a where
    fromConstRI :: RI.Constant -> Parser a

    toConstRI :: ToJSON a => a -> RI.Constant

instance AllowedInReqs Bool where
    toConstRI = RI.RBool

    fromConstRI (RI.RBool b) = pure b
    fromConstRI _            = fail "Bool expected"

instance AllowedInReqs Int where
    toConstRI = RI.RInt

    fromConstRI (RI.RInt i) = pure i
    fromConstRI _           = fail "Int expected"

instance AllowedInReqs PublicKey where
    toConstRI = RI.RPublicKey

    fromConstRI (RI.RPublicKey pk) = pure pk
    fromConstRI _                  = fail "PublicKey expected"

instance AllowedInReqs Signature where
    toConstRI = RI.RSignature

    fromConstRI (RI.RSignature sig) = pure sig
    fromConstRI _                   = fail "Signature expected"

instance AllowedInReqs ReqByteString where
    toConstRI = RI.RByteString

    fromConstRI (RI.RByteString c) = pure c
    fromConstRI _                  = fail "ReqByteString expected"

-- | Alias to represent /real/ contractor 'Requirements'.  All
-- functions that use contractor requirements should use this alias.
type Requirements = Reqs Bool

-- | Contractor requirements language.
data Reqs a where
    -- | Constant value.  See 'AllowedInReqs' to check if particular
    -- type is supported.
    RConst :: AllowedInReqs a => a -> Reqs a
    -- | Variable access.  Variables are passed in 'ReqProof'.
    RVar :: Int -> Reqs a
    -- | List of requirements that should all be satisfied.
    RAnd :: [Reqs Bool] -> Reqs Bool
    -- | Integer comparison.
    RLess :: Reqs Int -> Reqs Int -> Reqs Bool
    -- | @RVerify bs pk sig@ checks if @sig@ corresponds to @bs@
    -- signed with @pk@.
    RVerify :: Reqs ReqByteString
            -> Reqs PublicKey
            -> Reqs Signature
            -> Reqs Bool
    -- | @RSha256@ hashes given input with SHA-256 hash function.
    RSha256 :: Reqs ReqByteString
            -> Reqs ReqByteString
    -- | @REqBs@ compares two given 'ReqByteString's for equality.  It
    -- was initially planned to have polymorphic @REq :: Reqs a ->
    -- Reqs a -> Reqs Bool@ but this caused too many difficulties in
    -- typechecking aka 'fromRI'.
    REqBs :: Reqs ReqByteString -> Reqs ReqByteString -> Reqs Bool

deriving instance Eq a => Eq (Reqs a)
deriving instance Show a => Show (Reqs a)

instance (AllowedInReqs a, FromRI a) => FromJSON (Reqs a) where
    parseJSON v = parseJSON v >>= fromRI

class FromJSON a => FromRI a where
    fromRI :: (FromJSON a, AllowedInReqs a) => RI.Requirements -> Parser (Reqs a)
    fromRI = fromConstOrVarRI

    fromConstOrVarRI :: AllowedInReqs a => RI.Requirements -> Parser (Reqs a)
    fromConstOrVarRI = \case
        RI.RConst c -> RConst <$> fromConstRI c
        RI.RVar idx -> pure $ RVar idx
        x       -> fail $ "Const or Var expected, "+||x||+" found"

    toRI :: FromRI a => Reqs a -> RI.Requirements
    toRI (RConst c)          = RI.RConst $ toConstRI c
    toRI (RVar v)            = RI.RVar v
    toRI (RAnd xs)           = RI.RAnd $ toRI <$> xs
    toRI (RLess lhs rhs)     = RI.RLess (toRI lhs) (toRI rhs)
    toRI (RVerify bs pk sig) = RI.RVerify (toRI bs) (toRI pk) (toRI sig)
    toRI (RSha256 bs)        = RI.RSha256 (toRI bs)
    toRI (REqBs lhs rhs)     = RI.REqBs (toRI lhs) (toRI rhs)

instance FromRI Int
instance FromRI PublicKey
instance FromRI Signature

instance FromRI Bool where
    fromRI = \case
        RI.RAnd xs -> RAnd <$> (fromRI `mapM` xs)
        RI.RVerify bs pk sig -> RVerify <$> fromRI bs <*> fromRI pk <*> fromRI sig
        RI.RLess lhs rhs -> RLess <$> fromRI lhs <*> fromRI rhs
        RI.RConst c -> RConst <$> fromConstRI c
        RI.RVar idx -> pure $ RVar idx
        RI.REqBs lhs rhs -> REqBs <$> fromRI lhs <*> fromRI rhs
        x -> fail $ "Unexpected RI during `Reqs Bool` typecheck: "+||x||+""

instance FromRI ReqByteString where
    fromRI = \case
        RI.RConst c -> RConst <$> fromConstRI c
        RI.RVar idx -> pure $ RVar idx
        RI.RSha256 bs -> RSha256 <$> fromRI bs
        x -> fail $ "Unexpected RI during `Reqs BS` typecheck: "+||x||+""

-- | This instance works for all @a@ except 'ByteString'.
-- 'ByteString' instance is implemented separately.
instance (FromRI a, ToJSON a) => ToJSON (Reqs a) where
    toJSON = toJSON . toRI

-- | We need to implement two different serialisation schemes for
-- 'Reqs': JSON and 'Serialise'.  'Reqs' type is quite complex and
-- implementing same functionality twice is error-prone.  This is the
-- reason why 'Serialise' instance just stores a single 'ByteString'
-- with JSON representation.
instance AllowedInReqs a => Serialise (Reqs a) where
    encode = Serialise.encode . Aeson.encode
    decode = Serialise.decode >>= \bs -> case Aeson.decode bs of
        Nothing -> fail "failed to decode JSON"
        Just ra -> pure ra

-- | A proof that Contractor provides to prove that they satisfy the
-- requirements.  When interpreter meets @RVar i@ it sustitutes it
-- with a value stored under key @i@ in this 'HashMap'.
type ReqProof = HashMap Int Proof

newtype Proof = Proof ByteString
    deriving (Eq, Show, Generic, Serialise)

instance FromJSON Proof where
    parseJSON v = Proof <$> (parseJSON v >>= bytesFromJson64)

instance ToJSON Proof where
    toJSON (Proof v) = bytesToJson64 v

-- | Check if provided proof satisfies the expression.
--
-- The 'MonadDHT' constraint is currently unused but will be needed
-- when we add support for functions that query 'AleState' to 'Requirements'.
validate :: (Monad m, MonadDHT m) => ReqProof -> Requirements -> m Bool
validate p expr = either (const False) identity <$> runExceptT (eval p expr)

-- | Evaluate a 'Reqs' expression to a value.
eval :: (AllowedInReqs a, MonadDHT m, MonadError Text m)
     => ReqProof -> Reqs a -> m a
eval _prf (RConst x) = pure x
eval prf (RVar i) = case HM.lookup i prf of
    Nothing -> throwError $ "Variable "+|i|+" not found in proof"
    Just (Proof v) -> case deserialiseOrFail (BSL.fromStrict v) of
        Left err -> throwError $ "Deserialisation failed: "+||err||+""
        Right a  -> pure a
eval prf (RAnd reqs) = allM (eval prf) reqs
eval prf (RLess lhs rhs) = liftA2 (<) (eval prf lhs) (eval prf rhs)
eval prf (RVerify bs' pk' sig') = do
    (RI.ReqByteString bs) <- eval prf bs'
    pk <- eval prf pk'
    sig <- eval prf sig'
    pure $ verify pk bs sig
eval prf (RSha256 bs) = sha256 <$> eval prf bs
  where
    sha256 :: ReqByteString -> ReqByteString
    sha256 bs' = convert (hash bs' :: Digest SHA256)
eval prf (REqBs lhs rhs) = (==) <$> (eval prf lhs) <*> (eval prf rhs)


-- | Empty 'ReqProof'.
emptyProof :: ReqProof
emptyProof = HM.empty

