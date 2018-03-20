{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Named keys for use in tools.
module Ale.Tools.Keychain
       ( Key (..)
       , asPublic
       , asSecret

       , Keychain (..)
       , publicKeys
       , secretKeys
       , loadKeychain
       , saveKeychain
       , newKeychain

       -- * Parsing
       , ParsableKey (..)
       ) where

import Universum hiding (HashMap, Key)

import Control.Exception.Safe (throwString)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), eitherDecode, object, withObject, (.:),
                   (.=))
import Data.Aeson.Encode.Pretty (Config (confCompare), defConfig, encodePretty')
import Data.Text.Buildable (Buildable (build))
import Fmt ((+|), (|+))
import Options.Applicative (ReadM, maybeReader)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (FilePath)

import Ale.Core.Crypto (PublicKey, SecretKey, publicFromBase64, secretFromBase64, secretToBase64,
                        toPublic)
import Ale.Data.HashMap (HashMap)

import qualified Ale.Data.HashMap as HM
import qualified Data.ByteString.Lazy as BSL


-- | A secret or a public key.
data Key = Sec !SecretKey | Pub !PublicKey
    deriving (Generic)

instance Buildable Key where
    build (Sec sk) = build sk
    build (Pub pk) = build pk

instance ToJSON Key where
    toJSON (Sec sk) = object [ "secret" .= secretToBase64 sk ]
    toJSON (Pub pk) = object [ "public" .= pk ]

instance FromJSON Key where
    parseJSON = withObject "Key" $ \o ->
        Sec <$> o .: "secret" <|> Pub <$> o .: "public"

-- | Turn a key into a public one.
asPublic :: Key -> PublicKey
asPublic (Sec sk) = toPublic sk
asPublic (Pub pk) = pk

-- | Turn a key into a secret one.
asSecret :: Key -> Maybe SecretKey
asSecret (Sec sk) = Just sk
asSecret (Pub _)  = Nothing


-- | A storage for keys with identifiers.
-- This is a file-based storage but it is super lazy, meaning that it will
-- attempt to access the file only when it is really required.
newtype Keychain = Keychain (FilePath, MVar (HashMap Text Key))

-- | Load 'Keychain' from a file.
-- This function is super lazy: it will not even try to open the file
-- until the keychain is really needed.
loadKeychain :: MonadIO m => FilePath -> m Keychain
loadKeychain fpath = Keychain . (fpath, ) <$> newEmptyMVar

-- | Load keychain data from file if not already loaded.
loadKeys :: (MonadIO m, MonadThrow m) => Keychain -> m (HashMap Text Key)
loadKeys (Keychain (fpath, mvar)) = do
    res <- tryTakeMVar mvar
    case res of
       Nothing -> do
           bs <- liftIO $ BSL.readFile fpath
           hm <- case eitherDecode bs of
               Left e  -> throwString e
               Right v -> pure v
           liftIO $ putMVar mvar hm
           pure hm
       Just hm -> pure hm

-- | Get the public keys stored in the keychain.
-- This function returns only the keys that have no secret part,
-- i.e. it doesn’t derive public keys from secret ones.
publicKeys :: (MonadIO m, MonadThrow m) => Keychain -> m (HashMap Text PublicKey)
publicKeys kc = HM.mapMaybe publicOnly <$> loadKeys kc
  where
    publicOnly (Pub pk) = Just pk
    publicOnly (Sec _)  = Nothing

-- | Get the secret keys stored in the keychain.
secretKeys :: (MonadIO m, MonadThrow m) => Keychain -> m (HashMap Text SecretKey)
secretKeys kc = HM.mapMaybe asSecret <$> loadKeys kc

-- | Save 'Keychain' to the file.
saveKeychain :: MonadIO m => Keychain -> m ()
saveKeychain (Keychain (fpath, mvar)) = do
    res <- tryTakeMVar mvar
    case res of
        -- The map was never even loaded so there is no reason to bother.
        Nothing -> pure ()
        -- If we have a map, we always just rewrite the file.
        Just hm -> liftIO $ do
            createDirectoryIfMissing True (takeDirectory fpath)
            BSL.writeFile fpath (encodePretty' (defConfig {confCompare = compare}) hm)

-- | Create a new 'Keychain' at the given path.
-- Keep in mind that if you 'saveKeychain' this new keychain, it will overwrite
-- whatever was in the file.
newKeychain :: MonadIO m => FilePath -> HashMap Text Key -> m Keychain
newKeychain fpath = fmap (Keychain . (fpath, )) . newMVar


-----------------------
-- Parsing and resolving
-----------------------

-- | It is possible to parse named either a public or a secret key.
class ParsableKey a where
    -- | Type of the parsed argument corresponding to the key type.
    data Named a :: *

    -- | Reader for the key.
    -- Feed the result to 'resolveKey'.
    keyReader :: ReadM (Named a)

    -- | Turn a parsed key argument into an actual key.
    resolveKey :: (MonadIO m, MonadThrow m) => Named a -> Keychain -> m a

instance ParsableKey PublicKey where
    data Named PublicKey = PubKeyId Text | PubKeyValue PublicKey

    keyReader = maybeReader $ \case
        '!':val -> fmap PubKeyValue . publicFromBase64 . fromString $ val
        keyId   -> Just . PubKeyId . fromString $ keyId

    resolveKey (PubKeyValue pk) _ = pure pk
    resolveKey (PubKeyId keyId) kc   = loadKeys kc >>= lookupPublic
      where
        lookupPublic :: (MonadThrow m) => HashMap Text Key -> m PublicKey
        lookupPublic m = case HM.lookup keyId m of
            Nothing -> throwString $ "Public key '"+|keyId|+"' not found"
            Just k  -> pure $ asPublic k


instance ParsableKey SecretKey where
    data Named SecretKey = SecKeyId Text | SecKeyValue SecretKey

    keyReader = maybeReader $ \case
        '!':val -> fmap SecKeyValue . secretFromBase64 . fromString $ val
        keyId   -> Just . SecKeyId . fromString $ keyId

    resolveKey (SecKeyValue sk) _ = pure sk
    resolveKey (SecKeyId keyId) kc   = loadKeys kc >>= lookupSecret
      where
        lookupSecret :: (MonadThrow m) => HashMap Text Key -> m SecretKey
        lookupSecret m = case HM.lookup keyId m of
            Nothing -> throwString $ "Secret key '"+|keyId|+"' not found"
            Just k  -> case asSecret k of
                Nothing -> throwString $ "No secret part for key '"+|keyId|+"'"
                Just sk -> pure sk
