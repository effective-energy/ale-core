{-# LANGUAGE UndecidableInstances #-}

-- | Internals of Tokens
module Ale.Core.Tokens.Internal
       ( TokenCount ()
       , tokenCount
       , getTokenCount

       , TokenKind (..)

       , Tokens (..)
       , empty
       , fromHashMap
       , fromList
       , singleton
       , countOf
       , entities

       , addOne
       , subOne
       , addMany
       , subMany

       , TokenBalance
       ) where

import Universum hiding (HashMap, empty)

import Ale.Fmt (Buildable (..), genericF)
import Codec.Serialise (Serialise (..))
import Control.Monad.Except (MonadError (..), runExcept)
import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (FromJSONKeyTextParser),
                   ToJSON (..), ToJSONKey (..))
import Data.Aeson.Types (toJSONKeyText)
import Data.Hashable (Hashable)
import Data.Word (Word64)
import Web.HttpApiData

import Ale.Core.Crypto (publicFromBase64, publicToBase64)
import Ale.Core.Entity (Entity)
import Ale.Data.HashMap (HashMap)

import qualified Ale.Data.HashMap as M
import qualified Data.Text as T
import qualified Text.Read as TR
import qualified Text.Show as TS

-- | Data type used to count tokens.
newtype TokenCount = TokenCount { getTokenCount :: Word64 }
    deriving (Eq, Ord, Num, Generic, Serialise, Bounded, Buildable)

instance Show TokenCount where
    showsPrec d (TokenCount tc) = TS.showsPrec d tc

instance Read TokenCount where
    readsPrec d s = map (first TokenCount) $ TR.readsPrec d s

instance ToJSON TokenCount where
    toJSON = toJSON . getTokenCount
    toEncoding = toEncoding . getTokenCount

instance FromJSON TokenCount where
    parseJSON v = parseJSON v <&> TokenCount

instance FromHttpApiData TokenCount where
    parseUrlPiece = maybeToRight "Bad TokenCount" . tokenCountFromText

-- | 'TokenCount' smart constructor.
tokenCount :: Word64 -> TokenCount
tokenCount = TokenCount

tokenCountFromText :: Text -> Maybe TokenCount
tokenCountFromText = readMaybe . toString

data TokenKind = Money
               | Fuel Entity
               | Transferable Entity
    deriving (Eq, Ord, Show, Generic)

instance Hashable TokenKind
instance Serialise TokenKind

instance Buildable TokenKind where
    build = genericF

instance ToJSON TokenKind where
    toJSON = toJSON . tokenKindToText

instance ToJSONKey TokenKind where
    toJSONKey = toJSONKeyText tokenKindToText

instance FromJSON TokenKind where
    parseJSON v = parseJSON v >>= tokenKindFromText

instance FromJSONKey TokenKind where
    fromJSONKey = FromJSONKeyTextParser tokenKindFromText

instance ToHttpApiData TokenKind where
    toUrlPiece = tokenKindToText

instance FromHttpApiData TokenKind where
    parseUrlPiece = maybeToRight "Bad TokenKind" . tokenKindFromText

tokenKindToText :: TokenKind -> Text
tokenKindToText  Money           = "$@"
tokenKindToText (Fuel e)         = "F@" <> publicToBase64 e
tokenKindToText (Transferable e) = "T@" <> publicToBase64 e

tokenKindFromText :: MonadFail m => Text -> m TokenKind
tokenKindFromText t =
    case T.take 2 t of
        "F@" -> Fuel         <$> publicFromBase64 (T.drop 2 t)
        "T@" -> Transferable <$> publicFromBase64 (T.drop 2 t)
        "$@" -> return Money
        _    -> fail "bad TokenKind"

-- | A multiset of tokens, coloured by the issuing entity.
newtype Tokens = Tokens { getTokens :: HashMap TokenKind TokenCount }
    deriving (Eq, Show, Generic, Serialise, ToPairs)

instance ToJSON Tokens where
    toJSON = toJSON . getTokens
    toEncoding = toEncoding . getTokens

instance FromJSON Tokens where
    parseJSON v = parseJSON v <&> M.filter (/= 0) <&> Tokens

instance Buildable Tokens where
    build = genericF

-- | An empty multiset of tokens.
empty :: Tokens
empty = Tokens M.empty

-- | Build a multiset of tokens from a 'HashMap'.
fromHashMap :: HashMap TokenKind TokenCount -> Tokens
fromHashMap = Tokens

-- | Build a multiset of tokens from a list of pairs.
fromList :: [(TokenKind, TokenCount)] -> Tokens
fromList = Tokens . M.fromList . nonZeroOnly

-- | Build a multiset of tokens from one pair.
singleton :: TokenKind -> TokenCount -> Tokens
singleton k v = fromList [(k, v)]

-- | Lookup the count of a specific kind of tokens in a multiset.
countOf :: TokenKind -> Tokens -> TokenCount
countOf ent (Tokens counts) = M.lookupDefault 0 ent counts

-- | List of entities whose tokens there are in the multiset.
entities :: Tokens -> [TokenKind]
entities = keys

nonZeroOnly :: [(TokenKind, TokenCount)] -> [(TokenKind, TokenCount)]
nonZeroOnly = filter ((/= 0) . snd)

-- | Add a number of tokens issued by an entity to a multiset.
addOne :: TokenKind -> TokenCount -> Tokens -> Tokens
addOne ent count tokens@(Tokens counts) =
    case count of
        0 -> tokens
        _ -> Tokens $ M.insertWith (+) ent count counts

-- | Subtract a number of tokens issued by an entity to a multiset.
-- Fails if the value would become negative after subtraction.
-- Peforms deletion if the value would become zero after subtraction.
subOne :: TokenKind -> TokenCount -> Tokens -> Maybe Tokens
subOne ent count (Tokens counts) = do
    prev <- M.lookup ent counts
    case compare count prev of
        LT -> return $ Tokens $ M.adjust (subtract count) ent counts
        EQ -> return $ Tokens $ M.delete ent counts
        GT -> fail "token not found"

-- | Add a multiset set of tokens to a multiset.
--
-- The time complexity is /O(m * log n)/.  It comes from 'HashMap'
-- and is expected to be /O(m)/ in practice.
addMany :: Tokens        -- ^ Tokens to add
        -> TokenBalance  -- ^ Balance to add to
        -> TokenBalance
addMany (Tokens amnt) was = M.foldrWithKey addOne was amnt

-- | Subtract a multiset of tokens from a multiset.
-- The order of the parameters is the same as for 'subtract'.
-- Fails if there are not enough tokens from some entity.
--
-- The time complexity is /O(m * log n)/.  It comes from 'HashMap'
-- and is expected to be /O(m)/ in practice.
subMany :: Tokens        -- ^ Tokens to subtract
        -> TokenBalance  -- ^ Balance to subtract from
        -> Either TokenKind TokenBalance
subMany (Tokens amnt) was = runExcept (M.foldrWithKey go (pure was) amnt)
  where
    go :: MonadError TokenKind m => TokenKind -> TokenCount -> m TokenBalance -> m TokenBalance
    go e tc bal = (subOne e tc <$> bal) >>= maybeToError e

    maybeToError :: MonadError e m => e -> Maybe a -> m a
    maybeToError _ (Just a) = pure a
    maybeToError e Nothing  = throwError e


-- | A balance on an account of an 'Entity'.
type TokenBalance = Tokens
