{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Ale.Node.Rest.Instances
       ( base64Data
       , base16Data
       ) where

import Universum hiding (HashMap)

import Control.Lens.Operators ((?~))
import Data.Aeson.Types (ToJSON (toJSON), ToJSONKey, Value (String))
import Data.HashMap.Strict.InsOrd (fromList)
import Data.Swagger (Format, HasFormat (format), HasPattern (pattern), HasType (type_),
                     NamedSchema (NamedSchema), Pattern, SwaggerType (SwaggerObject, SwaggerString),
                     ToParamSchema (toParamSchema), ToSchema (declareNamedSchema), binarySchema,
                     declareSchemaRef, description, enum_, example, properties, required, schema,
                     toSchema)
import Servant.API.ContentTypes (MimeRender, MimeUnrender, OctetStream)

import Ale.Core.Block (BlockPointer)
import Ale.Core.Crypto (PublicKey, SecretKey, Signature)
import Ale.Core.Crypto.Signed (Signed)
import Ale.Core.Height (Height)
import Ale.Core.Message (ContractorProposal, Deadline, EmployerAcceptance, ExtraFieldValue,
                         JobDescription (JobDescription), JobOffer, Message, MessagePointer,
                         Submission, SubmissionData)
import Ale.Core.Requirements (Proof, Reqs, Requirements)
import Ale.Core.Submission.Acceptance (AcceptanceTest)
import Ale.Core.Tokens (TokenCount, TokenKind, Tokens)

import qualified Data.HashMap.Strict as HMS

import qualified Ale.Core.Requirements.Internal as I
import qualified Ale.Core.Storage as S
import qualified Ale.Data.HashMap as HM

-----------------------
-- Data encoding
-----------------------

deriving instance MimeRender   OctetStream JobDescription
deriving instance MimeUnrender OctetStream JobDescription



-----------------------
-- Swagger
-----------------------

-- 'ToParamSchema' is used for types that are passed around in a simple form,
-- e.g. as HTTP parameters. This class basically corresponds to 'HttpApiData'.
--
-- 'ToSchema' is used for more complex objects that are passed in request and
-- response bodies. It corresponds to 'ToJSON'.
--
-- You should take care to make the structures of the instances match.
-- Basically this means that whenever the 'ToJSON' instance was derived, you
-- should derive 'ToSchema' as well, but if 'ToJSON' instance was implemented
-- by hand, you will have to implement 'ToSchema' by hand. Same for 'HttpApiData'
-- with 'ToParamSchema'.

----
-- Ale.Core.Block
----

instance ToSchema BlockPointer where
    declareNamedSchema _ = pure $ NamedSchema (Just "BlockPointer") $ mempty
        & base64Data
        & example ?~ String "60a6dbf961241bff301d30d0cc7d7a811909eb8bc4790ab00544b492285c58ad"

----
-- Ale.Core.Crypto
----

instance ToSchema PublicKey where
    declareNamedSchema _ = pure $ NamedSchema (Just "PublicKey") $ mempty
        & base64Data
        & example ?~ String "WYJsP2L2883QBqEB6zKv4pD4i846QqNNREpudp5VWLY="

instance ToParamSchema PublicKey where
    toParamSchema _ = mempty
        & base64Data

instance ToSchema SecretKey where
    declareNamedSchema _ = pure $ NamedSchema (Just "SecretKey") $ mempty
        & base64Data
        & example ?~ String "JhCC4sZFI/GF3M9EBZGgJ3ACgVeK9kfbGqK0pePcRXk="

instance ToSchema Signature where
    declareNamedSchema _ = pure $ NamedSchema (Just "Signature") $ mempty
        & base64Data
        & example ?~ String "35MiSiEaEiZvRhQvNbpfCVmsi9zYdhzCaMCvPoX/HO4dt4TNDJ8sgNFQEydjeWkWIH7NI6q2G0TWt1NmxPbwCA=="

instance ToSchema a => ToSchema (Signed a)

----
-- Ale.Core.Message
----
instance ToSchema Height
instance ToSchema Message

instance ToSchema (MessagePointer a) where
    declareNamedSchema _ = pure $ NamedSchema (Just "MessagePointer") $ mempty
        & base16Data
        & example ?~ String "60a6dbf961241bff301d30d0cc7d7a811909eb8bc4790ab00544b492285c58ad"

instance ToParamSchema (MessagePointer a) where
    toParamSchema _ = mempty
        & base16Data


instance ToSchema JobOffer where
    declareNamedSchema _ = do
        jdSchema <- declareSchemaRef (Proxy :: Proxy (S.Reference JobDescription))
        deadlineSchema <- declareSchemaRef (Proxy :: Proxy Deadline)
        priceSchema <- declareSchemaRef (Proxy :: Proxy Tokens)
        reqsSchema <- declareSchemaRef (Proxy :: Proxy Requirements)
        accSchema <- declareSchemaRef (Proxy :: Proxy AcceptanceTest)
        extraSchema <- declareSchemaRef (Proxy :: Proxy (HM.HashMap Text ExtraFieldValue))
        pure $ NamedSchema (Just "JobOffer") $ mempty
            & type_ .~ SwaggerObject
            & properties .~ fromList
               [ ("descr" ,    jdSchema)
               , ("deadline", deadlineSchema)
               , ("price", priceSchema)
               , ("reqs", reqsSchema)
               , ("acc", accSchema)
               , ("extra", extraSchema)
               ]
            & required .~ ["price", "reqs", "acc", "extra"]

instance ToSchema JobDescription where
    declareNamedSchema _ = pure $ NamedSchema (Just "JobDescription") binarySchema

instance ToSchema ExtraFieldValue where
    declareNamedSchema _ = pure $ NamedSchema (Just "Extra") $ mempty
        & base64Data

instance ToSchema ContractorProposal

instance ToSchema Submission

instance ToSchema SubmissionData where
    declareNamedSchema _ = pure $ NamedSchema (Just "SubmissionData") $ mempty
        & base64Data

instance ToSchema EmployerAcceptance

----
-- Ale.Core.Requirements
----

instance ToSchema I.ReqByteString where
    declareNamedSchema _ = pure $ NamedSchema (Just "RequirementsConstant") $ mempty
        & base64Data

instance ToSchema I.Requirements

instance ToSchema I.Constant

instance ToSchema (Reqs a) where
    declareNamedSchema _ = do
        ns <- declareNamedSchema (Proxy :: Proxy I.Requirements)
        pure $ ns
            & schema.description ?~
                "`RConstant` – a constant value that can be used in expressions described below.\n\
                \`RVar` – reference to an input parameter.\n\
                \`RLess` – operator `<` for integers.\n\
                \`RAnd` – logical `and` of a list of booleans.\n\
                \`RVerify` – whether the bytes are signed by a valid signature."
instance ToSchema Proof where
    declareNamedSchema _ = pure $ NamedSchema (Just "Proof") $ mempty
        & base64Data

----
-- Ale.Core.Submission.Acceptance
----

instance ToSchema AcceptanceTest

----
-- Ale.Core.Storage
----

instance ToSchema (S.Reference a) where
    declareNamedSchema _ = pure $ NamedSchema (Just "StorageReference") $ mempty
        & base16Data
        & example ?~ String "103b512c203a303a20413b4b483d3e203c353d4f3f"

----
-- Ale.Core.Tokens
----

instance ToSchema TokenKind where
    declareNamedSchema _ = pure $ NamedSchema (Just "TokenKind") $ mempty
        & type_ .~ SwaggerString
        & enum_ ?~ ["$@"]

instance ToParamSchema TokenKind where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & enum_ ?~ ["$@"]
-- TODO: pattern?

instance ToSchema TokenCount where
    declareNamedSchema _ = pure $ NamedSchema (Just "TokenCount") $
        toSchema (Proxy :: Proxy Word64)
            & example ?~ toJSON (10 :: Word64)

instance ToParamSchema TokenCount

instance ToSchema Tokens where
    declareNamedSchema _ = do
        tcSchema <- declareSchemaRef (Proxy :: Proxy TokenCount)
        pure $ NamedSchema (Just "Tokens") $ mempty
            & type_ .~ SwaggerObject
            & properties .~ fromList
                [ ("$@", tcSchema)
                ]


----
-- Ale.Data.HashMap
----

instance (ToJSONKey k, ToSchema k, ToJSON v, ToSchema v) => ToSchema (HM.HashMap k v) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (HMS.HashMap k v))


----------------------
-- Swagger utils
----------------------

base64Data :: (HasPattern s (Maybe Pattern), HasFormat s (Maybe Format), HasType s (SwaggerType t))
           => s -> s
base64Data = set pattern (Just pat)
           . set format (Just "byte")
           . set type_ SwaggerString
  where
    pat = "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$"

base16Data :: (HasPattern s (Maybe Pattern), HasFormat s (Maybe Format), HasType s (SwaggerType t))
           => s -> s
base16Data = set pattern (Just pat)
           . set format (Just "base16")
           . set type_ SwaggerString
  where
    pat = "^[a-z0-9]*$"
