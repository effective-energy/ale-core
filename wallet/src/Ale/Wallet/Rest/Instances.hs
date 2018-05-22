{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Ale.Wallet.Rest.Instances () where

import Universum

import Data.HashMap.Strict.InsOrd (fromList)
import Data.Swagger (NamedSchema (NamedSchema), SwaggerType (SwaggerObject),
                     ToParamSchema (toParamSchema), ToSchema (declareNamedSchema), declareSchemaRef,
                     properties, required, type_)

import Ale.Core.Crypto (SecretKey)
import Ale.Core.Message (Deadline, JobDescription)
import Ale.Core.Requirements (Requirements)
import Ale.Core.Tokens (TokenCount)
import Ale.Node.Rest.Instances (base64Data)
import Ale.Wallet.FrontendMessage (JobOffer)
import Ale.Wallet.Types (Transaction, WalletInfo, WalletInfoSecret)


----
-- Ale.Core.Crypto
----

-- These instances are dangerous, so we are defining them for the Wallet only.

instance ToParamSchema SecretKey where
    toParamSchema _ = mempty
        & base64Data


----
-- Ale.Wallet.FrontendMessage
----

instance ToSchema JobOffer where
    declareNamedSchema _ = do
        jdSchema <- declareSchemaRef (Proxy :: Proxy JobDescription)
        deadlineSchema <- declareSchemaRef (Proxy :: Proxy Deadline)
        priceSchema <- declareSchemaRef (Proxy :: Proxy TokenCount)
        reqsSchema <- declareSchemaRef (Proxy :: Proxy Requirements)
        pure $ NamedSchema (Just "JobOffer") $ mempty
            & type_ .~ SwaggerObject
            & properties .~ fromList
               [ ("descr" , jdSchema)
               , ("deadline", deadlineSchema)
               , ("price", priceSchema)
               , ("reqs", reqsSchema)
               ]
            & required .~ ["descr", "price", "reqs"]


----
-- Ale.Wallet.Types
----

instance ToSchema WalletInfo

instance ToSchema WalletInfoSecret

instance ToSchema Transaction
