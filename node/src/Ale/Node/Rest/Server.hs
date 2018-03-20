{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | REST server implementation
module Ale.Node.Rest.Server
       ( nodeServer'
       , nodeServer
       , AsServerWithEnv
       ) where

import Universum

import Control.Lens (at)
import Control.Monad.Except (MonadError (throwError))
import Data.Proxy (Proxy (Proxy))
import Monad.Capabilities (Capabilities, CapsT, HasCaps)
import Network.Wai (Application)
import Servant (Handler (Handler), NoContent (..), ServantErr, err404, errBody, hoistServer, serve)
import Servant.Generic (AsServerT, toServant)
import System.Wlog (logDebug)

import Ale.Core.DHT.Types (DHT, MonadDHT (..))
import Ale.Core.Message (JobDescription)
import Ale.Core.Storage (Reference)
import Ale.Core.Tokens (countOf)
import Ale.Node.Context.Logging (Logging)
import Ale.Node.Context.Mining (Mining, MonadMining (mine))
import Ale.Node.Rest.Api (Api, Node (..), NodeMessage (..), NodeState (..), NodeStorage (..))
import Ale.State (AleState, asdAccounts, asdOpenOffers, asdSubmissionsByProposal, asdTakenOffers,
                  getASBlockOrError, getASDataOrError)

import qualified Ale.State.MessageSet as MS


-- | We use our custom Handler type because we need a 'MonadMask' instance for it.
type Handler' = ExceptT ServantErr IO

type AsServerWithEnv caps = AsServerT (CapsT caps Handler')

-- | @servant-generic@ implementation of the Node REST API.
nodeServer' :: forall caps. HasCaps '[Mining, DHT, AleState, Logging] caps
            => Node (AsServerWithEnv caps)
nodeServer' = Node
    { nPing    = logDebug "test" >> pure "☎ Ale?"
    , nMessage = toServant messageServer
    , nStorage = toServant storageServer
    , nState   = toServant stateServer
    }
  where
    messageServer :: NodeMessage (AsServerWithEnv caps)
    messageServer = NodeMessage {..}
      where
        nmPost msg = mine msg $> NoContent

        nmJobOffer ptr = do
            asd <- getASDataOrError
            let mOpen  = asd^.asdOpenOffers.at ptr
            let mTaken = asd^.asdTakenOffers.at ptr
            maybe (throwError err404 {errBody = "Job offer not found"})
                  (pure . snd)
                  (mOpen <|> mTaken)

    storageServer :: NodeStorage (AsServerWithEnv caps)
    storageServer = NodeStorage {..}
      where
        postJobDescription :: MonadDHT m => JobDescription -> m (Reference JobDescription)
        postJobDescription = putDHT

    stateServer :: NodeState (AsServerWithEnv caps)
    stateServer = NodeState {..}
      where
        nsBlockHash = getASBlockOrError

        nsAccount ent = do
            s <- getASDataOrError
            return $ s^.asdAccounts.at ent

        nsCashOf ent kind =
            maybe 0 (countOf kind) <$> nsAccount ent

        nsOpenOffers = do
            asd <- getASDataOrError
            return . map (\(p, _, _) -> p) . MS.toList $ asd^.asdOpenOffers

        nsProposalSubmissions propPtr = do
            asd <- getASDataOrError
            let mSubs = asd^.asdSubmissionsByProposal.at propPtr
            return $ fromMaybe [] mSubs


-- | Node REST API 'Application'.
nodeServer :: forall caps. HasCaps '[Mining, DHT, AleState, Logging] caps
           => Capabilities caps Handler' -> Application
nodeServer caps = serve (Proxy :: Proxy Api) $ hoistServer
    (Proxy :: Proxy Api) (Handler . usingReaderT caps) (toServant nodeServer')
