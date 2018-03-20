{-# LANGUAGE TypeOperators #-}
module Test.Ale.Node.Rest.Server where

import Universum

import Control.Concurrent.Async (async, uninterruptibleCancel)
import Control.Lens (at, to, (?~))
import Data.Text (isInfixOf)
import Monad.Capabilities (CapImpl (..), Capabilities, CapabilitiesBuilder (..), HasCaps, HasNoCap,
                           addCap, initCaps)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (defaultSettings, openFreePort, runSettingsSocket,
                                 setBeforeMainLoop)
import Servant.Client (BaseUrl (BaseUrl), ClientEnv (ClientEnv), ClientM, Scheme (Http),
                       ServantError, runClientM)

import Hedgehog (Gen, Property, assert, discard, evalEither, failure, forAll, property, success,
                 (===))
import Test.Tasty.HUnit (assertBool, assertFailure)

import Ale.Core.Block (Bundle (..), UnsignedBlock (..), signBlock)
import Ale.Core.Crypto (SecretKey)
import Ale.Core.DHT.Pure (emptyPureDHT, getPureDHT, putPureDHT)
import Ale.Core.DHT.Types (DHT (..), MonadDHT (getDHT, putDHT))
import Ale.Core.Genesis.Block (initialState)
import Ale.Core.Genesis.Data (GenesisData, gdDistribution, gdServerKey)
import Ale.Core.Genesis.Distribution (toAccounts)
import Ale.Fmt ((+||), (||+))
import Ale.Node.Context.Logging (Logging (..))
import Ale.Node.Context.Mining (Mining (..))
import Ale.Node.Context.State (stateDBImpl)
import Ale.Node.DB.Memory (getDBRaw, putDBRaw, withDB)
import Ale.Node.DB.Types (DB (..), DBError (..))
import Ale.Node.Rest.Client (Node (nMessage, nPing, nState, nStorage), NodeMessage (nmPost),
                             NodeState (nsAccount, nsBlockHash, nsCashOf),
                             NodeStorage (postJobDescription), node)
import Ale.Node.Rest.Server (nodeServer)
import Ale.State (AleState (..), getASBlockOrError, updateState)
import Ale.Transactions (transfer)

import Test.Ale.Core.Crypto.Gen (genKeyPair)
import Test.Ale.Core.DHT.Instances ()
import Test.Ale.Core.Entity.Gen (genEntity)
import Test.Ale.Core.Genesis.Data.Gen (genGenesisData)
import Test.Ale.Core.Message.Gen (genJobDescription)
import Test.Ale.Core.Tokens.Gen (genTokens)
import Test.Util (evalMaybe)

import qualified Ale.Core.Tokens as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

unit_apiPing :: IO ()
unit_apiPing = withNodeServer stubCaps $ \env -> do
    pong <- runClientM (nPing node) env
    case pong of
        Left err -> assertFailure $ show err
        Right t  -> assertBool "Unexpected response" ("Ale" `isInfixOf` t)

hprop_apiJobDescription :: Property
hprop_apiJobDescription = property $ do
    (genData, serverSk) <- forAll genGenDataAndSecretKey
    jd <- forAll genJobDescription
    caps <- newPureCaps genData serverSk
    jdPtr <- runSingleRequest caps (postJobDescription (nStorage node) jd) >>= evalEither
    realJD <- usingReaderT (unIOCaps caps) (getDHT jdPtr) >>= evalEither
    jd === realJD

hprop_apiCurrentBlockHash :: Property
hprop_apiCurrentBlockHash = property $ do
    (genData, serverSk) <- forAll genGenDataAndSecretKey
    caps <- newPureCaps genData serverSk
    blockPtr <- runSingleRequest caps (nsBlockHash (nState node)) >>= evalEither
    realBlockPtr <- usingReaderT (unIOCaps caps) getASBlockOrError
    realBlockPtr === blockPtr

hprop_apiAccount :: Property
hprop_apiAccount = property $ do
    (genData, serverSk) <- forAll genGenDataAndSecretKey
    let accs = genData^.gdDistribution.to toAccounts.to keys
    goodAcct <- forAll $ Gen.element accs
    badAcct <- forAll genEntity
    when (badAcct `elem` accs) discard
    caps <- newPureCaps genData serverSk
    mBal <- runSingleRequest caps (nsAccount (nState node) goodAcct) >>= evalEither
    bal <- evalMaybe mBal
    Just bal === genData^.gdDistribution.at goodAcct
    mBad <- runSingleRequest caps (nsAccount (nState node) badAcct) >>= evalEither
    case mBad of
        Nothing -> success
        Just _  -> failure

hprop_apiCashOf :: Property
hprop_apiCashOf = property $ do
    (genData, serverSk) <- forAll genGenDataAndSecretKey
    let accs = genData^.gdDistribution.to toAccounts.to toPairs
    (acct, toks) <- forAll $ Gen.element accs
    tokKind <- forAll $ Gen.element (T.entities toks)
    caps <- newPureCaps genData serverSk
    bal <- runSingleRequest caps (nsCashOf (nState node) acct tokKind) >>= evalEither
    Just bal === (T.countOf tokKind <$> genData^.gdDistribution.at acct)

hprop_apiPostMessage :: Property
hprop_apiPostMessage = property $ do
    (genData', serverSk) <- forAll genGenDataAndSecretKey
    let accs = genData'^.gdDistribution.to toAccounts.to toPairs
    (sk, pk) <- forAll genKeyPair
    bal <- forAll $ genTokens (Range.linear 1 50)
    sendTo <- fst <$> forAll (Gen.element accs)
    let genData = genData' & (gdDistribution.at pk) ?~ bal
    let msg = transfer sk bal sendTo "challenge"
    caps <- newPureCaps genData serverSk
    (curHead', _msgSent', nextHead') <- liftIO $ withNodeServer caps $ \env -> do
        curHead' <- runClientM (nsBlockHash (nState node)) env
        msgSent' <- runClientM (nmPost (nMessage node) msg) env
        nextHead' <- runClientM (nsBlockHash (nState node)) env
        pure (curHead', msgSent', nextHead')
    curHead <- evalEither curHead'
    nextHead <- evalEither nextHead'
    assert (curHead /= nextHead)
    realHead <- usingReaderT (unIOCaps caps) getASBlockOrError
    realHead === nextHead


-----------------------
-- Internal helpers.
-----------------------

-- | Run a test that needs a node REST server.
-- This function will launch a server for you, execute your test
-- and shut the server down when the test exits.
withNodeServer :: forall m a. (MonadIO m, MonadMask m)
               => IOCaps
               -> (ClientEnv -> m a) -> m a
withNodeServer mcaps go = do
    ready <- newEmptyMVar
    (port, sock) <- liftIO openFreePort
    bracket (liftIO $ async $ startRestServer sock ready)
            (liftIO . uninterruptibleCancel) $ \_ -> do
        manager <- liftIO $ newManager defaultManagerSettings
        let baseUrl = BaseUrl Http "localhost" port ""
        () <- takeMVar ready
        go (ClientEnv manager baseUrl)
  where
    startRestServer sock ready =
      let settings = defaultSettings & setBeforeMainLoop (putMVar ready ())
      in runSettingsSocket settings sock (nodeServer $ unIOCaps mcaps)

runSingleRequest :: forall m a. (MonadIO m)
                 => IOCaps
                 -> ClientM a -> m (Either ServantError a)
runSingleRequest caps req = liftIO $ withNodeServer caps $ \env ->
    runClientM req env

---
-- Stub implementations
---
stateStubImpl :: Monad m => CapImpl AleState '[] m
stateStubImpl = CapImpl AleState
    { _getASBlock = error "stub"
    , _putASBlock = error "stub"
    , _getASHeight = error "stub"
    , _putASHeight = error "stub"
    , _getASData = error "stub"
    , _putASData = error "stub"
    }

loggingStubImpl :: Monad m => CapImpl Logging '[] m
loggingStubImpl = CapImpl Logging
    { _dispatchMessage = \_ln _s _t -> pure ()
    , _loggerName = "test"
    }

miningStubImpl :: Monad m => CapImpl Mining '[] m
miningStubImpl = CapImpl Mining
    { _mine = error "not implemented"
    }

storageStubImpl :: Monad m => CapImpl DHT '[] m
storageStubImpl = CapImpl DHT
    { _getDHT = error "not implemented"
    , _putDHT = error "not implemented"
    }

dbStubImpl :: Monad m => CapImpl DB '[] m
dbStubImpl = CapImpl DB
    { _getDBRaw = error "not implemented"
    , _putDBRaw = error "not implemented"
    , _delDBRaw = error "not implemented"
    , _withDBIterator = \_ _ _ -> error "not implemented"
    }

stubCaps :: IOCaps
stubCaps = IOCaps $ addCap miningStubImpl
                  $ addCap stateStubImpl
                  $ addCap dbStubImpl
                  $ addCap storageStubImpl
                  $ addCap loggingStubImpl
                  $ initCaps NoCaps

---
-- Test implementations
---

type NodeCaps = '[Mining, AleState, DB, DHT, Logging]

newtype IOCaps = IOCaps
    { unIOCaps :: forall m. (MonadIO m) => Capabilities NodeCaps m
    }

newtype CapImplIO cap icaps = CapImplIO
    { unCapImplIO :: forall m. MonadIO m => CapImpl cap icaps m }

newPureStorageImpl :: MonadIO m => m (CapImplIO DHT '[])
newPureStorageImpl = do
    storage <- newIORef emptyPureDHT
    pure $ CapImplIO $ CapImpl DHT
        { _getDHT = \ref -> getPureDHT ref <$> readIORef storage
        , _putDHT = \item -> do
            dht <- readIORef storage
            let (dht', res) = putPureDHT item dht
            writeIORef storage dht'
            pure res
        }

withPureDBImpl :: MonadIO m
               => (CapImplIO DB '[] -> m a)
               -> m a
withPureDBImpl cont = withDB $ \db ->
    cont $ CapImplIO $ CapImpl DB
        { _getDBRaw = \pref k -> maybe (Left DBKeyNotFound) Right <$> getDBRaw pref k db
        , _putDBRaw = \pref k i -> putDBRaw pref k i db
        , _delDBRaw = \_pref _k -> error "not implemented"
        , _withDBIterator = \_pref _drop _cont -> error "not implemented"
        }

addCapIO :: (MonadIO m, Typeable cap, HasCaps icaps (cap : caps), HasNoCap cap caps)
         => CapImplIO cap icaps
         -> Capabilities caps m
         -> Capabilities (cap : caps) m
addCapIO = addCap . unCapImplIO

-- | Return a fresh instance of 'Capability'ies with in-memory 'DHT'
-- and 'DB' implementations pre-initialised with given 'GenesisData'.
newPureCaps :: (MonadIO m)
            => GenesisData
            -> SecretKey
            -> m IOCaps
newPureCaps genData serverSk = withPureDBImpl $ \dbImpl -> do
    storageImpl <- newPureStorageImpl
    let stateImpl = CapImplIO stateDBImpl
    let miningImpl :: CapImplIO Mining '[AleState, DHT, DB] = CapImplIO $ CapImpl Mining
            { _mine = \msg -> do
                    prevBlock <- getASBlockOrError
                    bundle <- putDHT $ Bundle [msg]
                    let blk = signBlock serverSk $ UnsignedBlock prevBlock bundle
                    eDiff <- updateState blk (genData^.gdServerKey)
                    case eDiff of
                        Left e  -> error $ "ooops: "+||e||+""
                        Right _ -> pure ()
            }
    let caps = IOCaps $ addCapIO miningImpl
                      $ addCapIO stateImpl
                      $ addCapIO dbImpl
                      $ addCapIO storageImpl
                      $ addCap loggingStubImpl
                      $ initCaps NoCaps
    usingReaderT (unIOCaps caps) $ initialState genData
    pure caps

----------------------------------------
--- Helper functions
----------------------------------------

genGenDataAndSecretKey :: Gen (GenesisData, SecretKey)
genGenDataAndSecretKey = do
    genData <- genGenesisData $ Range.linear 1 10
    (sk, pk) <- genKeyPair
    pure (genData & gdServerKey .~ pk, sk)
