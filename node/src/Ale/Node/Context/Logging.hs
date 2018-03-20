{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Node logging context.
module Ale.Node.Context.Logging
       ( Logging (..)

       , LoggingImplIO (..)
       , mkLoggingImpl

       , withLogging
       , setupAleLoggingWithName
       ) where

import Universum

import Control.Lens.TH (makeLensesFor)
import Control.Monad.Reader (withReaderT)
import Monad.Capabilities (CapImpl (CapImpl), Capabilities, CapabilitiesBuilder (NoCaps), CapsT,
                           HasCap, HasNoCap, addCap, adjustCap, initCaps, withCap)
import System.Wlog as L (CanLog (..), HasLoggerName (..), LoggerName, Severity, askLoggerName)

import Ale.Capabilities (CapImplIO)
import Ale.Log (setupAleLogging)


-- | Capability required to do logging: current logger name and a function that
-- acutally logs a message.
data Logging m = Logging
    { _dispatchMessage :: LoggerName -> Severity -> Text -> m ()
    , _loggerName      :: LoggerName
    }
makeLensesFor [("_loggerName", "loggerName")] ''Logging


-- | This wrapper is required to preserve polymorphism when returning a polymorphic
-- capability implementation within monadic context.
newtype LoggingImplIO = LoggingImplIO
    { getLoggingImplIO :: forall m. MonadIO m => CapImpl Logging '[] m
    }

-- | Extract logging context from a 'WithLogger' monad and pack it.
mkLoggingImpl :: (MonadIO m, HasLoggerName m) => m LoggingImplIO
mkLoggingImpl = do
    name <- askLoggerName
    pure $ LoggingImplIO $ CapImpl Logging
        { _dispatchMessage = \ln s t -> liftIO $ L.dispatchMessage ln s t
        , _loggerName = name
        }


-- | Setup logging and add it as a capability.
withLogging :: (MonadIO m, HasNoCap Logging caps)
            => LoggerName
            -> (CapImplIO Logging '[] -> CapsT (Logging : caps) m a)
            -> CapsT caps m a
withLogging name cont = do
    setupAleLogging

    let loggingImpl :: CapImplIO Logging '[]
        loggingImpl = CapImpl Logging
            { _dispatchMessage = \ln s t -> liftIO $ L.dispatchMessage ln s t
            , _loggerName = name
            }
    withReaderT (addCap loggingImpl) (cont loggingImpl)

-- | This function work like the original one from "Ale.Log", but it also
-- puts the computation into the 'CapsT' monad.
--
-- It exists solely to simplify the transfer to logging as capability.
setupAleLoggingWithName :: (MonadIO m)
                        => LoggerName
                        -> CapsT '[Logging] m a
                        -> m a
setupAleLoggingWithName name act = usingReaderT (initCaps NoCaps) $
    withLogging name $ \_ -> act
{-# ANN setupAleLoggingWithName ("HLint: ignore Use const" :: Text) #-}
-- ^ For some reason, changing @\_ -> act@ here to @const act@ (which
-- seems legal) introduces some "ambiguous type variable" compilation
-- error.

-- These instances overlap with the ones provided by `log-warper`.
instance {-# OVERLAPPING #-} (Monad m, HasCap Logging caps, m' ~ m) => CanLog (ReaderT (Capabilities caps m') m) where
    dispatchMessage name sev t = withCap $ \cap -> _dispatchMessage cap name sev t

instance {-# OVERLAPPING #-} (Monad m, HasCap Logging caps, m' ~ m) => HasLoggerName (ReaderT (Capabilities caps m') m) where
    askLoggerName = withCap $ \cap -> pure $ _loggerName cap
    modifyLoggerName nameMod = withReaderT (adjustCap $ loggerName %~ nameMod)
