{-# LANGUAGE TemplateHaskell #-}

-- | This module provides pure AleState implementation.
-- Provided implementation doesn't persist data anywhere.

module Ale.State.Pure
       ( AleState
       , asBlock
       , asHeight
       , asData
       , emptyAleState

       , PureAleState
       , runPureAleState
       , evalPureAleState

       , PureAleStateT
       , runPureAleStateT
       , evalPureAleStateT
       ) where

import Universum

import Control.Lens (makeLenses)

import Ale.Core.Block (BlockPointer)
import Ale.Core.DHT.Types (MonadDHT (..), getDHTDefault, putDHTDefault)
import Ale.Core.Height (Height)
import Ale.Fmt (Buildable (..), (+|), (|+))
import Ale.State (AleStateData, MonadAleState (..))

-- | Global state of Ale. Essentially, this is 'AleStateData'
-- plus a pointer to the last applied block.
data AleState = AleState
    { _asBlock  :: BlockPointer
    , _asHeight :: Height
    , _asData   :: AleStateData
    } deriving (Show, Generic)

makeLenses ''AleState

instance Buildable AleState where
    build AleState{..} = "Ale state at block "+|_asBlock|+""

emptyAleState :: AleState
emptyAleState = AleState (error "Tried to read asBlock without initialization")
                         0
                         (error "Tried to read asData without initialization")

-- | A local-only AleState storage.
newtype PureAleStateT m a = PureAleStateT (StateT AleState m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance Monad m => MonadAleState (PureAleStateT m) where
    getASBlock    = PureAleStateT $ gets $ Right . _asBlock
    putASBlock bl = PureAleStateT $ modify (\as -> as {_asBlock = bl})

    getASHeight = PureAleStateT $ gets $ Right . _asHeight
    putASHeight h = PureAleStateT $ modify (\as -> as {_asHeight = h})

    getASData    = PureAleStateT $ gets $ Right . _asData
    putASData dt = PureAleStateT $ modify (\as -> as {_asData = dt})


type PureAleState = PureAleStateT Identity

evalPureAleStateT :: Monad m => PureAleStateT m a -> m a
evalPureAleStateT (PureAleStateT m) = evalStateT m emptyAleState

runPureAleStateT :: Monad m => AleState -> PureAleStateT m a -> m (a, AleState)
runPureAleStateT st (PureAleStateT m) = runStateT m st

-- | Performs a computation that uses AleState and returns a map representing
-- the resulting state of AleState.
runPureAleState :: PureAleState a -> (a, AleState)
runPureAleState = runIdentity . runPureAleStateT emptyAleState

-- | Performs a computation that uses AleState.
evalPureAleState :: PureAleState a -> a
evalPureAleState = runIdentity . evalPureAleStateT


instance (Monad m, MonadDHT m) => MonadDHT (PureAleStateT m) where
    getDHT = getDHTDefault
    putDHT = putDHTDefault
