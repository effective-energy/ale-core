module Test.Ale.Node.PingPong where

import Universum

import Test.HUnit (Assertion, assert, assertFailure, (@=?))

import Control.Concurrent (threadDelay)
import Data.Aeson (decode)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.IO (hClose, hFlush, hPutStr, hPutStrLn)
import System.Process (CreateProcess (std_err, std_in, std_out), StdStream (CreatePipe), proc,
                       waitForProcess, withCreateProcess)

import Ale.Node.PingPong.Client (PingStats, psPingsSent)
import Ale.Node.PingPong.Server (PongStats, psPingsReceived)

import qualified Data.ByteString.Lazy as BSL

unit_pingPong :: Assertion
unit_pingPong = assert $
    withCreateProcess (proc' "ale-pong" []) $ \mPongIn mPongOut _err pongPh -> do
        threadDelay 100000 -- Give server some time to initialise.
        pongIn  <- unMaybe mPongIn
        pongOut <- unMaybe mPongOut
        withCreateProcess (proc' "ale-ping" ["-s"]) $ \mPingIn mPingOut _err pingPh -> do

            pingIn  <- unMaybe mPingIn
            pingOut <- unMaybe mPingOut

            hPutStr pingIn msg
            hClose pingIn
            pingStats :: PingStats <- unMaybe =<< decode <$> BSL.hGetContents pingOut

            threadDelay 100000 -- Wait for "network" to propagate packets to
                               -- pong and pong to process them.

            hPutStrLn pongIn "q" -- Exit Pong.
            hFlush pongIn
            pongStats :: PongStats <- unMaybe =<< decode <$> BSL.hGetContents pongOut

            pingCode <- waitForProcess pingPh
            pongCode <- waitForProcess pongPh

            ExitSuccess @=? pingCode
            ExitSuccess @=? pongCode

            length msg @=? pingStats^.psPingsSent
            length msg @=? pongStats^.psPingsReceived
  where
    unMaybe :: Maybe a -> IO a
    unMaybe ma = case ma of
        Nothing -> assertFailure "Just expected"
        Just a  -> pure a

    msg = "lool"

    proc' :: FilePath -> [String] -> CreateProcess
    proc' fp args = (proc fp args)
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
