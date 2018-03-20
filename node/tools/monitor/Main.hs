module Main
       ( main
       ) where

import Universum

import Data.Default (def)
import System.Wlog (logInfo)

import Ale.Log (setupAleLoggingWithName)
import Ale.Node (foreverBlocks)
import Ale.Node.Startup (Components (..), withNode)
import Ale.Tools.Paths (nodeConfigFile)

main :: IO ()
main = setupAleLoggingWithName "monitor" $ do
    logInfo "Monitor starting..."

    withNode nodeConfigFile def $ \(Components bcc) ->
        foreverBlocks bcc pass
