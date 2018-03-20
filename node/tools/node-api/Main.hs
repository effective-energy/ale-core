module Main
       ( main
       ) where

import Universum

import Data.Aeson.Encode.Pretty (encodePretty)

import Ale.Node.Rest.Api (swagger)


main :: IO ()
main = putStrLn $ encodePretty swagger
