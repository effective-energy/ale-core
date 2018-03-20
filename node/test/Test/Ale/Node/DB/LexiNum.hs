{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Ale.Node.DB.LexiNum where

import Universum

import Test.HUnit (Assertion, (@=?))

import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.List (consume)

import Ale.Node.DB.LexiNum
import Ale.Node.DB.Types (Collection (..), iterDBCollection, putDBCollection)

import Test.Ale.Node.DB.Rocks (runWithTestDB)


col :: Collection Word32 '[LexiInt] Word32
col = Collection
    { cName = "lexi"
    , cToKeys = fromIntegral
    , cToItem = identity
    , cRecollect = flip const
    }

unit_LexiInt :: Assertion
unit_LexiInt = do
    xs <- runWithTestDB $ do
        forM_ [1 .. 600]$ putDBCollection col
        iterDBCollection col >>= \src -> runConduitRes (src .| consume)
    [1 .. 600] @=? xs

col2 :: Collection (Word32, ByteString, (), Word32) '[LexiInt, ByteString, ()] Word32
col2 = Collection
    { cName = "lexi2"
    , cToKeys = \(w32, bs, u, _item) -> (fromIntegral w32, (bs, u))
    , cToItem = \(_w32, _bs, _u, item) -> item
    , cRecollect = \(li, (bs, u)) item -> (fromIntegral $ unLexiInt li, bs, u, item)
    }

unit_LexiIntNested :: Assertion
unit_LexiIntNested = do
    let items = sort [(i, encodeUtf8 (show j :: Text), (), i + j) | i <- [1..10], j <- [1..10]]
    xs <- runWithTestDB $ do
        forM_ items $ putDBCollection col2
        iterDBCollection col2 >>= \src -> runConduitRes (src .| consume)
    items @=? sort xs

