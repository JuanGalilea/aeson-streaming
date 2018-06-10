{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Streaming.Tests.AesonCompatibility (
  aesonCompatibility
) where

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString as AP
import Test.Tasty
import Test.Tasty.HUnit
import Data.ByteString.Lazy (fromStrict)

import Data.Aeson.Streaming

objectOrdering :: TestTree
objectOrdering = testCase "Objects with duplicate keys select the same value" $
  let json = "{\"hello\":\"there\",\"hello\":\"world\"}"
      valueParser = do
        ((), result) <- parseValue =<< root
        pure result
  in Aeson.eitherDecode (fromStrict json) @=? AP.parseOnly valueParser json

aesonCompatibility :: TestTree
aesonCompatibility = testGroup "aeson compatiblity" [ objectOrdering ]
