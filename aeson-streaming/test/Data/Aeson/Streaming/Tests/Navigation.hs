{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aeson.Streaming.Tests.Navigation (navigation) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson.Streaming.Tests.Utils
import Data.Aeson.Streaming

testRoot :: TestTree
testRoot = testCase "can find the root" $ do
  let json = "{ \"hello\" : \"world\" }"
  parseA json @=? parseP (navigateTo' []) json

testShallowArray :: TestTree
testShallowArray = testCase "can find an array item" $ do
  let json = "[ false, { \"hello\" : \"world\" }, true ]"
  parseA "{ \"hello\": \"world\" }" @=? parseP (navigateTo' [Offset 1]) json

testShallowObject :: TestTree
testShallowObject = testCase "can find an object field" $ do
  let json = "{ \"one\" : false, \"two\" : { \"hello\" : \"world\" }, \"three\" : true }"
  parseA "{ \"hello\": \"world\" }" @=? parseP (navigateTo' [Field "two"]) json

testArrayInObjectInArray :: TestTree
testArrayInObjectInArray = testCase "can find an array element in an object in an array" $ do
  let json = "[true, false, { \"one\" : false, \"two\" : [ \"a\", \"b\", \"c\", { \"hello\" : \"world\" }, \"d\" ], \"three\" : true }]"
  parseA "{ \"hello\": \"world\" }" @=? parseP (navigateTo' [Offset 2, Field "two", Offset 3]) json

navigation :: TestTree
navigation = testGroup "navigation" [ testRoot
                                    , testShallowArray
                                    , testShallowObject
                                    , testArrayInObjectInArray
                                    ]
