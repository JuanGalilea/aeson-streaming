{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Streaming.Tests.Navigation (navigation) where

import Data.Either
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

testArrayInObjectInArrayFailNonexistant :: TestTree
testArrayInObjectInArrayFailNonexistant = testCase "can fail with a path that doesn't exist" $ do
  let json = "[true, false, { \"one\" : false, \"two\" : [ \"a\", \"b\", \"c\", { \"hello\" : \"world\" }, \"d\" ], \"three\" : true }]"
  let pc = fromLeft (error "was successful") $ parseF (navigateTo' [Offset 2, Field "two", Offset 5]) json
  "Failed reading: Didn't find element at .[2].two[5]" @=? pc

testArrayInObjectInArrayFailWrongType :: TestTree
testArrayInObjectInArrayFailWrongType = testCase "can fail with a path that doesn't exist" $ do
  let json = "[true, false, { \"one\" : false, \"two\" : [ \"a\", \"b\", \"c\", { \"hello\" : \"world\" }, \"d\" ], \"three\" : true }]"
  let pc = fromLeft (error "was successful") $ parseF (navigateTo' [Offset 2, Offset 3, Offset 5]) json
  "Failed reading: Didn't find the right kind of element at .[2][3]" @=? pc

navigation :: TestTree
navigation = testGroup "navigation" [ testRoot
                                    , testShallowArray
                                    , testShallowObject
                                    , testArrayInObjectInArray
                                    , testArrayInObjectInArrayFailNonexistant
                                    , testArrayInObjectInArrayFailWrongType
                                    ]
