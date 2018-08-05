{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Streaming.Tests.Errors (
  errors
) where

import qualified Data.Attoparsec.Lazy as APL
import Data.ByteString.Lazy (ByteString)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson.Streaming

failIt :: Parser a -> ByteString -> (ByteString, [String], String)
failIt parser json =
  case parseL parser json of
    APL.Done _ _ -> error "should have failed"
    APL.Fail remaining ctx msg -> (remaining, ctx, msg)

context :: ByteString -> (ByteString, [String], String)
context = failIt (parseValue =<< root)

beforeFirstField :: TestTree
beforeFirstField = testCase "before first field" $
  ("", [], "expecting field or end of object: not enough input") @=? context "{"

inField :: TestTree
inField = testCase "in field" $
  ("", [], "expecting field name: not enough input") @=? context "{\"x"

afterFieldNameBeforeColon :: TestTree
afterFieldNameBeforeColon = testCase "after field name but before the colon" $
  ("", ["x"], "expecting colon: not enough input") @=? context "{\"x\""

afterColon :: TestTree
afterColon = testCase "after field name but before the colon" $
  ("", ["x"], "expecting datum: not enough input") @=? context "{\"x\":"

afterFieldBeforeComma :: TestTree
afterFieldBeforeComma = testCase "after a whole field" $
  ("", [], "expecting comma or end of object: not enough input") @=? context "{\"x\":true"

afterObjectComma :: TestTree
afterObjectComma = testCase "after a whole field" $
  ("", [], "expecting field name: not enough input") @=? context "{\"x\":true,"

withinObject :: TestTree
withinObject =
  testGroup "within object"
  [ beforeFirstField
  , inField
  , afterFieldNameBeforeColon
  , afterColon
  , afterFieldBeforeComma
  , afterObjectComma
  ]

beforeFirstElement :: TestTree
beforeFirstElement = testCase "before first element" $
  ("", [], "expecting datum or end of array: not enough input") @=? context "["

afterFirstElementBeforeComma :: TestTree
afterFirstElementBeforeComma = testCase "before first element" $
  ("", [], "expecting comma or end of array: not enough input") @=? context "[0"

afterArrayComma :: TestTree
afterArrayComma = testCase "after a whole item" $
  ("", ["1"], "expecting datum: not enough input") @=? context "[0,"

withinArray :: TestTree
withinArray =
  testGroup "within array"
  [ beforeFirstElement
  , afterFirstElementBeforeComma
  , afterArrayComma
  ]

-- making sure that the context for nested stuff is popped
-- appropriately when finished.
complex :: TestTree
complex = testCase "complex navigation" $ do
  let json = "{ \"field\" : [1, 2, 3], \"another field\" : true }"
      parser = do
        ObjectResult p0 <- root
        Element "field" (ArrayResult p1) <- p0
        Element 0 (NumberResult p2 1) <- p1
        Element 1 (NumberResult p3 2) <- p2
        Element 2 (NumberResult p4 3) <- p3
        End p5 <- p4
        Element "another field" _ <- p5
        fail "ack"
  (" }", ["another field"], "Failed reading: ack") @=? failIt parser json

errors :: TestTree
errors =
  testGroup "errors"
  [ withinObject
  , withinArray
  , complex
  ]
