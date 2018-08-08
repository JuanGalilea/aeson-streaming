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
  ("", ["expecting first field or end of object"], "not enough input") @=? context "{"

inFieldName :: TestTree
inFieldName = testCase "in field name" $
  ("", ["expecting first field"], "not enough input") @=? context "{\"x"

afterFieldNameBeforeColon :: TestTree
afterFieldNameBeforeColon = testCase "after field name but before the colon" $
  ("", ["expecting colon after field name \"x\""], "not enough input") @=? context "{\"x\""

afterColon :: TestTree
afterColon = testCase "after the colon" $
  ("", ["\"x\"", "expecting datum"], "not enough input") @=? context "{\"x\":"

afterFieldBeforeComma :: TestTree
afterFieldBeforeComma = testCase "after a whole field" $
  ("", ["expecting comma or end of object after field \"x\""], "not enough input") @=? context "{\"x\":true"

inField :: TestTree
inField = testCase "inside a field" $
  ("", ["\"x\"", "expecting first field or end of object"], "not enough input") @=? context "{\"x\":{"

afterObjectComma :: TestTree
afterObjectComma = testCase "after a whole field and the comma" $
  ("", ["expecting field name after field \"x\""], "not enough input") @=? context "{\"x\":true,"

beforeFirstFieldNested :: TestTree
beforeFirstFieldNested = testCase "before first field nested" $
  ("", ["\"a\"", "expecting first field or end of object"], "not enough input") @=? context "{\"a\":{"

inFieldNameNested :: TestTree
inFieldNameNested = testCase "in field name nested" $
  ("", ["\"a\"", "expecting first field"], "not enough input") @=? context "{\"a\":{\"x"

afterFieldNameBeforeColonNested :: TestTree
afterFieldNameBeforeColonNested = testCase "after field name but before the colon nested" $
  ("", ["\"a\"", "expecting colon after field name \"x\""], "not enough input") @=? context "{\"a\":{\"x\""

afterColonNested :: TestTree
afterColonNested = testCase "after the colon" $
  ("", ["\"a\"", "\"x\"", "expecting datum"], "not enough input") @=? context "{\"a\":{\"x\":"

afterFieldBeforeCommaNested :: TestTree
afterFieldBeforeCommaNested = testCase "after a whole field nested" $
  ("", ["\"a\"", "expecting comma or end of object after field \"x\""], "not enough input") @=? context "{\"a\":{\"x\":true"

inFieldNested :: TestTree
inFieldNested = testCase "after a whole field nested" $
  ("", ["\"a\"", "\"x\"", "expecting first field or end of object"], "not enough input") @=? context "{\"a\":{\"x\":{"

afterObjectCommaNested :: TestTree
afterObjectCommaNested = testCase "after a whole field and the comma nested" $
  ("", ["\"a\"", "expecting field name after field \"x\""], "not enough input") @=? context "{\"a\":{\"x\":true,"

withinObject :: TestTree
withinObject =
  testGroup "within object"
  [ beforeFirstField
  , inFieldName
  , afterFieldNameBeforeColon
  , afterColon
  , afterFieldBeforeComma
  , inField
  , afterObjectComma
  , beforeFirstFieldNested
  , inFieldNameNested
  , afterFieldNameBeforeColonNested
  , afterColonNested
  , afterFieldBeforeCommaNested
  , inFieldNested
  , afterObjectCommaNested
  ]

beforeFirstElement :: TestTree
beforeFirstElement = testCase "before first element" $
  ("", ["expecting first element or end of object"], "not enough input") @=? context "["

afterFirstElementBeforeComma :: TestTree
afterFirstElementBeforeComma = testCase "before first element" $
  ("", ["expecting comma or end of array after element 0"], "not enough input") @=? context "[0"

inFirstElement :: TestTree
inFirstElement = testCase "before first element nested" $
  ("", ["0", "expecting first element or end of object"], "not enough input") @=? context "[["

afterArrayComma :: TestTree
afterArrayComma = testCase "after a whole item" $
  ("", ["1", "expecting datum"], "not enough input") @=? context "[0,"

beforeFirstElementNested :: TestTree
beforeFirstElementNested = testCase "before first element nested" $
  ("", ["\"a\"", "expecting first element or end of object"], "not enough input") @=? context "{\"a\":["

afterFirstElementBeforeCommaNested :: TestTree
afterFirstElementBeforeCommaNested = testCase "before first element nested" $
  ("", ["\"a\"", "expecting comma or end of array after element 0"], "not enough input") @=? context "{\"a\":[0"

inFirstElementNested :: TestTree
inFirstElementNested = testCase "before first element nested" $
  ("", ["\"a\"", "0", "expecting first element or end of object"], "not enough input") @=? context "{\"a\":[["

afterArrayCommaNested :: TestTree
afterArrayCommaNested = testCase "after a whole item nested" $
  ("", ["\"a\"", "1", "expecting datum"], "not enough input") @=? context "{\"a\":[0,"

withinArray :: TestTree
withinArray =
  testGroup "within array"
  [ beforeFirstElement
  , afterFirstElementBeforeComma
  , inFirstElement
  , afterArrayComma
  , beforeFirstElementNested
  , afterFirstElementBeforeCommaNested
  , inFirstElementNested
  , afterArrayCommaNested
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
  (" }", ["expecting comma or end of object after field \"another field\""], "Failed reading: ack") @=? failIt parser json

brokenKeyword :: TestTree
brokenKeyword = testCase "broken keyword" $
  ("trfalse", ["expecting datum"], "string") @=? context "trfalse"

brokenString :: TestTree
brokenString = testCase "broken string" $
  ("", ["expecting string"], "not enough input") @=? context "\"abc"

errors :: TestTree
errors =
  testGroup "errors"
  [ withinObject
  , withinArray
  , complex
  , brokenKeyword
  , brokenString
  ]
