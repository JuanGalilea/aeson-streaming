{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Aeson.Streaming.Tests.Paths (paths) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Data.Aeson.Streaming

testShow :: TestTree
testShow = testCase "show" $
  ".hello[0][\"there world\"]" @=? show [Field "hello", Offset 0, Field "there world"]

testRead :: TestTree
testRead = testCase "read" $
  [Field "hello", Offset 0, Field "there world"] @=? read ".hello[0][\"there world\"]"

testQQExp :: TestTree
testQQExp = testCase "quasiquoter expression" $
  [Field "hello", Offset 0, Field "there world"] @=? [jpath|.hello[0]["there world"]|]

testQQPat :: TestTree
testQQPat = testCase "quasiquoter pattern" $
  case [Field "hello", Offset 0, Field "there world"] of
    [jpath|.hello[0]["there world"]|] -> pure ()
    _ -> assertFailure "didn't match the path"

genPathComponent :: (MonadGen m) => m PathComponent
genPathComponent = Gen.choice [genOffset, genField]
  where
    genOffset = Offset <$> Gen.int (Range.linear 0 maxBound)
    genField = Field <$> genFieldName
    genFieldName = Gen.choice [ Gen.text (Range.linear 0 100) Gen.alpha
                              , Gen.text (Range.linear 0 100) Gen.ascii
                              , Gen.text (Range.linear 0 100) Gen.unicode ]

testRoundTrip :: TestTree
testRoundTrip = testProperty "read . show == id" $ property $ do
  path <- forAll (Gen.list (Range.linear 0 100) genPathComponent)
  read (show path) === path

testPreDot :: TestTree
testPreDot = testProperty "path starts with a dot" $ property $ do
  path <- forAll (Gen.list (Range.linear 0 100) genPathComponent)
  take 1 (show path) === "."

paths :: TestTree
paths = testGroup "paths" [ testShow
                          , testRead
                          , testRoundTrip
                          , testPreDot
                          , testQQExp
                          , testQQPat
                          ]
