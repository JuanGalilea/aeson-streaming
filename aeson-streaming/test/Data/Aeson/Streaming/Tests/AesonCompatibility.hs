{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Streaming.Tests.AesonCompatibility (
  aesonCompatibility
) where

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString.Lazy as AP
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Data.ByteString.Lazy (ByteString)
import qualified Data.Scientific as Scientific
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

import Data.Aeson.Streaming

parseA :: ByteString -> Maybe Aeson.Value
parseA = Aeson.decode

parseS :: ByteString -> Maybe Aeson.Value
parseS = AP.maybeResult . parseL parser
  where
    parser = do
      ((), result) <- parseValue =<< root
      pure result

objectOrdering :: TestTree
objectOrdering = testCase "Objects with duplicate keys select the same value" $
  let json = "{ \"hello\" : \"there\", \"hello\" : \"world\" }"
  in parseA json @=? parseS json

genValue :: Gen Aeson.Value
genValue = Gen.recursive Gen.choice [ genString
                                    , genNumber
                                    , genBool
                                    , genNull ]
                                    [ genArray
                                    , genObject
                                    ]
  where
    genArray = Aeson.Array . V.fromList <$> Gen.list (Range.linear 0 10) genValue
    genObject = Aeson.Object . HM.fromList <$> Gen.list (Range.linear 0 10) ((,) <$> Gen.text (Range.linear 0 30) Gen.unicode <*> genValue)
    genString = Aeson.String <$> Gen.text (Range.linear 0 1000) Gen.unicode
    genNumber = Aeson.Number . Scientific.fromFloatDigits <$> Gen.double (Range.linearFracFrom 0 (-1e6) 1e6)
    genBool = Aeson.Bool <$> Gen.bool
    genNull = pure Aeson.Null

roundtripsIdentically :: TestTree
roundtripsIdentically = testProperty "Roundtrips identically to aeson" $ property $ do
  v <- Aeson.encode <$> forAll genValue
  parseS v === parseA v

aesonCompatibility :: TestTree
aesonCompatibility = testGroup "aeson compatiblity" [ objectOrdering
                                                    , roundtripsIdentically ]
