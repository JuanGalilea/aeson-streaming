module Data.Aeson.Streaming.Tests where

import Test.Tasty

import Data.Aeson.Streaming.Tests.AesonCompatibility

tests :: TestTree
tests = testGroup "aeson-streaming" [ aesonCompatibility
                                    ]
