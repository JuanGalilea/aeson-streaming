module Data.Aeson.Streaming.Tests (tests) where

import Test.Tasty

import Data.Aeson.Streaming.Tests.AesonCompatibility
import Data.Aeson.Streaming.Tests.Navigation

tests :: TestTree
tests = testGroup "aeson-streaming" [ aesonCompatibility
                                    , navigation
                                    ]
