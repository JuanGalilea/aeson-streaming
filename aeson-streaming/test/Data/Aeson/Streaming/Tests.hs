module Data.Aeson.Streaming.Tests (tests) where

import Test.Tasty

import Data.Aeson.Streaming.Tests.AesonCompatibility
import Data.Aeson.Streaming.Tests.Navigation
import Data.Aeson.Streaming.Tests.Paths

tests :: TestTree
tests = testGroup "aeson-streaming" [ aesonCompatibility
                                    , navigation
                                    , paths
                                    ]
