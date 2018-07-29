module Main (main) where

import Test.Tasty (defaultMain)
import Data.Aeson.Streaming.Tests (tests)

main :: IO ()
main = defaultMain tests
