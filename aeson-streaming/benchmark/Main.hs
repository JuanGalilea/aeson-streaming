{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import Data.Attoparsec.ByteString.Lazy (Result(..))
import Data.Aeson.Streaming
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Builder as BSL
import Data.List

anObject :: ByteString
anObject = "{\"atomic field\" : true, \"object field\" : { \"smiling\" : \"gnus\", \"are\" : \"happy\" }, \"array field\" : [ 1, 2, 3 ] }"

manyObjects :: Int -> ByteString
manyObjects n = BSL.toLazyByteString . mconcat $ [ "[" ] ++ intersperse "," (replicate n (BSL.lazyByteString anObject)) ++ [ "]" ]

consume :: ByteString -> ()
consume bs =
  case parseL (skipValue =<< root) bs of
    Done "" r -> r
    Done _ _ -> error "Didn't consume the whole thing"
    Fail _ _ msg -> error msg

main :: IO ()
main = defaultMain [ bench "tiny" $ nf consume (manyObjects 1)
                   , bench "small" $ nf consume (manyObjects 100)
                   , bench "large" $ nf consume (manyObjects 10000)
                   , bench "giant" $ nf consume (manyObjects 1000000)
                   ]
