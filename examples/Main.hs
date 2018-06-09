{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Conduit
import Data.Conduit.Attoparsec
import Data.Aeson.Streaming
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.ByteString (ByteString)

stringElements :: (MonadThrow m)
               => NextParser ('InArray p)
               -> ConduitT ByteString Text m (NextParser p)
stringElements p =
  sinkParser p >>= \case
    Right (StringResult p' element) ->
      do
        yield element
        stringElements p'
    Left p' ->
      return p'
    _ ->
      fail "found a non-string in the array"

stringArray :: (MonadThrow m)
             => ConduitT ByteString Text m ()
stringArray =
  sinkParser root >>= \case
    ArrayResult np -> stringElements np
    _ -> fail "input wasn't an array"

main :: IO ()
main = runConduit $ stdinC .| stringArray .| mapM_C T.putStrLn

