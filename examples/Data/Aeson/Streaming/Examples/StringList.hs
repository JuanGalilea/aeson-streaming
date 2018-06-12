{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Data.Aeson.Streaming.Examples.StringList (
  stringArray
) where

import Conduit
import Data.Conduit.Attoparsec (sinkParser)
import Data.Aeson.Streaming
import Data.ByteString (ByteString)
import Data.Text (Text)

-- Consume an array of strings, yielding each string downstream.
stringArray :: (MonadThrow m)
             => ConduitT ByteString Text m ()
stringArray =
  sinkParser root >>= \case
    ArrayResult np -> stringElements np
    _ -> fail "input wasn't an array"

stringElements :: (MonadThrow m)
               => NextParser ('In 'Array p)
               -> ConduitT ByteString Text m (NextParser p)
stringElements p =
  sinkParser p >>= \case
    Element _ (StringResult p' element) ->
      do
        yield element
        stringElements p'
    End p' ->
      return p'
    _ ->
      fail "found a non-string in the array"

