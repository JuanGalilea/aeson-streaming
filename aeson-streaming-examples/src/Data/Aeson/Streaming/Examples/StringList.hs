{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Data.Aeson.Streaming.Examples.StringList (
  stringArray
) where

import Conduit
import Data.Aeson.Streaming
import Data.Aeson.Streaming.Conduit (sinkParser')
import Data.ByteString (ByteString)
import Data.Text (Text)

-- Consume an array of strings, yielding each string downstream.
stringArray :: (MonadThrow m)
             => ConduitT ByteString Text m ()
stringArray =
  sinkParser' root >>= \case
    ArrayResult parser -> stringElements parser
    _ -> fail "input wasn't an array"

-- This will yield each string in the array at the current level, and
-- returning a parser for continuing to read after the end of the
-- array.  In the case of being called from `stringArray`, `p` is
-- `Root` and so `NextParser p` is `()`.
stringElements :: (MonadThrow m)
               => NextParser ('In 'Array p)
               -> ConduitT ByteString Text m (NextParser p)
stringElements parser =
  sinkParser' parser >>= \case
    Element _ (StringResult nextParser element) ->
      do
        yield element
        stringElements nextParser
    Element _ _ ->
      fail "found a non-string in the array"
    End nextParser ->
      return nextParser
