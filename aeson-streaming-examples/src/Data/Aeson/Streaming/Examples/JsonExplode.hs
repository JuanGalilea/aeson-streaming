{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Data.Aeson.Streaming.Examples.JsonExplode (
  jsonExplode
) where

import Conduit
import Data.Aeson (Value)
import Data.Aeson.Streaming
import Data.Aeson.Streaming.Conduit (sinkParser')
import Data.ByteString (ByteString)
import Data.DList (DList)
import qualified Data.DList as DL

-- An alias because otherwise the internal type annotations are very long
type JsonExplode m a = ConduitT ByteString ([PathComponent], Value) m a

-- Incrementally convert JSON input to a series of (path,
-- atomic-value) pairs.
jsonExplode :: forall m. (MonadThrow m)
            => JsonExplode m ()
jsonExplode = consume DL.empty =<< sinkParser' root
  where
    -- Yield all the leaves of the current value, which is located at
    -- the given path, and return a parser for the next value at the
    -- current level.
    consume :: DList PathComponent -> ParseResult p -> JsonExplode m (NextParser p)
    consume path (ArrayResult parser) = consumeCompound path parser
    consume path (ObjectResult parser) = consumeCompound path parser
    consume path (AtomicResult nextParser value) = -- The current value is a leaf
      do
        yield (DL.toList path, value)
        pure nextParser

    -- Produce all the leaf values from within the current compound,
    -- resulting in a parser to consume the rest of the current
    -- compound's parent.
    consumeCompound :: (PathableIndex c) => DList PathComponent -> NextParser ('In c p) -> JsonExplode m (NextParser p)
    consumeCompound path parser =
      sinkParser' parser >>= \case
        Element idx result ->
          do
            nextParser <- consume (DL.snoc path (pathComponent idx)) result
            consumeCompound path nextParser
        End nextParser ->
          pure nextParser
