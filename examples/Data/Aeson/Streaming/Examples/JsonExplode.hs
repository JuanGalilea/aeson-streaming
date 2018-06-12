{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Data.Aeson.Streaming.Examples.JsonExplode (
  jsonExplode
) where

import Conduit
import Data.Conduit.Attoparsec (sinkParser)
import Data.Aeson (Value)
import Data.Aeson.Streaming
import Data.ByteString (ByteString)

type JsonExplode m a = ConduitT ByteString ([PathComponent], Value) m a

-- Convert JSON input to a series of (path, atomic-value) pairs
jsonExplode :: forall m. (MonadThrow m)
            => JsonExplode m ()
jsonExplode = step pure [] =<< sinkParser root
  where
    step :: (NextParser p -> JsonExplode m a) -> [PathComponent] -> ParseResult p -> JsonExplode m a
    step next path result =
      next =<< case result of
                 ArrayResult np -> loopCompound path np
                 ObjectResult np -> loopCompound path np
                 AtomicResult np v -> np <$ yield (path, v)
    loopCompound :: (PathableIndex c) => [PathComponent] -> NextParser ('In c p) -> JsonExplode m (NextParser p)
    loopCompound l p =
      sinkParser p >>= \case
        Element idx result -> step (loopCompound l) (pathComponent idx : l) result
        End np -> pure np
