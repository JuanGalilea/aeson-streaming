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
import Data.DList (DList)
import qualified Data.DList as DL

type JsonExplode m a = ConduitT ByteString ([PathComponent], Value) m a

-- Convert JSON input to a series of (path, atomic-value) pairs
jsonExplode :: forall m. (MonadThrow m)
            => JsonExplode m ()
jsonExplode = step pure DL.empty =<< sinkParser root
  where
    step :: (NextParser p -> JsonExplode m a) -> DList PathComponent -> ParseResult p -> JsonExplode m a
    step next path result =
      next =<< case result of
                 ArrayResult np -> loopCompound path np
                 ObjectResult np -> loopCompound path np
                 AtomicResult np v -> np <$ yield (DL.toList path, v)
    loopCompound :: (PathableIndex c) => DList PathComponent -> NextParser ('In c p) -> JsonExplode m (NextParser p)
    loopCompound l p =
      sinkParser p >>= \case
        Element idx result -> step (loopCompound l) (DL.snoc l (pathComponent idx)) result
        End np -> pure np
