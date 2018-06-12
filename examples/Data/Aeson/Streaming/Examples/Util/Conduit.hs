{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Data.Aeson.Streaming.Examples.Util.Conduit (
  skipRestOfCompound'
, skipValue'
) where

import Conduit
import Data.Conduit.Attoparsec (sinkParser)
import Data.ByteString (ByteString)
import Data.Aeson.Streaming

-- TODO: move this entire module into a library.

skipRestOfCompound' :: (MonadThrow m)
                    => NextParser ('In c p)
                    -> ConduitT ByteString x m (NextParser p)
skipRestOfCompound' = go
  where
    go p =
      sinkParser p >>= \case
        End n -> pure n
        Element _ r -> skipValue' r >>= go

skipValue' :: (MonadThrow m)
           => ParseResult p
           -> ConduitT ByteString x m (NextParser p)
skipValue' (ObjectResult p) = skipRestOfCompound' p
skipValue' (ArrayResult p) = skipRestOfCompound' p
skipValue' (AtomicResult p _) = pure p
