{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Aeson.Streaming.Examples.Navigate (
  navigate
, navigate'
) where

import Conduit
import Data.Conduit.Attoparsec (sinkParser)
import Control.Monad.Trans.Maybe
import Data.Attoparsec.ByteString (Parser)
import Data.Aeson (Value)
import Data.Aeson.Streaming
import Data.ByteString (ByteString)

import Data.Aeson.Streaming.Examples.Util.Conduit

-- Note this keeps the whole of the document as far as it scans in
-- memory, because it's a single parser and Attoparsec doesn't know
-- we're never going to backtrack (and can't, since someone could
-- do "navigate [...] <|> trySomethingElse").
navigate :: [PathComponent] -> Parser (Maybe Value)
navigate = runMaybeT . start
  where
    start :: [PathComponent] -> MaybeT Parser Value
    start path = go path =<< lift root
    go :: [PathComponent] -> ParseResult p -> MaybeT Parser Value
    go [] p = lift (snd <$> parseValue p)
    go (Field f : rest) (ObjectResult p) = go rest =<< findField f p
    go (Offset i : rest) (ArrayResult p) = go rest =<< findField i p
    go _ _ = fail "path mismatch"
    findField :: (Eq (Index c)) => Index c -> NextParser ('In c p) -> MaybeT Parser (ParseResult ('In c p))
    findField target p =
      lift p >>= \case
        Element i r | i == target -> return r
                    | otherwise -> findField target =<< lift (skipValue r)
        End _ -> fail "index not found"

-- This version, on the other hand, runs in more-or-less constant
-- memory (the parser will retain runs of whitespace and the contents
-- of strings as it skips).
navigate' :: forall m x. (MonadThrow m)
          => [PathComponent]
          -> ConduitT ByteString x m (Maybe Value)
navigate' = runMaybeT . start
  where
    start :: [PathComponent] -> MaybeT (ConduitT ByteString x m) Value
    start path = go path =<< lift (sinkParser root)
    go :: [PathComponent] -> ParseResult p -> MaybeT (ConduitT ByteString x m) Value
    go [] p = lift (snd <$> sinkParser (parseValue p))
    go (Field f : rest) (ObjectResult p) = go rest =<< findField f p
    go (Offset i : rest) (ArrayResult p) = go rest =<< findField i p
    go _ _ = fail "path mismatch"
    findField :: (Eq (Index c)) => Index c -> NextParser ('In c p) -> MaybeT (ConduitT ByteString x m) (ParseResult ('In c p))
    findField target p =
      lift (sinkParser p) >>= \case
        Element i r | i == target -> return r
                    | otherwise -> findField target =<< lift (skipValue' r)
        End _ -> fail "index not found"
