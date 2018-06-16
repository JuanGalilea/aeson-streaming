{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aeson.Streaming.Examples.Navigate (
  navigate
) where

import Conduit
import Control.Monad.Trans.Maybe
import Data.Aeson (Value)
import Data.Aeson.Streaming

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
