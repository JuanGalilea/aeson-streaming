{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import System.Environment (getArgs)
import Conduit
import Control.Monad.Trans.Maybe
import Data.Conduit.Attoparsec
import qualified Data.Attoparsec.ByteString as AP
import Data.Aeson (Value)
import qualified Data.Aeson.Text as A
import Data.Aeson.Streaming
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.ByteString (ByteString)
import Data.Semigroup ((<>))
import Text.Read (readMaybe)

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

stringArray :: (MonadThrow m)
             => ConduitT ByteString Text m ()
stringArray =
  sinkParser root >>= \case
    ArrayResult np -> stringElements np
    _ -> fail "input wasn't an array"

tshow :: (Show a) => a -> Text
tshow = T.pack . show

type JsonExplode m a = ConduitT ByteString ([PathComponent], Value) m a
jsonExplode :: forall m. (MonadThrow m)
            => JsonExplode m ()
jsonExplode = sinkParser root >>= step pure []
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

renderPath :: [PathComponent] -> Text
renderPath = mconcat . concatMap render . reverse
  where
    render (Offset i) = ["[", tshow i, "]"]
    render (Field f) | isIdentifier f = [".", f]
                     | otherwise = ["[", tshow f, "]"]
    isIdentifier _ = True

-- Note this keeps the whole of the document as far as it scans in
-- memory, because it's a single parser and Attoparsec doesn't know
-- we're never going to backtrack (and can't, since someone could
-- do "navigate [...] <|> trySomethingElse").
navigate :: [PathComponent] -> AP.Parser (Maybe Value)
navigate = runMaybeT . start
  where
    start :: [PathComponent] -> MaybeT AP.Parser Value
    start path = go path =<< lift root
    go :: [PathComponent] -> ParseResult p -> MaybeT AP.Parser Value
    go [] p = lift (snd <$> parseValue p)
    go (Field f : rest) (ObjectResult p) = go rest =<< findField f p
    go (Offset i : rest) (ArrayResult p) = go rest =<< findField i p
    go _ _ = fail "path mismatch"
    findField :: (Eq (Index c)) => Index c -> NextParser ('In c p) -> MaybeT AP.Parser (ParseResult ('In c p))
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

-- TODO: move these two conduit versions of the skip parsers into a
-- library.
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

parsePathComponent :: String -> PathComponent
parsePathComponent p =
  case readMaybe p of
    Just i -> Offset i
    Nothing -> Field (T.pack p)

parsePath :: [String] -> [PathComponent]
parsePath = map parsePathComponent

asText :: Value -> Text
asText = TL.toStrict . A.encodeToLazyText

main :: IO ()
main =
  getArgs >>= \case
    ["string-list"] -> runConduit $ stdinC .| stringArray
                                           .| mapM_C T.putStrLn
    ["explode"] -> runConduit $ stdinC .| jsonExplode
                                       .| mapM_C (\(p, v) ->
                                                    T.putStrLn $ renderPath p <> " : " <> asText v)
    ("navigate" : path) -> runConduit $ stdinC .| (yield =<< navigate' (parsePath path) {- sinkParser (navigate $ parsePath path) -})
                                               .| mapM_C (T.putStrLn . maybe "--not found--" asText)
    _ -> putStrLn "Unknoown command"
