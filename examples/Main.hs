{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import System.Environment (getArgs)
import Conduit
import Data.Conduit.Attoparsec
import Data.Aeson (Value)
import Data.Aeson.Streaming
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.ByteString (ByteString)
import Data.Semigroup ((<>))

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

class StringableIndex (c :: Compound) where
  sIdx :: Index c -> Text

instance StringableIndex 'Object where
  sIdx = id

instance StringableIndex 'Array where
  sIdx = tshow

type JsonExplode m a = ConduitT ByteString ([Text], Value) m a
jsonExplode :: forall m. (MonadThrow m)
            => JsonExplode m ()
jsonExplode = sinkParser root >>= step pure []
  where
    step :: (NextParser p -> JsonExplode m a) -> [Text] -> ParseResult p -> JsonExplode m a
    step next path result =
      next =<< case result of
                 ArrayResult np -> loopCompound path np
                 ObjectResult np -> loopCompound path np
                 AtomicResult np v -> np <$ yield (path, v)
    loopCompound :: (StringableIndex c) => [Text] -> NextParser ('In c p) -> JsonExplode m (NextParser p)
    loopCompound l p =
      sinkParser p >>= \case
        Element idx result -> step (loopCompound l) (sIdx idx : l) result
        End np -> pure np

main :: IO ()
main =
  getArgs >>= \case
    ["string-list"] -> runConduit $ stdinC .| stringArray
                                           .| mapM_C T.putStrLn
    ["explode"] -> runConduit $ stdinC .| jsonExplode
                                       .| mapM_C (\(p, v) ->
                                                    T.putStrLn $ T.intercalate "." (reverse p) <> tshow v)
    _ -> putStrLn "Unknoown command"
