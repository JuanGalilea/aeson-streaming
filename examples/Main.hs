{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import System.Environment (getArgs)
import Conduit
import Control.Monad
import Data.Conduit.Attoparsec
import Data.Aeson (Value)
import Data.Aeson.Streaming
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.ByteString (ByteString)
import Data.Semigroup ((<>))

stringElements :: (MonadThrow m)
               => NextParser ('InArray p)
               -> ConduitT ByteString Text m (NextParser p)
stringElements p =
  sinkParser p >>= \case
    Right (StringResult p' element) ->
      do
        yield element
        stringElements p'
    Left p' ->
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

type JsonExplode m a = ConduitT ByteString ([Text], Value) m a
jsonExplode :: forall m. (MonadThrow m)
            => JsonExplode m ()
jsonExplode = sinkParser root >>= step pure []
  where
    step :: (NextParser p -> JsonExplode m a) -> [Text] -> ParseResult p -> JsonExplode m a
    step next path result =
      next =<< case result of
                 ArrayResult np -> loopArray path 0 np
                 ObjectResult np -> loopObject path np
                 AtomicResult np v -> np <$ yield (path, v)
    loopArray :: [Text] -> Int -> NextParser ('InArray p) -> JsonExplode m (NextParser p)
    loopArray l i =
      either pure (step (loopArray l (i+1)) (tshow i : l)) <=< sinkParser
    loopObject :: [Text] -> NextParser ('InObject p) -> JsonExplode m (NextParser p)
    loopObject l p =
      sinkParser p >>= \case
        Right (field, result) -> step (loopObject l) (field : l) result
        Left np -> pure np

main :: IO ()
main =
  getArgs >>= \case
    ["string-list"] -> runConduit $ stdinC .| stringArray
                                           .| mapM_C T.putStrLn
    ["explode"] -> runConduit $ stdinC .| jsonExplode
                                       .| mapM_C (\(p, v) ->
                                                    T.putStrLn $ T.intercalate "." (reverse p) <> tshow v)
    _ -> putStrLn "Unknoown command"
