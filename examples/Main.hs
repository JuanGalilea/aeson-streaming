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

main :: IO ()
main =
  getArgs >>= \case
    ["string-list"] -> runConduit $ stdinC .| stringArray
                                           .| mapM_C T.putStrLn
    ["explode"] -> runConduit $ stdinC .| jsonExplode
                                       .| mapM_C (\(p, v) ->
                                                    T.putStrLn $ renderPath p <> " : " <> tshow v)
    _ -> putStrLn "Unknoown command"
