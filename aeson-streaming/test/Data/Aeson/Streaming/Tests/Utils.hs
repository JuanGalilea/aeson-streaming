module Data.Aeson.Streaming.Tests.Utils (
  parseA
, parseS
, parseP
) where

import Data.ByteString.Lazy (ByteString)
import Data.Aeson (decode, Value)
import Data.Attoparsec.ByteString.Lazy (maybeResult)

import Data.Aeson.Streaming

parseA :: ByteString -> Maybe Value
parseA = decode

parseS :: ByteString -> Maybe Value
parseS = parseP (SomeParseResult <$> root)

parseP :: Parser SomeParseResult -> ByteString -> Maybe Value
parseP p = maybeResult . parseL parser
  where
    parser = do
      SomeParseResult p' <- p
      (_, result) <- parseValue p'
      pure result
