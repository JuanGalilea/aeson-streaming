module Data.Aeson.Streaming.Tests.Utils (
  parseA
, parseS
, parseP
, parseF
) where

import Data.ByteString.Lazy (ByteString)
import Data.Aeson (decode, Value)
import Data.Attoparsec.ByteString.Lazy (maybeResult, Result(..))

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

parseF :: Parser a -> ByteString -> Either String a
parseF p bs = case parseL p bs of
  Done _ r -> Right r
  Fail _ _ m -> Left m
