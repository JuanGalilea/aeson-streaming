{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Data.Aeson.Streaming.Conduit (
  sinkParser
, sinkParser'
, ParseError(..)
, yieldElements
, yieldEntries
) where

import Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad
import Data.Aeson (FromJSON)
import Data.Attoparsec.ByteString (IResult(..))
import Control.Exception
import Data.Typeable

import Data.Aeson.Streaming

-- | Run the given `Parser` until it fails or produces a result.
sinkParser :: (Monad m) => Parser p -> ConduitT ByteString x m (Either ParseError p)
sinkParser p = go (parse p)
  where
    go parser =
      await >>= \case
        Just bs | BS.null bs -> go parser
                | otherwise -> step (parser bs)
        Nothing -> step (parser BS.empty)
    step (Done bs' r) =
      do
        unless (BS.null bs') (leftover bs')
        pure $ Right r
    step (Fail x y z) =
      pure . Left $ ParseError x y z
    step (Partial cont) =
      go cont

-- | Run the given `Parser` until it fails or produces a result.
-- Throws a `ParseError` if failure occurs.
sinkParser' :: (MonadThrow m) => Parser p -> ConduitT ByteString x m p
sinkParser' p =
  sinkParser p >>= either throwM pure

-- | Yields the remaining elements of the current compound (array or
-- object) without their indexes or field names.  Returns a parser
-- which will continue consuming input after the end of that compound.
yieldElements :: (MonadThrow m, FromJSON v) => Parser (Element c p) -> ConduitT ByteString v m (NextParser p)
yieldElements = loop
  where
    loop = step <=< sinkParser'
    step (Element _ elementParser) =
      do
        (nextParser, value) <- sinkParser' (decodeValue' elementParser)
        yield value
        loop nextParser
    step (End nextParser) =
      pure nextParser

-- | Yields the remaining elements of the current compound (array or
-- object) with their indexes or field names.  Returns a parser which
-- will continue consuming input after the end of that compound.
yieldEntries :: (MonadThrow m, FromJSON v) => Parser (Element c p) -> ConduitT ByteString (Index c, v) m (NextParser p)
yieldEntries = loop
  where
    loop = step <=< sinkParser'
    step (Element idx elementParser) =
      do
        (nextParser, value) <- sinkParser' (decodeValue' elementParser)
        yield (idx, value)
        loop nextParser
    step (End nextParser) =
      pure nextParser

-- | An attoparsec `Fail`ure as an exception
data ParseError = ParseError ByteString [String] String
                deriving (Show, Typeable)

instance Exception ParseError
