{-# LANGUAGE LambdaCase #-}

module Data.Aeson.Streaming.Conduit (
  sinkParser
, sinkParser'
, ParseError(..)
) where

import Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad
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

-- | An attoparsec `Fail`ure as an exception
data ParseError = ParseError ByteString [String] String
                deriving (Show, Typeable)

instance Exception ParseError
