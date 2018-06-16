{-# LANGUAGE LambdaCase #-}

module Data.Aeson.Streaming.Examples.Util.Conduit (
  sinkParser
, ParseError
) where

import Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Control.Monad
import Data.Attoparsec.ByteString (IResult(..))
import Control.Exception
import Data.Typeable

import Data.Aeson.Streaming

-- TODO: move this module into a library.

sinkParser :: MonadThrow m => Parser p -> ConduitT ByteString x m p
sinkParser p = go (parse p)
  where
    go parser =
      fmap (parser . fromMaybe BS.empty) await >>= \case
        Done bs' r ->
          do
            unless (BS.null bs') (leftover bs')
            pure r
        Fail x y z ->
          throwM (ParseError x y z)
        Partial cont ->
          go cont

data ParseError = ParseError ByteString [String] String
                | EndOfInput
                deriving (Show, Typeable)

instance Exception ParseError
