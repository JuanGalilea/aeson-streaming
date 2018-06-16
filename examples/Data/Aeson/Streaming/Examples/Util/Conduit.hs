{-# LANGUAGE LambdaCase #-}

module Data.Aeson.Streaming.Examples.Util.Conduit (
  sinkParser
, ParseError
) where

import Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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
      await >>= \case
        Just bs ->
          case parser bs of
            Done bs' r -> do
              unless (BS.null bs') (leftover bs')
              pure r
            Fail x y z -> throwM (ParseError x y z)
            Partial cont ->
              go cont
        Nothing ->
          throwM EndOfInput

data ParseError = ParseError ByteString [String] String
                | EndOfInput
                deriving (Show, Typeable)

instance Exception ParseError
