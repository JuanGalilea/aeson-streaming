{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Data.Aeson.Streaming.Conduit (
  sinkParser
, sinkParser'
, ParseError(..)
, yieldElements
, yieldEntries
, yieldArrayElementsAt
, yieldObjectFieldsAt
) where

import Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad
import Data.Aeson (FromJSON)
import Data.Attoparsec.ByteString (IResult(..))
import Control.Exception
import Data.Typeable
import Data.Text (Text)

import Data.Aeson.Streaming

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications

-- | Run the given `Parser` until it fails or produces a result.
--
-- >>> :{
--   runConduit $
--     yield "{ \"arr\": [1, 2, 3] }" .|
--     sinkParser (parseValue =<< root)
-- :}
-- Right ((),Object (fromList [("arr",Array [Number 1.0,Number 2.0,Number 3.0])]))
--
-- >>> :{
--   runConduit $
--     yield "{ \"arr\": oops this is bad json" .|
--     sinkParser (parseValue =<< root)
-- :}
-- Left (ParseError ...)
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

yieldCompoundContents :: (MonadThrow m, FromJSON v) => (Index c -> v -> a) -> Parser (Element c p) -> ConduitT ByteString a m (NextParser p)
yieldCompoundContents extract = loop
  where
    loop = step <=< sinkParser'
    step (Element idx elementParser) =
      do
        (nextParser, value) <- sinkParser' (decodeValue' elementParser)
        yield $ extract idx value
        loop nextParser
    step (End nextParser) =
      pure nextParser

-- | Yields the remaining elements of the current compound (array or
-- object) without their indexes or field names.  Returns a parser
-- which will continue consuming input after the end of that compound.
-- Throws a `ParseError` if the JSON is malformed or an element is not
-- decodable as the desired type.
yieldElements :: (MonadThrow m, FromJSON v) => Parser (Element c p) -> ConduitT ByteString v m (NextParser p)
yieldElements = yieldCompoundContents (flip const)

-- | Yields the remaining elements of the current compound (array or
-- object) with their indexes or field names.  Returns a parser which
-- will continue consuming input after the end of that compound.
-- Throws a `ParseError` if the JSON is malformed or if an entry is
-- not decodable as the desired type.
yieldEntries :: (MonadThrow m, FromJSON v) => Parser (Element c p) -> ConduitT ByteString (Index c, v) m (NextParser p)
yieldEntries = yieldCompoundContents (,)

-- | Navigate to an array and produce the elements therein.  Throws a
-- `ParseError` if the JSON is malformed, the path does not name an
-- array, or an element is not decodable as the desired type.
--
-- >>> :{
--   runConduit $
--     yield "{ \"arr\": [1, 2, 3] }" .|
--     yieldArrayElementsAt [jpath|.arr|] .|
--     printC @Int
-- :}
-- 1
-- 2
-- 3
yieldArrayElementsAt :: (MonadThrow m, FromJSON v) => [PathComponent] -> ConduitT ByteString v m ()
yieldArrayElementsAt path = do
  SomeArrayParser arrayParser <- sinkParser' $
    navigateTo' path >>= \case
      SomeParseResult (ArrayResult p) -> pure $ SomeArrayParser p
      _ -> fail $ "Expected array at " ++ show path
  void $ yieldElements arrayParser

-- | Navigate to an object and produce the elements therein together
-- with their field names.  Throws a `ParseError` if the JSON is
-- malformed, the path does not name an object, or an element is not
-- decodable as the desired type.
--
-- >>> :{
--   runConduit $
--     yield "{ \"obj\": {\"x\": 1, \"y\": 2} }" .|
--     yieldObjectFieldsAt [jpath|.obj|] .|
--     printC @(Text, Int)
-- :}
-- ("x",1)
-- ("y",2)
yieldObjectFieldsAt :: (MonadThrow m, FromJSON v) => [PathComponent] -> ConduitT ByteString (Text, v) m ()
yieldObjectFieldsAt path = do
  SomeObjectParser objectParser <- sinkParser' $
    navigateTo' path >>= \case
      SomeParseResult (ObjectResult p) -> pure $ SomeObjectParser p
      _ -> fail $ "Expected object at " ++ show path
  void $ yieldEntries objectParser

-- | An attoparsec `Fail`ure as an exception
data ParseError = ParseError ByteString [String] String
                deriving (Show, Typeable)

instance Exception ParseError
