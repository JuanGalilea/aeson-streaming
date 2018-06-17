{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Aeson.Streaming (
  Path(..)
, Parser
, parse
, parseL
, NextParser
, ParseResult(.., AtomicResult)
, Compound(..)
, Element(..)
, Index
, PathComponent(..)
, PathableIndex(..)
, isCompound
, isAtom
, atom
, root
, skipValue
, skipRestOfCompound
, parseValue
, decodeValue
, decodeValue'
, parseRestOfArray
, parseRestOfObject
) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Streaming.Internal as S
import Data.Aeson.Streaming.Internal (Compound(..), Path(..), Index,
                                      PathComponent(..), PathableIndex(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Lazy as APL
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Scientific (Scientific)
import qualified Data.Vector as V

-- | A parser akin to an Attoparsec `AP.Parser`, but without
-- backtracking capabilities, which allows it to discard its input as
-- it is used.
newtype Parser a = Parser (ByteString -> AP.Result a)

-- | Apply a parser to a `ByteString` to start the parsing process.
parse :: Parser a -> ByteString -> AP.Result a
parse (Parser f) = f

-- | Apply a parser to a lazy `BSL.ByteString`.
parseL :: Parser a -> BSL.ByteString -> APL.Result a
parseL p = go (parse p) . BSL.toChunks
  where go f [] = convert [] $ f BS.empty
        go f (bs : bss) = convert bss $ f bs
        convert bss (AP.Done bs r) = APL.Done (prepend bs bss) r
        convert bss (AP.Fail bs a b) = APL.Fail (prepend bs bss) a b
        convert bss (AP.Partial f') = go f' bss
        prepend bs bss = BSL.fromChunks (bs : bss)

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap f . p)

instance Applicative Parser where
  pure a = Parser (flip AP.Done a)
  Parser f <*> Parser r = Parser $ parseLoop result f
    where
      result f' = fmap f' . r

instance Monad Parser where
  (Parser f) >>= next = Parser $ parseLoop (parse . next) f

parseLoop :: (i -> ByteString -> AP.Result r) -> (ByteString -> AP.Result i) -> ByteString -> AP.Result r
parseLoop result = go
  where
    go f bs =
      case f bs of
        AP.Done leftover i | BS.null leftover -> AP.Partial (result i)
                           | otherwise -> result i leftover
        AP.Fail x y z -> AP.Fail x y z
        AP.Partial f' -> AP.Partial (go f')

-- | When parsing nested values, this type indicates whether a new
-- element has been parsed or if the end of the compound has arrived.
data Element (c :: Compound) (p :: Path)
  = Element !(Index c) (ParseResult ('In c p))
  -- ^ There is a new element with the provided index.
  | End (NextParser p)
  -- ^ There are no more elements.  The parser returned will continue
  -- with the container's parent.

-- | When we're done reading an object, whether there is more to parse
-- depends on the current path to the root.  This is a function from
-- the current path to the next parser for reading more values.
type family NextParser (p :: Path) = r | r -> p where
  NextParser 'Root = ()
  NextParser ('In c p) = Parser (Element c p)

-- | One step of parsing can produce either one of the atomic JSON
-- types (null, string, boolean, number) or a parser that can consume
-- one of the compound types (array, object)
data ParseResult (p :: Path)
  = ArrayResult (NextParser ('In 'Array p))
  -- ^ We've seen a @[@ and have a parser for producing the elements
  -- of the array.
  | ObjectResult (NextParser ('In 'Object p))
  -- ^ We've seen a @{@ and have a parser for producing the elements
  -- of the array.
  | NullResult (NextParser p)
  -- ^ We've seen and consumed a @null@
  | StringResult (NextParser p) !Text
  -- ^ We've seen an consumed a string
  | BoolResult (NextParser p) !Bool
  -- ^ We've seen and cosumed a boolean
  | NumberResult (NextParser p) !Scientific
  -- ^ We've seen and consumed a number

-- | True if the result is the start of an array or object, otherwise
-- false.
isCompound :: ParseResult p -> Bool
isCompound (ArrayResult _) = True
isCompound (ObjectResult _) = True
isCompound _ = False

-- | True if the result is an atomic value, otherwise false.
isAtom :: ParseResult p -> Bool
isAtom = not . isCompound

-- | Match an atomic `PatternResult` as a `A.Value`.
pattern AtomicResult :: NextParser p -> A.Value -> ParseResult p
pattern AtomicResult p v <- (atom -> Just (p, v))
{-# COMPLETE AtomicResult, ArrayResult, ObjectResult #-}

-- | Extract an atomic `A.Value` from a `ParseResult` if it is one.
atom :: ParseResult p -> Maybe (NextParser p, A.Value)
atom (NullResult p) = Just (p, A.Null)
atom (StringResult p s) = Just (p, A.String s)
atom (BoolResult p b) = Just (p, A.Bool b)
atom (NumberResult p n) = Just (p, A.Number n)
atom _ = Nothing
{-# INLINE atom #-}

-- | A parser for a top-level value.
root :: Parser (ParseResult 'Root)
root = Parser $ AP.parse (convertResult <$> S.root)

class ParserConverter (p :: Path) where
  convertParser :: S.NextParser p -> NextParser p

instance ParserConverter 'Root where
  convertParser () = ()

instance (ParserConverter p) => ParserConverter ('In c p) where
  convertParser parser = Parser $ AP.parse (convertElement <$> parser)

convertElement :: (ParserConverter p) => S.Element c p -> Element c p
convertElement (S.Element index r) = Element index (convertResult r)
convertElement (S.End p) = End (convertParser p)

convertResult :: (ParserConverter p) => S.ParseResult p -> ParseResult p
convertResult (S.ArrayResult p) = ArrayResult (convertParser p)
convertResult (S.ObjectResult p) = ObjectResult (convertParser p)
convertResult (S.NullResult p) = NullResult (convertParser p)
convertResult (S.NumberResult p n) = NumberResult (convertParser p) n
convertResult (S.StringResult p s) = StringResult (convertParser p) s
convertResult (S.BoolResult p b) = BoolResult (convertParser p) b

-- | Skip the rest of current value.  This is a no-op for atoms, and
-- consumes the rest of the current object or array otherwise.
skipValue :: ParseResult p -> Parser (NextParser p)
skipValue (ArrayResult p) = skipRestOfCompound p
skipValue (ObjectResult p) = skipRestOfCompound p
skipValue (AtomicResult p _) = pure p

-- | Skip the rest of the current array or object.
skipRestOfCompound :: NextParser ('In c p) -> Parser (NextParser p)
skipRestOfCompound = go
  where
    go p =
      p >>= \case
        Element _ r -> go =<< skipValue r
        End n -> pure n

-- | Parse the whole of the current value from the current position.
-- This consumes nothing if the current value is atomic.
parseValue :: ParseResult p -> Parser (NextParser p, A.Value)
parseValue (ObjectResult cont) = fmap A.Object <$> parseRestOfObject cont
parseValue (ArrayResult cont) = fmap A.Array <$> parseRestOfArray cont
parseValue (AtomicResult cont value) = pure (cont, value)

-- | Decode a value via a `A.FromJSON` instance.  Note that this does
-- /not/ use direct decoding, instead converting via `A.Value`.
decodeValue :: (A.FromJSON a) => ParseResult p -> Parser (NextParser p, A.Result a)
decodeValue p = fmap A.fromJSON <$> parseValue p

-- | Decode a value via a `A.FromJSON` instance, failing the parse if
-- the decoding fails.  Note that this does /not/ use direct decoding,
-- instead converting via `A.Value`.
decodeValue' :: (A.FromJSON a) => ParseResult p -> Parser (NextParser p, a)
decodeValue' p =
  decodeValue p >>= \case
    (p', A.Success v) -> pure (p', v)
    (_, A.Error s) -> fail s

parseRestOfCompound :: (Index c -> A.Value -> e) -> ([e] -> r) -> NextParser ('In c p) -> Parser (NextParser p, r)
parseRestOfCompound interest complete p0 = go p0 []
  where
    go p acc =
      p >>= \case
        Element k pr -> do
          (p', v) <- parseValue pr
          go p' (interest k v : acc)
        End p' ->
          pure (p', complete acc)

-- | Parse the rest of the current object into an `A.Object`.
parseRestOfObject :: NextParser ('In 'Object p) -> Parser (NextParser p, A.Object)
parseRestOfObject = parseRestOfCompound (,) HM.fromList

-- | Parse the rest of the current array into an `A.Array`.
parseRestOfArray :: NextParser ('In 'Array p) -> Parser (NextParser p, A.Array)
parseRestOfArray = parseRestOfCompound (\_ v -> v) (V.fromList . reverse)
