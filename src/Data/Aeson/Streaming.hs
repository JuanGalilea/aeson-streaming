{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module Data.Aeson.Streaming (
  Path(..)
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
import qualified Data.Aeson.Parser.Internal as A
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Scientific (Scientific)
import qualified Data.Vector as V

#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COLON 58
#define COMMA 44
#define DASH 45
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define C_0 48
#define C_9 57
#define C_f 102
#define C_n 110
#define C_t 116

-- | A data type used as an index for the different types of compounds.
data Compound = Array | Object

-- | JSON values are nested arrays and objects.  This is a type-level
-- list from the current point in parsing to the root.
data Path = Root
          -- ^ The top level
          | In Compound Path
          -- ^ The path to the current nested level

-- | The index type of a JSON compound
type family Index (c :: Compound) = t | t -> c where
   Index 'Array = Int
   Index 'Object = Text

-- | A generic path component
data PathComponent = Offset !Int | Field !Text
  deriving (Show, Read, Eq, Ord)

class PathableIndex (c :: Compound) where
  -- | Promote an index of possibly partially unknown type to a path component
  pathComponent :: Index c -> PathComponent

instance PathableIndex 'Object where
  pathComponent = Field

instance PathableIndex 'Array where
  pathComponent = Offset

-- | When parsing nested values, this type indicates whether a new
-- element has been parsed or if the end of the compound has arrived.
data Element (c :: Compound) (p :: Path)
  = Element !(Index c) (ParseResult ('In c p))
  -- ^ There is a new element with the provided index.
  | End (NextParser p)
  -- ^ There are no more elements.  The parser returned will continue
  -- with the container.

-- | When we're done reading an object, whether there is more to parse
-- depends on the current path to the root.  This is a function from
-- the current path to the next parser for reading more values.
type family NextParser (p :: Path) = r | r -> p where
  NextParser 'Root = ()
  -- x^ After the root value, there is nothing more to parse.
  NextParser ('In c p) = Parser (Element c p)
  -- x^ After seeing the start of a compound, elements of that
  -- compound can be parsed.

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

skipSpace :: Parser ()
skipSpace = AP.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
{-# INLINE skipSpace #-}

-- | A parser for a top-level value.
root :: Parser (ParseResult 'Root)
root = nested ()

-- | Skip the rest of current value.  This is a no-op for atoms, and
-- consumes the rest of the current object or array otherwise.
skipValue :: ParseResult p -> Parser (NextParser p)
skipValue (ArrayResult p) = skipRestOfCompound p
skipValue (ObjectResult p) = skipRestOfCompound p
skipValue (AtomicResult p _) = pure p

-- | Skip the rest of the current array
skipRestOfCompound :: NextParser ('In c p) -> Parser (NextParser p)
skipRestOfCompound = go
  where
    go p =
      p >>= \case
        End n -> pure n
        Element _ r -> skipValue r >>= go

broken :: Parser a
broken = fail "not a valid json value"

nested :: NextParser p -> Parser (ParseResult p)
nested cont = do
  skipSpace
  AP.peekWord8' >>= \case
    DOUBLE_QUOTE -> AP.anyWord8 *> (StringResult cont <$> A.jstring_)
    OPEN_CURLY -> AP.anyWord8 *> (ObjectResult <$> object cont)
    OPEN_SQUARE -> AP.anyWord8 *> (ArrayResult <$> array cont)
    C_f -> BoolResult cont False <$ AP.string "false"
    C_t -> BoolResult cont True <$ AP.string "true"
    C_n -> NullResult cont <$ AP.string "null"
    w | w >= C_0 && w <= C_9 || w == DASH -> NumberResult cont <$> A.scientific
    _ -> broken

array :: NextParser p -> Parser (NextParser ('In 'Array p))
array cont = do
  skipSpace
  AP.peekWord8' >>= \case
    CLOSE_SQUARE -> pure (End cont) <$ AP.anyWord8
    _ -> pure (Element 0 <$> nested (restOfArray 1 cont))

restOfArray :: Int -> NextParser p -> NextParser ('In 'Array p)
restOfArray !i cont = do
  skipSpace
  AP.anyWord8 >>= \case
    CLOSE_SQUARE -> pure (End cont)
    COMMA -> Element i <$> nested (restOfArray (i+1) cont)
    _ -> broken

object :: NextParser p -> Parser (NextParser ('In 'Object p))
object cont = do
  skipSpace
  AP.peekWord8' >>= \case
    CLOSE_CURLY -> pure (pure (End cont))
    _ -> pure (field cont)

restOfObject :: NextParser p -> NextParser ('In 'Object p)
restOfObject cont = do
  skipSpace
  AP.anyWord8 >>= \case
    CLOSE_CURLY -> pure (End cont)
    COMMA -> skipSpace *> field cont
    _ -> broken

field :: NextParser p -> Parser (Element 'Object p)
field cont =
  Element <$> A.jstring <*> (skipSpace *> AP.word8 COLON *> nested (restOfObject cont))

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
    (_, A.Error s) -> fail s
    (p', A.Success v) -> pure (p', v)

-- | Parse the rest of the current object into an `A.Object`.
parseRestOfObject :: NextParser ('In 'Object p) -> Parser (NextParser p, A.Object)
parseRestOfObject p0 = go p0 []
  where
    go p acc =
      p >>= \case
        Element k pr -> do
          (p', v) <- parseValue pr
          go p' ((k, v) : acc)
        End p' ->
          pure (p', HM.fromList acc)

-- | Parse the rest of the current array into an `A.Array`.
parseRestOfArray :: NextParser ('In 'Array p) -> Parser (NextParser p, A.Array)
parseRestOfArray p0 = go p0 []
  where
    go p acc =
      p >>= \case
        Element _ pr -> do
          (p', v) <- parseValue pr
          go p' (v : acc)
        End p' ->
          pure (p', V.fromList $ reverse acc)
