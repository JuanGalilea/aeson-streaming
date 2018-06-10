{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module Data.Aeson.Streaming (
  Path(..)
, NextParser
, ParseResult(.., AtomicResult)
, isCompound
, isAtom
, atom
, root
, skipValue
, skipRestOfArray
, skipRestOfObject
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

-- | JSON values are nested arrays and objects.  This is a type-level
-- list from the current point in parsing to the root.
data Path = Root
          -- ^ At the top level; there will be no more after the first value read.
          | InArray Path
          -- ^ In an array.  It is possible that there will be more values.
          | InObject Path
          -- ^ In an object.  It is possible there will be more key/value pairs.

-- | When we're done reading an object, whether there is more to parse
-- depends on the current path to the root.  This is a function from
-- the current path to the next parser for reading more values.
type family NextParser (p :: Path) = r | r -> p where
  NextParser 'Root = ()
  -- x^ After the root value, there is nothing more to parse.
  NextParser ('InArray p) = Parser (Either (NextParser p) (ParseResult ('InArray p)))
  -- x^ If we're in an array, the next thing is either the end of the
  -- array (Left, containing the parser to continue reading the
  -- enclosing datum, if any) or more data (Right, containing a parser
  -- to continue reading the current array)
  NextParser ('InObject p) = Parser (Either (NextParser p) (Text, ParseResult ('InObject p)))
  -- x^ IF we're in an object, the next thing is either the end of the
  -- object (Left, containing the parser to continue reading the
  -- enclosing datum, if any) or more data (Right, containing a parser
  -- that will produce the next item's key and value)

-- | One step of parsing can produce either one of the atomic JSON
-- types (null, string, boolean, number) or a parser that can consume
-- one of the compound types (array, object)
data ParseResult (p :: Path) where
  -- | We've seen a @[@ and have a parser for producing the elements
  -- of the array.
  ArrayResult :: NextParser ('InArray p) -> ParseResult p
  -- | We've seen a @{@ and have a parser for producing the elements
  -- of the array.
  ObjectResult :: NextParser ('InObject p) -> ParseResult p
  -- | We've seen and consumed a @null@
  NullResult :: NextParser p -> ParseResult p
  -- | We've seen an consumed a string
  StringResult :: NextParser p -> !Text -> ParseResult p
  -- | We've seen and cosumed a boolean
  BoolResult :: NextParser p -> !Bool -> ParseResult p
  -- | We've seen and consumed a number
  NumberResult :: NextParser p -> !Scientific -> ParseResult p

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
skipValue (ArrayResult p) = skipRestOfArray p
skipValue (ObjectResult p) = skipRestOfObject p
skipValue (AtomicResult p _) = pure p

-- | Skip the rest of the current array
skipRestOfArray :: NextParser ('InArray p) -> Parser (NextParser p)
skipRestOfArray = go
  where
    go p =
      p >>= \case
        Left n -> pure n
        Right r -> skipValue r >>= go

-- | Skip the rest of the current object
skipRestOfObject :: NextParser ('InObject d) -> Parser (NextParser d)
skipRestOfObject = go
  where
    go p =
      p >>= \case
        Left n -> pure n
        Right (_, r) -> go =<< skipValue r

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

array :: NextParser p -> Parser (NextParser ('InArray p))
array cont = do
  skipSpace
  AP.peekWord8' >>= \case
    CLOSE_SQUARE -> pure (Left cont) <$ AP.anyWord8
    _ -> pure (Right <$> nested (restOfArray cont))

restOfArray :: NextParser p -> NextParser ('InArray p)
restOfArray cont = do
  skipSpace
  AP.anyWord8 >>= \case
    CLOSE_SQUARE -> pure (Left cont)
    COMMA -> Right <$> nested (restOfArray cont)
    _ -> broken

object :: NextParser p -> Parser (NextParser ('InObject p))
object cont = do
  skipSpace
  AP.peekWord8' >>= \case
    CLOSE_CURLY -> pure (pure (Left cont))
    _ -> pure (Right <$> field cont)

restOfObject :: NextParser p -> NextParser ('InObject p)
restOfObject cont = do
  skipSpace
  AP.anyWord8 >>= \case
    CLOSE_CURLY -> pure (Left cont)
    COMMA -> Right <$> (skipSpace *> field cont)
    _ -> broken

field :: NextParser p -> Parser (Text, ParseResult ('InObject p))
field cont =
  (,) <$> A.jstring <*> (skipSpace *> AP.word8 COLON *> nested (restOfObject cont))

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
parseRestOfObject :: NextParser ('InObject p) -> Parser (NextParser p, A.Object)
parseRestOfObject p0 = go p0 []
  where
    go p acc =
      p >>= \case
        Right (k, pr) -> do
          (p', v) <- parseValue pr
          go p' ((k, v) : acc)
        Left p' ->
          pure (p', HM.fromList acc)

-- | Parse the rest of the current array into an `A.Array`.
parseRestOfArray :: NextParser ('InArray p) -> Parser (NextParser p, A.Array)
parseRestOfArray p0 = go p0 []
  where
    go p acc =
      p >>= \case
        Right pr -> do
          (p', v) <- parseValue pr
          go p' (v : acc)
        Left p' ->
          pure (p', V.fromList $ reverse acc)
