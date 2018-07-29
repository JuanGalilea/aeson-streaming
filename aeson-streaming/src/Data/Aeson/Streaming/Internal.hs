{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module Data.Aeson.Streaming.Internal (
  Path(..)
, NextParser
, ParseResult(..)
, Compound(..)
, Element(..)
, Index
, PathComponent(..)
, PathableIndex(..)
, root
) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.Parser.Internal as A
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.Scientific (Scientific)
import Text.ParserCombinators.ReadP

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
  deriving (Eq, Ord)

instance Show PathComponent where
  showsPrec d v = showParen (d > 10) $ doShow v
    where doShow (Offset i) = showString "Offset " . showsPrec 11 i
          doShow (Field f) = showString "Field " . showsPrec 11 f
  showList = renderPath

-- this renders a jq-style query path.
renderPath :: [PathComponent] -> ShowS
renderPath path suffix = preDot $ foldr render suffix path
  where
    render (Offset i) = ('[':) . shows i . (']':)
    render (Field f) =
      let fs = T.unpack f
      in if isIdentifier fs
         then ('.':) . showString fs
         else ('[':) . showString (TL.unpack $ A.encodeToLazyText f) . (']':)
    preDot cs@('.':_) = cs
    preDot cs = '.' : cs

isIdentifier :: String -> Bool
isIdentifier "" = False
isIdentifier (c : cs) = isInitial c && all isContinuation cs
isInitial, isContinuation :: Char -> Bool
isInitial c = isAsciiLower c || isAsciiUpper c || c == '_'
isContinuation c = isInitial c || isDigit c

instance Read PathComponent where
  readsPrec d r = readParen (d > 10) (\r' -> [(Offset m,t) |
                                              ("Offset",s) <- lex r',
                                               (m,t) <- readsPrec 11  s]) r
                  ++ readParen (d > 10) (\r' -> [(Field m,t) |
                                                 ("Field",s) <- lex r',
                                                 (m,t) <- readsPrec 11  s]) r
  readList = readP_to_S parsePath

parsePath :: ReadP [PathComponent]
parsePath = nonEmpty +++ empty
  where
    nonEmpty = do
      first <- initialComponent
      rest <- many component
      pure (first : rest)
    empty = do
      _ <- char '.'
      pure []
    initialComponent = parseField +++ (char '.' *> parseIndex)
    component = parseField +++ parseIndex
    parseField = do
      _ <- char '.'
      c <- satisfy isInitial
      cs <- munch isContinuation
      pure . Field $ T.pack (c:cs)
    parseIndex = char '[' *> (parseNumericIndex +++ parseStringIndex) <* char ']'
    parseNumericIndex = do
      cs <- munch1 isDigit
      pure . Offset $ read cs
    parseStringIndex = char '"' *> (deJSON . ('"':) . (++"\"") . concat =<< many parseStringChar) <* char '"'
    parseStringChar =
      get >>= \case
        '"' -> pfail
        '\\' -> ('\\':) . (:[]) <$> get
        other -> pure [other]
    deJSON t =
      case A.decode . BSL.fromStrict . T.encodeUtf8 $ T.pack t of
        Just s -> pure $ Field s
        Nothing -> pfail

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
  -- with the container's parent.

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
  = ArrayResult (Parser (Element 'Array p))
  -- ^ We've seen a @[@ and have a parser for producing the elements
  -- of the array.
  | ObjectResult (Parser (Element 'Object p))
  -- ^ We've seen a @{@ and have a parser for producing the elements
  -- of the array.
  | NullResult (NextParser p)
  -- ^ We've seen and consumed a @null@
  | StringResult (NextParser p) Text
  -- ^ We've seen an consumed a string
  | BoolResult (NextParser p) Bool
  -- ^ We've seen and cosumed a boolean
  | NumberResult (NextParser p) Scientific
  -- ^ We've seen and consumed a number

skipSpace :: Parser ()
skipSpace = AP.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
{-# INLINE skipSpace #-}

-- | A parser for a top-level value.
root :: Parser (ParseResult 'Root)
root = nested ()

broken :: Parser a
broken = fail "not a valid json value"

nested :: NextParser p -> Parser (ParseResult p)
nested cont = do
  skipSpace
  AP.peekWord8' >>= \case
    DOUBLE_QUOTE -> AP.anyWord8 *> (StringResult cont <$> A.jstring_)
    OPEN_CURLY -> ObjectResult (object cont) <$ AP.anyWord8
    OPEN_SQUARE -> ArrayResult (array cont) <$ AP.anyWord8
    C_f -> BoolResult cont False <$ AP.string "false"
    C_t -> BoolResult cont True <$ AP.string "true"
    C_n -> NullResult cont <$ AP.string "null"
    w | w >= C_0 && w <= C_9 || w == DASH -> NumberResult cont <$> A.scientific
    _ -> broken

array :: NextParser p -> Parser (Element 'Array p)
array cont = do
  skipSpace
  AP.peekWord8' >>= \case
    CLOSE_SQUARE -> End cont <$ AP.anyWord8
    _ -> Element 0 <$> nested (restOfArray 1 cont)

restOfArray :: Int -> NextParser p -> Parser (Element 'Array p)
restOfArray !i cont = do
  skipSpace
  AP.anyWord8 >>= \case
    CLOSE_SQUARE -> pure (End cont)
    COMMA -> Element i <$> nested (restOfArray (i+1) cont)
    _ -> broken

object :: NextParser p -> Parser (Element 'Object p)
object cont = do
  skipSpace
  AP.peekWord8' >>= \case
    CLOSE_CURLY -> End cont <$ AP.anyWord8
    _ -> field cont

restOfObject :: NextParser p -> Parser (Element 'Object p)
restOfObject cont = do
  skipSpace
  AP.anyWord8 >>= \case
    CLOSE_CURLY -> pure (End cont)
    COMMA -> skipSpace *> field cont
    _ -> broken

field :: NextParser p -> Parser (Element 'Object p)
field cont =
  Element <$> A.jstring <*> (skipSpace *> AP.word8 COLON *> nested (restOfObject cont))
