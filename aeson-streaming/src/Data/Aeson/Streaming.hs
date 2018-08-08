{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Aeson.Streaming (
  Path(..)
, Parser
, parse
, parseL
, SomeParseResult(..)
, SomeArrayParser(..)
, SomeObjectParser(..)
, NextParser
, ParseResult(.., AtomicResult)
, Compound(..)
, Element(..)
, Index
, PathComponent(..)
, PathableIndex(..)
, atom
, root
, skipValue
, skipRestOfCompound
, parseValue
, decodeValue
, decodeValue'
, parseRestOfArray
, parseRestOfObject
, navigateFromTo
, navigateFromTo'
, navigateTo
, navigateTo'
, findElement
, findElement'
, jpath
) where

import Control.Monad
import qualified Control.Monad.Fail as Fail
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser.Internal as A
import qualified Data.Aeson.Text as A
import Data.Aeson.Streaming.Paths (PathComponent(..), jpath)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Lazy as APL
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Scientific (Scientific)
import qualified Data.Vector as V
import Data.Word (Word8)

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

-- | A parser akin to an Attoparsec `AP.Parser`, but without
-- backtracking capabilities, which allows it to discard its input as
-- it is used.
newtype Parser a = Parser { unParser :: ParseFun a }
                 deriving (Functor)

data State = State { sContext :: String, sPath :: [PathComponent] }

type ParseFun a = State -> ByteString -> AP.Result (State, a)

askPath :: Parser [PathComponent]
askPath = Parser $ \s bs -> AP.Done bs (s, sPath s)

setState :: State -> Parser ()
setState s = Parser $ \_ bs -> AP.Done bs (s, ())

setContext :: String -> Parser ()
setContext ctx = Parser $ \s bs -> AP.Done bs (s { sContext = ctx } , ())

data SomeParseResult = forall p. SomeParseResult (ParseResult p)

-- | A wrapper that allows a function to return a parser positioned
-- within an arbitarily deeply nested array, forgetting the path it
-- took to get there.
data SomeArrayParser = forall p. SomeArrayParser (Parser (Element 'Array p))

-- | A wrapper that allows a function to return a parser positioned
-- within an arbitarily deeply nested object, forgetting the path it
-- took to get there.
data SomeObjectParser = forall p. SomeObjectParser (Parser (Element 'Object p))

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

class PathableIndex (c :: Compound) where
  -- | Promote an index of possibly partially unknown type to a path component
  pathComponent :: Index c -> PathComponent

instance PathableIndex 'Object where
  pathComponent = Field

instance PathableIndex 'Array where
  pathComponent = Offset

initialState :: State
initialState = State "datum" []

-- | Apply a parser to a `ByteString` to start the parsing process.
parse :: Parser a -> ByteString -> AP.Result a
parse (Parser f) = fmap snd <$> f initialState
{-# INLINE parse #-}

-- | Apply a parser to a lazy `BSL.ByteString`.
parseL :: Parser a -> BSL.ByteString -> APL.Result a
parseL p = go (parse p) . BSL.toChunks
  where go f [] = convert [] $ f BS.empty
        go f (bs : bss) = convert bss $ f bs
        convert bss (AP.Done bs r) = APL.Done (prepend bs bss) r
        convert bss (AP.Fail bs a b) = APL.Fail (prepend bs bss) a b
        convert bss (AP.Partial f') = go f' bss
        prepend bs bss = BSL.fromChunks (bs : bss)

instance Applicative Parser where
  pure a = Parser $ \pc bs -> AP.Done bs (pc, a)
  f <*> r = do
    f' <- f
    f' <$> r
  {-# INLINE (<*>) #-}

instance Monad Parser where
  (Parser f) >>= next = Parser $ parseLoop (unParser . next) f
  {-# INLINE (>>=) #-}
  fail = Fail.fail

instance Fail.MonadFail Parser where
  fail err = Parser $ \ctx bs -> AP.Fail bs (contextify ctx) ("Failed reading: " ++ err)

parseLoop :: forall i r. (i -> ParseFun r) -> ParseFun i -> ParseFun r
parseLoop result initial ctx0 bs0 = step (initial ctx0 bs0)
  where
    step :: AP.Result (State, i) -> AP.Result (State, r)
    step (AP.Done leftover (ctx', i))
      | BS.null leftover = AP.Partial (result i ctx')
      | otherwise = result i ctx' leftover
    step (AP.Fail x y z) = AP.Fail x y z
    step (AP.Partial f') = AP.Partial (step . f')
{-# INLINE parseLoop #-}

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

-- | Match an atomic `ParseResult` as a `A.Value`.
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
root = nested ()

nested :: NextParser p -> Parser (ParseResult p)
nested cont = do
  skipSpace
  peekWord8' >>= \case
    DOUBLE_QUOTE -> do
      _ <- anyWord8
      setContext "string"
      StringResult cont <$> runAP A.jstring_
    OPEN_CURLY -> do
      _ <- anyWord8
      path <- askPath
      pure $ ObjectResult (setState State { sContext = "first field or end of object", sPath = path } >> object cont)
    OPEN_SQUARE -> do
      _ <- anyWord8
      path <- askPath
      pure $ ArrayResult (setState State { sContext = "first element or end of object", sPath = path } >> array cont)
    C_f -> BoolResult cont False <$ string "false"
    C_t -> BoolResult cont True <$ string "true"
    C_n -> NullResult cont <$ string "null"
    w | w >= C_0 && w <= C_9 || w == DASH -> do
      setContext "number"
      NumberResult cont <$> runAP A.scientific
    _ -> fail "invalid JSON"

array :: NextParser p -> Parser (Element 'Array p)
array cont = do
  skipSpace
  peekWord8' >>= \case
    CLOSE_SQUARE -> End cont <$ anyWord8
    _ -> do
      oldPath <- askPath
      Element 0 <$> do
        let ctx = "comma or end of array after element 0"
        setState State { sContext = "datum", sPath = Offset 0 : oldPath }
        r <- nested (setState State { sContext = ctx, sPath = oldPath } >> restOfArray 1 cont)
        case r of
          ArrayResult _ -> pure r
          ObjectResult _ -> pure r
          _ -> setState State { sContext = ctx, sPath = oldPath } >> pure r

restOfArray :: Int -> NextParser p -> Parser (Element 'Array p)
restOfArray !i cont = do
  skipSpace
  peekWord8' >>= \case
    CLOSE_SQUARE -> End cont <$ anyWord8
    COMMA -> do
      _ <- anyWord8
      oldPath <- askPath
      Element i <$> do
        let ctx = "comma or end of array after element " ++ show i
        setState State { sContext = "datum", sPath = Offset i : oldPath }
        r <- nested (setState State { sContext = ctx, sPath = oldPath } >> restOfArray (i+1) cont)
        case r of
          ArrayResult _ -> pure r
          ObjectResult _ -> pure r
          _ -> setState State { sContext = ctx, sPath = oldPath } >> pure r
    _ -> fail "expected comma or end of array"

object :: NextParser p -> Parser (Element 'Object p)
object cont = do
  skipSpace
  peekWord8' >>= \case
    CLOSE_CURLY -> End cont <$ anyWord8
    _ -> do
      setContext "first field"
      field cont

restOfObject :: String -> NextParser p -> Parser (Element 'Object p)
restOfObject fStr cont = do
  skipSpace
  peekWord8' >>= \case
    CLOSE_CURLY -> End cont <$ anyWord8
    COMMA -> do
      _ <- anyWord8
      setContext ("field name after field " ++ fStr)
      skipSpace
      field cont
    _ ->
      fail "expected comma or end of object"

field :: NextParser p -> Parser (Element 'Object p)
field cont = do
  !f <- runAP A.jstring
  let fStr = TL.unpack $ A.encodeToLazyText f
  oldPath <- askPath
  setContext $ "colon after field name " ++ fStr
  Element f <$> do
    skipSpace
    _ <- word8 COLON
    let ctx = "comma or end of object after field " ++ fStr
    setState State { sContext = "datum", sPath = Field f : oldPath }
    r <- nested (setState State { sContext = ctx, sPath = oldPath } >> restOfObject fStr cont)
    case r of
      ArrayResult _ -> pure r
      ObjectResult _ -> pure r
      _ -> setState State { sContext = ctx, sPath = oldPath } >> pure r

-- | Skip the rest of current value.  This is a no-op for atoms, and
-- consumes the rest of the current object or array otherwise.
skipValue :: ParseResult p -> Parser (NextParser p)
skipValue (ArrayResult p) = skipRestOfCompound p
skipValue (ObjectResult p) = skipRestOfCompound p
skipValue (AtomicResult p _) = pure p

-- | Skip the rest of the current array or object.
skipRestOfCompound :: Parser (Element c p) -> Parser (NextParser p)
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

-- | Decode a value via a `A.FromJSON` instance.
decodeValue :: (A.FromJSON a) => ParseResult p -> Parser (NextParser p, A.Result a)
decodeValue p = fmap A.fromJSON <$> parseValue p

-- | Decode a value via a `A.FromJSON` instance, failing the parse if
-- the decoding fails.
decodeValue' :: (A.FromJSON a) => ParseResult p -> Parser (NextParser p, a)
decodeValue' p =
  decodeValue p >>= \case
    (p', A.Success v) -> pure (p', v)
    (_, A.Error s) -> fail s

parseRestOfCompound :: (Index c -> A.Value -> e) -> ([e] -> r) -> Parser (Element c p) -> Parser (NextParser p, r)
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
parseRestOfObject :: Parser (Element 'Object p) -> Parser (NextParser p, A.Object)
parseRestOfObject = parseRestOfCompound (,) HM.fromList

-- | Parse the rest of the current array into an `A.Array`.
parseRestOfArray :: Parser (Element 'Array p) -> Parser (NextParser p, A.Array)
parseRestOfArray = parseRestOfCompound (\_ v -> v) (V.fromList . reverse)

-- | Navigate deeper into a tree, forgetting the path taken to get
-- there.  Returns either a ParseResult for the start of the
-- desired item or the path prefix at which the navigation failed
-- together with `Just` the parse result at that point if the failure
-- was due to a component being the wrong type or `Nothing` if it was
-- due to the desired index not being there.
navigateFromTo :: [PathComponent] -> ParseResult p -> Parser (Either ([PathComponent], Maybe SomeParseResult) SomeParseResult)
navigateFromTo = go []
  where
    go :: [PathComponent] -> [PathComponent] -> ParseResult p -> Parser (Either ([PathComponent], Maybe SomeParseResult) SomeParseResult)
    go _ [] r = pure . Right $ SomeParseResult r
    go path (idx@(Offset i) : rest) (ArrayResult p') = continue (idx:path) rest =<< findElement i =<< p'
    go path (idx@(Field f) : rest) (ObjectResult p') = continue (idx:path) rest =<< findElement f =<< p'
    go path (idx : _) r = pure $ Left (reverse (idx : path), Just $ SomeParseResult r)
    continue path _ (Left _) = pure $ Left (reverse path, Nothing)
    continue path rest (Right r) = go path rest r

-- | Like `navigateFromTo` but fails if the result can't be found
navigateFromTo' :: [PathComponent] -> ParseResult p -> Parser SomeParseResult
navigateFromTo' startPath startPoint =
  navigateFromTo startPath startPoint >>= \case
    Right r -> pure r
    Left (pc, Nothing) -> fail $ "Didn't find element at " ++ show pc
    Left (pc, Just _) -> fail $ "Didn't find the right kind of element at " ++ show pc

-- | Navigate to a particular point in the input object, forgetting
-- the path taken to get there.  This is the same as `navigateFromTo`
-- using `root` as the starting point.
navigateTo :: [PathComponent] -> Parser (Either ([PathComponent], Maybe SomeParseResult) SomeParseResult)
navigateTo path = navigateFromTo path =<< root

-- | Like `navigateTo` but fails if the result can't be found
navigateTo' :: [PathComponent] -> Parser SomeParseResult
navigateTo' path = navigateFromTo' path =<< root

-- | Find an element in the current compound item, returning either a
-- parse result for the start of the found item or a parser for
-- continuing to parse after the end of the compound.
findElement :: (Eq (Index c)) => Index c -> Element c p -> Parser (Either (NextParser p) (ParseResult ('In c p)))
findElement i (Element e r)
  | e == i = pure $ Right r
  | otherwise = findElement i =<< join (skipValue r)
findElement _ (End p) = pure (Left p)

-- | Like `findElement` but fails if the result can't be found
findElement' :: (Show (Index c), Eq (Index c)) => Index c -> Element c p -> Parser (ParseResult ('In c p))
findElement' i p =
  findElement i p >>= \case
    Right r -> pure r
    Left _ -> fail $ "Didn't find the element at " ++ show i

-- Fake, non-backtracking attoparsec subset

withInput :: ParseFun a -> ParseFun a
withInput f ctx bs =
  if BS.null bs
  then AP.Fail "" (contextify ctx) "not enough input"
  else f ctx bs

skipSpace :: Parser ()
skipSpace = Parser cont
  where
    go ctx bs =
      let remainder = BS.dropWhile (\w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09) bs
      in if BS.null remainder
         then AP.Partial (cont ctx)
         else AP.Done remainder (ctx, ())
    cont ctx bs =
      if BS.null bs
      then AP.Done bs (ctx, ())
      else go ctx bs

peekWord8' :: Parser Word8
peekWord8' = Parser $ withInput go
  where
    go ctx bs = AP.Done bs (ctx, BS.head bs)

anyWord8 :: Parser Word8
anyWord8 = Parser $ withInput go
  where
    go ctx bs = AP.Done (BS.tail bs) (ctx, BS.head bs)

word8 :: Word8 -> Parser Word8
word8 w = Parser $ withInput go
  where
    go ctx bs =
      let w' = BS.head bs
      in if w' == w
         then AP.Done (BS.tail bs) (ctx, w')
         else AP.Fail bs (contextify ctx) "satisfy"

string :: ByteString -> Parser ByteString
string s = Parser $ withInput go
  where
    go ctx bs =
      if s `BS.isPrefixOf` bs
      then AP.Done (BS.drop (BS.length s) bs) (ctx, s)
      else fixResult ctx $ AP.parse (AP.string s) bs

runAP :: AP.Parser a -> Parser a
runAP p = Parser $ \ctx bs -> fixResult ctx $ AP.parse p bs

fixResult :: State -> AP.Result a -> AP.Result (State, a)
fixResult ctx (AP.Done bs a) = AP.Done bs (ctx, a)
fixResult ctx (AP.Partial f) = AP.Partial (fixResult ctx . f)
fixResult ctx (AP.Fail bs _ msg) = AP.Fail bs (contextify ctx) msg

contextify :: State -> [String]
contextify State{sContext, sPath} = reverse $ ("expecting " ++ sContext) : map toStep sPath
  where
    toStep (Offset i) = show i
    toStep (Field f) = TL.unpack $ A.encodeToLazyText f
