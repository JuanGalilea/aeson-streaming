{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aeson.Streaming.Examples.Navigate (
  navigate
, navigate'
) where

import Conduit
import Control.Monad.Trans.Maybe
import Data.Aeson (Value)
import Data.Aeson.Streaming

-- Extract the subvalue at the given path, if it exists.  Unlike the
-- other examples, this is just a `Parser`, not a `Conduit` that uses
-- `Parser`s, because it finds a single item and then stops, rather
-- than producing multiple values from the input.
navigate :: [PathComponent] -> Parser (Maybe Value)
navigate path =
  navigateTo path >>= \case
    Right (SomeParseResult r) -> Just . snd <$> parseValue r
    Left _ -> pure Nothing

-- If you wanted to do this tree-walking yourself, the code would look
-- something like this.
navigate' :: [PathComponent] -> Parser (Maybe Value)
navigate' = runMaybeT . start
  where
    start :: [PathComponent] -> MaybeT Parser Value
    start path = step path =<< lift root

    step :: [PathComponent] -> ParseResult p -> MaybeT Parser Value
    step [] result =
      -- At the end of the path.  Parse the value here, ignore the
      -- "next parser", and return it.
      lift (snd <$> parseValue result)
    step (Field field : path) (ObjectResult parser) =
      -- Want an object, found an object.  Find the right field and
      -- step down the path.
      step path =<< lookFor field parser
    step (Offset index : path) (ArrayResult parser) =
      -- Want an array, found an array.  Find the right index and step
      -- down the path.
      step path =<< lookFor index parser
    step _ _ =
      -- We didn't find the type of thing we wanted along the path, so
      -- produce Nothing.
      fail "path mismatch"

    -- The current element is a compound; look for the given target
    -- element, or abort and result in Nothing if it's not there.
    lookFor :: (Eq (Index c)) => Index c -> NextParser ('In c p) -> MaybeT Parser (ParseResult ('In c p))
    lookFor target parser =
      lift (findElement target parser) >>= \case
        Right p -> pure p
        Left _ -> fail "index not found"
