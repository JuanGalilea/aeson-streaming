{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aeson.Streaming.Navigator (
  navigator
) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Read (readMaybe)
import qualified Data.Text as T
import Data.String (fromString)

import Data.Aeson.Streaming.Paths
import Data.Aeson.Streaming

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString.Lazy (Result(..))

-- | A quasiquoter that expands to a function which, given a
-- `ParseResult` to start from, will navigate to a particular location
-- in a tree, preserving the memory of the path.  The type of a
-- `navigator` expression is @`ParseResult` p -> `Parser` (`ParseResult` q)@
-- for some @q@ whose tail is ultimately @p@
--
-- >>> :t [navigator| .foo[0]["hello world"] |]
-- [navigator| .foo[0]["hello world"] |]
--   :: ParseResult p
--      -> Parser (ParseResult ('In 'Object ('In 'Array ('In 'Object p))))
--
-- >>> let Done _ (p', v) = parseL (parseValue =<< [navigator| .foo[0]["hello world"] |] =<< root) "{\"foo\":[{\"hello world\":true}]}"
-- >>> v
-- Bool True
-- >>> :t p'
-- p' :: Parser (Element 'Object ('In 'Array ('In 'Object 'Root)))
navigator :: QuasiQuoter
navigator = QuasiQuoter { quoteExp = convert convertPath
                        , quotePat = const $ fail "invalid pattern"
                        , quoteType = const $ fail "invalid type"
                        , quoteDec = const $ fail "invalid declaration"
                        }
  where
    convert converter = maybe (fail "invalid path") converter . extractPath
    -- we'll allow spaces on the ends for readability
    extractPath = readMaybe . T.unpack . T.strip . T.pack

requireArray :: ParseResult p -> Parser (Element 'Array p)
requireArray (ArrayResult p) = p
requireArray _ = fail "expected array"
{-# INLINE requireArray #-}

requireObject :: ParseResult p -> Parser (Element 'Object p)
requireObject (ObjectResult p) = p
requireObject _ = fail "expected object"
{-# INLINE requireObject #-}

-- Just forces the type to be right even in the empty-path case
finish :: ParseResult p -> Parser (ParseResult p)
finish = pure
{-# INLINE finish #-}

convertPath :: [PathComponent] -> Q Exp
convertPath = foldr step [|finish|]
  where
    step (Offset i) rest = [| requireArray >=> findElement' $(liftData i) >=> $rest |]
    step (Field f) rest = [| requireObject >=> findElement' (fromString $(pure . LitE . StringL $ T.unpack f)) >=> $rest |]

