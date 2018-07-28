{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Streaming.Examples.Util (
  renderPath
, valueAsText
, parsePath
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson (Value)
import qualified Data.Aeson.Text as AT
import Data.Aeson.Streaming
import Text.Read (readMaybe)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

renderPath :: [PathComponent] -> Text
renderPath = mconcat . concatMap render
  where
    render (Offset i) = ["[", tshow i, "]"]
    render (Field f) | isIdentifier (T.unpack f) = [".", f]
                     | otherwise = ["["] ++ TL.toChunks (AT.encodeToLazyText f) ++ ["]"]
    isIdentifier "" = False
    isIdentifier (c : cs) = isInitial c && all isContinuation cs
    isInitial c = isAsciiLower c || isAsciiUpper c || c == '_'
    isContinuation c = isInitial c || isDigit c

valueAsText :: Value -> Text
valueAsText = TL.toStrict . AT.encodeToLazyText

parsePathComponent :: String -> PathComponent
parsePathComponent p =
  case readMaybe p of
    Just i -> Offset i
    Nothing -> Field (T.pack p)

parsePath :: [String] -> [PathComponent]
parsePath = map parsePathComponent
