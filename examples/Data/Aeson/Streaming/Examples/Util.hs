{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Streaming.Examples.Util (
  tshow
, renderPath
, valueAsText
, parsePath
, parsePathComponent
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson (Value)
import qualified Data.Aeson.Text as AT
import Data.Aeson.Streaming
import Text.Read (readMaybe)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

renderPath :: [PathComponent] -> Text
renderPath = mconcat . concatMap render . reverse
  where
    render (Offset i) = ["[", tshow i, "]"]
    render (Field f) | isIdentifier f = [".", f]
                     | otherwise = ["[", tshow f, "]"]
    isIdentifier _ = True

valueAsText :: Value -> Text
valueAsText = TL.toStrict . AT.encodeToLazyText

parsePathComponent :: String -> PathComponent
parsePathComponent p =
  case readMaybe p of
    Just i -> Offset i
    Nothing -> Field (T.pack p)

parsePath :: [String] -> [PathComponent]
parsePath = map parsePathComponent
