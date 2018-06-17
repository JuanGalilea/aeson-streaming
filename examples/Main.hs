{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import System.Environment (getArgs, getProgName)
import Conduit
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Semigroup ((<>))
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (forM_)
import Data.List (intercalate)
import Data.ByteString (ByteString)

import Data.Aeson.Streaming
import Data.Aeson.Streaming.Examples
import Data.Aeson.Streaming.Examples.Util (renderPath, valueAsText, parsePath)
import Data.Aeson.Streaming.Examples.Util.Conduit (sinkParser)

commands :: Map String (String, [String] -> IO ())
commands = Map.fromList [
  ("string-list", (
      "Read an array of strings from standard input, printing each string",
      \_ ->
        runExample stringArray)),
  ("explode", (
      "Read a JSON value from standard input, printing the leaves with their paths",
      \_ ->
        runExample (jsonExplode .| mapC (\(p, v) ->
                                           renderPath p <> " : " <> valueAsText v)))),
  ("navigate", (
      "Read a JSON value from standard input, printing the value at the path given on the command line.",
      \path ->
        runExample ((yield . maybe "--not found--" valueAsText) =<< sinkParser (navigate $ parsePath path)))),
  ("skip", (
      "Skip over a JSON value from standard input, doing nothing with it at all.",
      \_ ->
        runExample (sinkParser (skipValue =<< root))))
  ]

runExample :: ConduitT ByteString Text IO () -> IO ()
runExample c = runConduit $ stdinC .| c .| mapM_C T.putStrLn

main :: IO ()
main =
  getArgs >>= \case
    [] ->
      showHelp
    help : _ | help == "help" || help == "--help" ->
      showHelp
    cmd : args ->
      case Map.lookup cmd commands of
        Just (_, f) -> f args
        Nothing -> putStrLn "Unknown command"

showHelp :: IO ()
showHelp =
  do
    self <- getProgName
    putStrLn $ "Usage: " <> self <> " COMMAND ARGS..."
    putStrLn ""
    putStrLn "COMMAND can be one of:"
    forM_ (Map.toList commands) $ \(command, (desc, _)) ->
      putStrLn $ "  " <> command <> replicate (15 - length command) ' ' <> wrap desc

wrap :: String -> String
wrap = loop [] [] . words
  where
    loop :: [String] -> [[String]] -> [String] -> String
    loop line acc [] =
      intercalate "\n" . addIndent [] . map (unwords . reverse) $ line : acc
    loop [] acc (w : ws) = loop [w] acc ws
    loop line acc (w : ws) =
      if sum (map length line) + length line + length w > width
      then loop [w] (line : acc) ws
      else loop (w : line) acc ws
    addIndent acc [] = acc
    addIndent acc [w] = w : acc
    addIndent acc (w : ws) = addIndent ((replicate indent ' ' ++ w) : acc) ws
    indent = 17
    width = 75 - indent
