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
import qualified Data.Text.IO as T
import Data.Semigroup ((<>))
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (forM_, void)
import Data.List (intercalate)

import Data.Aeson.Streaming
import Data.Aeson.Streaming.Examples
import Data.Aeson.Streaming.Examples.Util (renderPath, valueAsText, parsePath)
import Data.Aeson.Streaming.Examples.Util.Conduit (sinkParser)

commands :: Map String (String, [String] -> IO ())
commands = Map.fromList [
  ("string-list", (
      "Read an array of strings from standard input, printing each string",
      \_ ->
        runConduit $ stdinC .| stringArray
                            .| mapM_C T.putStrLn)),
  ("explode", (
      "Read a JSON value from standard input, printing the leaves with their paths",
      \_ ->
        runConduit $ stdinC .| jsonExplode
                            .| mapM_C (\(p, v) ->
                                         T.putStrLn $ renderPath p <> " : " <> valueAsText v))),
  ("navigate", (
      "Read a JSON value from standard input, printing the value at the path given on the command line.",
      \path ->
        runConduit $ stdinC .| (yield =<< sinkParser (navigate $ parsePath path) )
                            .| mapM_C (T.putStrLn . maybe "--not found--" valueAsText))),
  ("skip", (
      "Skip over a JSON value from standard input, doing nothing with it at all.",
      \_ ->
        runConduit $ stdinC .| sinkParser (void (root >>= skipValue))))
  ]

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
