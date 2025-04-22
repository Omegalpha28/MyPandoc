{-
-- EPITECH PROJECT, 2025
-- mypandoc
-- File description:
-- Main entry point for the pandoc.
-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Data.Maybe (isNothing)
import Text.Read (readMaybe)

data Options = Options
    {
        inputFile   :: Maybe FilePath,
        outputFormat :: Maybe String,
        outputFile  :: Maybe FilePath,
        inputFormat :: Maybe String
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    {
        inputFile = Nothing,
        outputFormat = Nothing,
        outputFile = Nothing,
        inputFormat = Nothing
    }

parseArgs :: [String] -> Options -> Either String Options
parseArgs [] opts
    | isNothing (inputFile opts)   = Left "Error: -i option is required."
    | isNothing (outputFormat opts) = Left "Error: -f option is required."
    | otherwise = Right opts

parseArgs ("-i":val:rest) opts =
    case inputFile opts of
        Nothing -> parseArgs rest opts { inputFile = Just val }
        Just _  -> Left "Error: -i specified multiple times."

parseArgs ("-f":val:rest) opts =
    case outputFormat opts of
        Nothing -> parseArgs rest opts { outputFormat = Just val }
        Just _  -> Left "Error: -f specified multiple times."

parseArgs ("-o":val:rest) opts =
    case outputFile opts of
        Nothing -> parseArgs rest opts { outputFile = Just val }
        Just _  -> Left "Error: -o specified multiple times."

parseArgs ("-e":val:rest) opts =
    case inputFormat opts of
        Nothing -> parseArgs rest opts { inputFormat = Just val }
        Just _  -> Left "Error: -e specified multiple times."

parseArgs _ _ = Left "Error: Invalid arguments."

usage :: String
usage = unlines
    [
        "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]",
        "",
        "   ifile     path to the file to convert",
        "   oformat   output format (xml, json, markdown)",
        "   ofile     path to the output file",
        "   iformat   input format (xml, json, markdown)"
    ]

exitWithError :: String -> IO a
exitWithError msg = putStrLn msg >> exitWith (ExitFailure 84)

exitUsage :: IO a
exitUsage = putStr usage >> exitWith (ExitSuccess)

processOptions :: Options -> IO ()
processOptions opts = do
    print opts

main :: IO ()
main = do
    args <- getArgs
    if null args
        then exitUsage
        else case parseArgs args defaultOptions of
            Left err   -> exitWithError err
            Right opts -> processOptions opts

