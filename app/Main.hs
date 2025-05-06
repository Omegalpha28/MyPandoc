{-
-- EPITECH PROJECT, 2025
-- mypandoc
-- File description:
-- Main entry point for the pandoc.
-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStr, hPutStrLn, stdout, stderr)
import Data.Maybe (isNothing, fromJust)
import Control.Exception (catch, IOException)
import Library
import DecoderFile
import EncoderFile

data Options = Options
    { inputFile   :: Maybe FilePath
    , outputFormat :: Maybe String
    , outputFile  :: Maybe FilePath
    , inputFormat :: Maybe String
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { inputFile = Nothing
    , outputFormat = Nothing
    , outputFile = Nothing
    , inputFormat = Nothing
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
    [ "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]"
    , ""
    , "   ifile     path to the file to convert"
    , "   oformat   output format (xml, json, markdown)"
    , "   ofile     path to the output file"
    , "   iformat   input format (xml, json, markdown)"
    ]

exitWithError :: String -> IO a
exitWithError msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

exitUsage :: IO a
exitUsage = hPutStr stderr usage >> exitWith (ExitFailure 84)

readInput :: FilePath -> IO String
readInput inputPath =
    readFile inputPath `catch` \e ->
        exitWithError $ "Error: Failed to read input file '" ++
            inputPath ++ "': " ++ show (e :: IOException)

decodeInput :: Maybe String -> String -> IO Documents
decodeInput inputFormat input =
    case decodeDocument inputFormat input of
        Left err -> exitWithError err
        Right doc -> return doc

encodeOutput :: String -> Documents -> IO String
encodeOutput outputFormat doc =
    case encodeDocument outputFormat doc of
        Left err -> exitWithError err
        Right output -> return output

writeOutput :: Maybe FilePath -> String -> IO ()
writeOutput (Just path) output =
    writeFile path output `catch` \e ->
        exitWithError $ "Error: Failed to write output file '" ++ path ++
            "': " ++ show (e :: IOException)
writeOutput Nothing output =
    hPutStr stdout output `catch` \e ->
        exitWithError $ "Error: Failed to write to stdout: " ++
            show (e :: IOException)

processOptions :: Options -> IO ()
processOptions opts = do
    let inputPath = fromJust (inputFile opts)
    input <- readInput inputPath
    doc <- decodeInput (inputFormat opts) input
    output <- encodeOutput (fromJust $ outputFormat opts) doc
    writeOutput (outputFile opts) output

main :: IO ()
main = do
    args <- getArgs
    if null args
        then exitUsage
        else case parseArgs args defaultOptions of
            Left err   -> exitWithError err
            Right opts -> processOptions opts
