{-
-- EPITECH PROJECT, 2025
-- mypandoc
-- File description:
-- Decoder file for the pandoc.
-}

module DecoderFile (
    decodeDocument
) where

import Library
import XmlDecoder (parseXML)
import JsonDecoder (parseJSON)
import MarkdownDecoder (parseMarkdown)
import Data.Char (isSpace, isAlphaNum, isDigit)
import Data.List (isPrefixOf, stripPrefix)
import Text.Read (readMaybe)

decodeDocument :: Maybe String -> String -> Either String Documents
decodeDocument (Just "xml") input = parseXML input
decodeDocument (Just "json") input = parseJSON input
decodeDocument (Just "markdown") input = parseMarkdown input
decodeDocument (Just fmt) _ = Left $ "Error: Unsupported input format: " ++ fmt
decodeDocument Nothing input = case detectFormat input of
    Left err -> Left err
    Right "xml" -> parseXML input
    Right "json" -> parseJSON input
    Right "markdown" -> parseMarkdown input
    Right fmt -> Left $ "Error: Detected unsupported format: " ++ fmt

detectFormat :: String -> Either String String
detectFormat input
    | "<document>" `isPrefixOf` dropWhile isSpace input = Right "xml"
    | "{" `isPrefixOf` dropWhile isSpace input = Right "json"
    | "---" `isPrefixOf` dropWhile isSpace input = Right "markdown"
    | otherwise = Left "Error: Unable to detect input format."
