{-
-- EPITECH PROJECT, 2025
-- mypandoc
-- File description:
-- decoder file for the pandoc.
-}

module UtilsDecoder where

import Library
import Data.Char (isSpace, isAlphaNum, isDigit)
import Data.List (isPrefixOf, stripPrefix)
import Text.Read (readMaybe)

type Parser a = String -> Either String (a, String)

skipSpaces :: Parser ()
skipSpaces input = Right ((), dropWhile isSpace input)

char :: Char -> Parser Char
char c [] = Left $ "Error: Expected '" ++ [c] ++ "', got end of input"
char c (x:xs)
    | x == c = Right (c, xs)
    | otherwise = Left $ "Error: Expected '" ++ [c] ++ "', got '" ++ [x] ++ "'"

string :: String -> Parser String
string s input
    | s `isPrefixOf` input = Right (s, drop (length s) input)
    | otherwise = Left $ "Error: Expected \"" ++ s ++ "\", got \"" ++ take (length s) input ++ "\""

parseUntil :: Char -> Parser String
parseUntil delim input =
    let (before, rest) = span (/= delim) input
    in if null rest
        then Left $ "Error: Expected delimiter '" ++ [delim] ++ "'"
        else Right (before, tail rest)

parseEscapedString :: Parser String
parseEscapedString input = do
    (str, rest) <- parseUntil '"' input
    return (unescape str, rest)
  where
    unescape [] = []
    unescape ('&':'l':'t':';':xs) = '<' : unescape xs
    unescape ('&':'g':'t':';':xs) = '>' : unescape xs
    unescape ('&':'a':'m':'p':';':xs) = '&' : unescape xs
    unescape ('&':'q':'u':'o':'t':';':xs) = '"' : unescape xs
    unescape ('\\':'"':xs) = '"' : unescape xs
    unescape ('\\':'\\':xs) = '\\' : unescape xs
    unescape ('\\':'n':xs) = '\n' : unescape xs
    unescape (x:xs) = x : unescape xs

parseQuoted :: Parser String
parseQuoted input = do
    (_, rest1) <- char '"' input
    (str, rest2) <- parseEscapedString rest1
    return (str, rest2)

parseName :: Parser String
parseName input =
    let (name, rest) = span (\c -> isAlphaNum c || c == '-' || c == ':') input
    in if null name
        then Left "Error: Expected a name"
        else Right (name, rest)
