{-
-- EPITECH PROJECT, 2025
-- mypandoc
-- File description:
-- Decoder file for the pandoc.
-}

module MarkdownDecoder (
    parseMarkdown
) where

import Library
import UtilsDecoder
import Data.Char (isSpace, isAlphaNum, isDigit)
import Data.List (isPrefixOf, stripPrefix)
import Text.Read (readMaybe)

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

parseMarkdown :: String -> Either String Documents
parseMarkdown input = do
    ((), rest1) <- skipSpaces input
    (header, rest2) <- parseMarkdownHeader rest1
    (body, rest3) <- parseMarkdownBody rest2
    ((), rest4) <- skipSpaces rest3
    if null rest4
        then Right (Documents header body)
        else Left "Error: Extra content after document"

parseMarkdownHeader :: Parser HeaderFile
parseMarkdownHeader input = do
    (_, rest1) <- string "---" input
    ((), rest2) <- skipSpaces rest1
    (metadata, rest3) <- parseUntilString "---" rest2
    (_, rest4) <- string "---" rest3
    let metadataLines = lines metadata
        attrs = map parseMetadataLine metadataLines
        title = lookup "title" attrs
        author = lookup "author" attrs
        date = lookup "date" attrs
    case title of
        Just t -> Right (HeaderFile t author date, rest4)
        Nothing -> Left "Error: Header missing title"

parseUntilString :: String -> Parser String
parseUntilString delim input =
    let go acc rest =
            if delim `isPrefixOf` rest
                then Right (reverse acc, rest)
                else case rest of
                    [] -> Left $ "Error: Expected \"" ++ delim ++ "\", reached end of input"
                    (c:cs) -> go (c:acc) cs
    in go [] input

parseMetadataLine :: String -> (String, String)
parseMetadataLine line =
    let (key, rest) = span (/= ':') line
        value = dropWhile isSpace $ drop 1 rest
    in (trim key, trim value)
  where
    trim = dropWhile isSpace . dropWhileEnd isSpace
    dropWhileEnd p = reverse . dropWhile p . reverse

parseMarkdownBody :: Parser BodyFile
parseMarkdownBody input = do
    (contents, rest) <- parseMarkdownContents input
    Right (BodyFile contents, rest)

parseMarkdownContents :: Parser [ContentElement]
parseMarkdownContents input
    | "</DOCUMENT>" `isPrefixOf` input = Right ([], input)
    | otherwise = do
        ((), rest0) <- skipSpaces input
        if null rest0
            then Right ([], rest0)
            else do
                (content, rest1) <- parseMarkdownContent rest0
                (restContents, rest2) <- parseMarkdownContents rest1
                Right (content : restContents, rest2)

parseMarkdownContent :: Parser ContentElement
parseMarkdownContent input = do
    ((), rest0) <- skipSpaces input
    case parseMarkdownHeaderElement rest0 of
        Right r -> Right r
        Left _ -> case parseMarkdownCodeBlock rest0 of
            Right r -> Right r
            Left _ -> case parseMarkdownList rest0 of
                Right r -> Right r
                Left _ -> case parseMarkdownParagraph rest0 of
                    Right r -> Right r
                    Left _ -> Left $ "Error: Invalid Markdown content at \"" ++ take 20 rest0 ++ "\""

parseMarkdownHeaderElement :: Parser ContentElement
parseMarkdownHeaderElement input = do
    let (hashes, rest1) = span (== '#') input
        level = length hashes
    if level >= 1 && level <= 6 then do
        ((), rest2) <- skipSpaces rest1
        (title, rest3) <- parseUntil '\n' rest2
        if null title
            then Left "Error: Empty header title"
            else Right (Section (Just $ trim title) [], rest3)
    else
        Left "Error: Expected header with #"

parseMarkdownCodeBlock :: Parser ContentElement
parseMarkdownCodeBlock input = do
    (_, rest1) <- string "```" input
    ((), rest2) <- skipSpaces rest1
    (code, rest3) <- parseUntil '`' rest2
    (_, rest4) <- string "```" rest3
    Right (CodeBlock (trim code), rest4)

parseMarkdownList :: Parser ContentElement
parseMarkdownList input = do
    let (items, rest) = parseMarkdownListItems input
    if null items
        then Left "Error: Expected list items"
        else Right (ListElement items, rest)

parseMarkdownListItems :: String -> ([Item], String)
parseMarkdownListItems input = go input []
  where
    go inp acc = case skipSpaces inp of
        Right ((), rest0)
            | "- " `isPrefixOf` rest0 ->
                let rest1 = drop 2 rest0
                in case parseMarkdownParagraphContent rest1 of
                    Right (content, rest2) -> go rest2 (Item [content] : acc)
                    Left _ -> (reverse acc, inp)
            | otherwise -> (reverse acc, inp)
        Left _ -> (reverse acc, inp)

parseMarkdownParagraph :: Parser ContentElement
parseMarkdownParagraph input = do
    (contents, rest) <- parseMarkdownParagraphContent input
    Right (Paragraph [contents], rest)

parseMarkdownParagraphContent :: Parser ContentElement
parseMarkdownParagraphContent input = do
    ((), rest0) <- skipSpaces input
    if null rest0 || "\n" `isPrefixOf` rest0 || "<" `isPrefixOf` rest0 || "#" `isPrefixOf` rest0 || "- " `isPrefixOf` rest0 || "```" `isPrefixOf` rest0
        then Left "Error: Invalid paragraph content"
        else do
            (content, rest1) <- parseMarkdownInline rest0
            Right (content, rest1)

parseMarkdownInline :: Parser ContentElement
parseMarkdownInline input = do
    case parseMarkdownBold input of
        Right r -> Right r
        Left _ -> case parseMarkdownItalic input of
            Right r -> Right r
            Left _ -> case parseMarkdownCode input of
                Right r -> Right r
                Left _ -> case parseMarkdownLink input of
                    Right r -> Right r
                    Left _ -> case parseMarkdownImage input of
                        Right r -> Right r
                        Left _ -> case parseMarkdownText input of
                            Right r -> Right r
                            Left _ -> Left $ "Error: Invalid inline content at \"" ++ take 20 input ++ "\""

parseMarkdownBold :: Parser ContentElement
parseMarkdownBold input = do
    (_, rest1) <- string "**" input
    (text, rest2) <- parseUntil '*' rest1
    (_, rest3) <- string "**" rest2
    if null text
        then Left "Error: Empty bold content"
        else Right (TextElement (Text text False True False), rest3)

parseMarkdownItalic :: Parser ContentElement
parseMarkdownItalic input = do
    (_, rest1) <- string "*" input
    (text, rest2) <- parseUntil '*' rest1
    (_, rest3) <- string "*" rest2
    if null text
        then Left "Error: Empty italic content"
        else Right (TextElement (Text text True False False), rest3)

parseMarkdownCode :: Parser ContentElement
parseMarkdownCode input = do
    (_, rest1) <- string "`" input
    (text, rest2) <- parseUntil '`' rest1
    (_, rest3) <- string "`" rest2
    if null text
        then Left "Error: Empty code content"
        else Right (TextElement (Text text False False True), rest3)

parseMarkdownLink :: Parser ContentElement
parseMarkdownLink input = do
    (_, rest1) <- string "[" input
    (text, rest2) <- parseUntil ']' rest1
    (_, rest3) <- string "](" rest2
    (url, rest4) <- parseUntil ')' rest3
    (_, rest5) <- string ")" rest4
    if null text || null url
        then Left "Error: Empty link text or URL"
        else Right (LinkElement (Link url text), rest5)

parseMarkdownImage :: Parser ContentElement
parseMarkdownImage input = do
    (_, rest1) <- string "![" input
    (alt, rest2) <- parseUntil ']' rest1
    (_, rest3) <- string "](" rest2
    (url, rest4) <- parseUntil ')' rest3
    (_, rest5) <- string ")" rest4
    if null alt || null url
        then Left "Error: Empty image alt or URL"
        else Right (ImageElement (Image url alt), rest5)

parseMarkdownText :: Parser ContentElement
parseMarkdownText input = do
    let special = "**`*[]!#\n- "
    let (text, rest) = span (`notElem` special) input
    if null text
        then Left "Error: Empty text content"
        else Right (TextElement (Text text False False False), rest)
