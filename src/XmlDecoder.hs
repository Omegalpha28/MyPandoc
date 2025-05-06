{-
-- EPITECH PROJECT, 2025
-- mypandoc
-- File description:
-- Decoder file for the pandoc.
-}

module XmlDecoder (
    parseXML
) where

import Library
import UtilsDecoder
import Data.Char (isSpace, isAlphaNum, isDigit)
import Data.List (isPrefixOf, stripPrefix)
import Text.Read (readMaybe)

parseXML :: String -> Either String Documents
parseXML input = do
    ((), rest1) <- skipSpaces input
    (_, rest2) <- string "<document>" rest1
    ((), rest3) <- skipSpaces rest2
    (header, rest4) <- parseHeaderXML rest3
    ((), rest5) <- skipSpaces rest4
    (body, rest6) <- parseBodyXML rest5
    ((), rest7) <- skipSpaces rest6
    (_, rest8) <- string "</document>" rest7
    ((), rest9) <- skipSpaces rest8
    if null rest9
        then Right (Documents header body)
        else Left "Error: Extra content after document"

parseHeaderXML :: Parser HeaderFile
parseHeaderXML input = do
    (_, rest1) <- string "<header" input
    (attrs, rest2) <- parseAttributes rest1
    (_, rest3) <- string ">" rest2
    (_, rest4) <- string "</header>" rest3
    let title = lookup "title" attrs
        author = lookup "author" attrs
        date = lookup "date" attrs
    case title of
        Just t -> Right (HeaderFile t author date, rest4)
        Nothing -> Left "Error: Header missing title attribute"

parseAttributes :: Parser [(String, String)]
parseAttributes input =
    if ">" `isPrefixOf` input
        then Right ([], input)
        else do
            ((), rest1) <- skipSpaces input
            (name, rest2) <- parseName rest1
            (_, rest3) <- char '=' rest2
            (value, rest4) <- parseQuoted rest3
            (restAttrs, rest5) <- parseAttributes rest4
            Right ((name, value) : restAttrs, rest5)

parseBodyXML :: Parser BodyFile
parseBodyXML input = do
    (_, rest1) <- string "<body>" input
    (contents, rest2) <- parseContentsXML rest1
    (_, rest3) <- string "</body>" rest2
    Right (BodyFile contents, rest3)

parseContentsXML :: Parser [ContentElement]
parseContentsXML input
    | "</" `isPrefixOf` input = Right ([], input)
    | otherwise = do
        (content, rest1) <- parseContentXML input
        (restContents, rest2) <- parseContentsXML rest1
        Right (content : restContents, rest2)

parseContentXML :: Parser ContentElement
parseContentXML input =
    case parseParagraphXML input of
        Right r -> Right r
        Left _ -> case parseSectionXML input of
            Right r -> Right r
            Left _ -> case parseCodeBlockXML input of
                Right r -> Right r
                Left _ -> case parseTextElementXML input of
                    Right r -> Right r
                    Left _ -> case parseLinkElementXML input of
                        Right r -> Right r
                        Left _ -> case parseImageElementXML input of
                            Right r -> Right r
                            Left _ -> case parseListElementXML input of
                                Right r -> Right r
                                Left _ -> Left "Error: Invalid content element"

parseParagraphXML :: Parser ContentElement
parseParagraphXML input = do
    (_, rest1) <- string "<paragraph>" input
    (contents, rest2) <- parseContentsXML rest1
    (_, rest3) <- string "</paragraph>" rest2
    Right (Paragraph contents, rest3)

parseSectionXML :: Parser ContentElement
parseSectionXML input = do
    (_, rest1) <- string "<section" input
    (attrs, rest2) <- parseAttributes rest1
    (_, rest3) <- char '>' rest2
    (contents, rest4) <- parseContentsXML rest3
    (_, rest5) <- string "</section>" rest4
    let title = lookup "title" attrs
    Right (Section title contents, rest5)

parseCodeBlockXML :: Parser ContentElement
parseCodeBlockXML input = do
    (_, rest1) <- string "<codeblock>" input
    (code, rest2) <- parseUntil '<' rest1
    (_, rest3) <- string "</codeblock>" rest2
    Right (CodeBlock code, rest3)

parseTextElementXML :: Parser ContentElement
parseTextElementXML input = do
    (tag, rest1) <- parseName input
    (_, rest2) <- char '>' rest1
    (text, rest3) <- parseUntil '<' rest2
    (_, rest4) <- string ("</" ++ tag ++ ">") rest3
    let italic = tag == "italic"
        bold = tag == "bold"
        code = tag == "code"
    Right (TextElement (Text text italic bold code), rest4)

parseLinkElementXML :: Parser ContentElement
parseLinkElementXML input = do
    (_, rest1) <- string "<link" input
    (attrs, rest2) <- parseAttributes rest1
    (_, rest3) <- char '>' rest2
    (content, rest4) <- parseUntil '<' rest3
    (_, rest5) <- string "</link>" rest4
    case lookup "src" attrs of
        Just src -> Right (LinkElement (Link src content), rest5)
        Nothing -> Left "Error: Link missing src attribute"

parseImageElementXML :: Parser ContentElement
parseImageElementXML input = do
    (_, rest1) <- string "<image" input
    (attrs, rest2) <- parseAttributes rest1
    (_, rest3) <- char '>' rest2
    (content, rest4) <- parseUntil '<' rest3
    (_, rest5) <- string "</image>" rest4
    case lookup "src" attrs of
        Just src -> Right (ImageElement (Image src content), rest5)
        Nothing -> Left "Error: Image missing src attribute"

parseListElementXML :: Parser ContentElement
parseListElementXML input = do
    (_, rest1) <- string "<list>" input
    (items, rest2) <- parseItemsXML rest1
    (_, rest3) <- string "</list>" rest2
    Right (ListElement items, rest3)

parseItemsXML :: Parser [Item]
parseItemsXML input
    | "</" `isPrefixOf` input = Right ([], input)
    | otherwise = do
        (item, rest1) <- parseItemXML input
        (restItems, rest2) <- parseItemsXML rest1
        Right (item : restItems, rest2)

parseItemXML :: Parser Item
parseItemXML input = do
    (_, rest1) <- string "<item>" input
    (contents, rest2) <- parseContentsXML rest1
    (_, rest3) <- string "</item>" rest2
    Right (Item contents, rest3)
