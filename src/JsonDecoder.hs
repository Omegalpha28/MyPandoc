{-
-- EPITECH PROJECT, 2025
-- mypandoc
-- File description:
-- Decoder file for the pandoc.
-}

module JsonDecoder (
    parseJSON
) where

import Library
import UtilsDecoder
import Data.Char (isSpace, isAlphaNum, isDigit)
import Data.List (isPrefixOf, stripPrefix)
import Text.Read (readMaybe)

parseJSON :: String -> Either String Documents
parseJSON input = do
    ((), rest1) <- skipSpaces input
    (value, rest2) <- parseObjectJSON rest1
    case value of
        JObject doc -> do
            ((), rest3) <- skipSpaces rest2
            if null rest3
                then case lookup "header" doc of
                    Just (JObject header) -> do
                        headerFile <- parseHeaderJSON header
                        case lookup "body" doc of
                            Just (JArray body) -> do
                                contents <- parseContentsJSON body
                                return (Documents headerFile (BodyFile contents))
                            _ -> Left "Error: Invalid or missing body in JSON"
                    _ -> Left "Error: Invalid or missing header in JSON"
                else Left "Error: Extra content after JSON document"
        _ -> Left "Error: JSON document must be an object"

data JSONValue
    = JObject [(String, JSONValue)]
    | JArray [JSONValue]
    | JString String
    | JNumber Double
    | JBool Bool
    | JNull
    deriving Show

parseObjectJSON :: Parser JSONValue
parseObjectJSON input = do
    ((), rest0) <- skipSpaces input
    (_, rest1) <- char '{' rest0
    (pairs, rest2) <- parsePairsJSON rest1
    (_, rest3) <- char '}' rest2
    Right (JObject pairs, rest3)

parsePairsJSON :: Parser [(String, JSONValue)]
parsePairsJSON input
    | "}" `isPrefixOf` input = Right ([], input)
    | otherwise = do
        ((), rest0) <- skipSpaces input
        (key, rest1) <- parseQuoted rest0
        (_, rest2) <- char ':' rest1
        ((), rest3) <- skipSpaces rest2
        (value, rest4) <- parseValueJSON rest3
        ((), rest5) <- skipSpaces rest4
        (comma, rest6) <- if "}" `isPrefixOf` rest5
                          then Right ((), rest5)
                          else do
                              (c, r) <- char ',' rest5
                              Right ((), r)
        (restPairs, rest7) <- parsePairsJSON rest6
        Right ((key, value) : restPairs, rest7)

parseValueJSON :: Parser JSONValue
parseValueJSON input = do
    ((), rest0) <- skipSpaces input
    case parseObjectJSON rest0 of
        Right r -> Right r
        Left _ -> case parseArrayJSON rest0 of
            Right r -> Right r
            Left _ -> case parseStringJSON rest0 of
                Right r -> Right r
                Left _ -> case parseNumberJSON rest0 of
                    Right r -> Right r
                    Left _ -> case parseBoolJSON rest0 of
                        Right r -> Right r
                        Left _ -> case parseNullJSON rest0 of
                            Right r -> Right r
                            Left _ -> Left $ "Error: Invalid JSON value at \"" ++ take 10 rest0 ++ "\""

parseArrayJSON :: Parser JSONValue
parseArrayJSON input = do
    ((), rest0) <- skipSpaces input
    (_, rest1) <- char '[' rest0
    (values, rest2) <- parseValuesJSON rest1
    (_, rest3) <- char ']' rest2
    Right (JArray values, rest3)

parseValuesJSON :: Parser [JSONValue]
parseValuesJSON input
    | "]" `isPrefixOf` input = Right ([], input)
    | otherwise = do
        (value, rest1) <- parseValueJSON input
        ((), rest2) <- skipSpaces rest1
        (comma, rest3) <- if "]" `isPrefixOf` rest2
                          then Right ((), rest2)
                          else do
                              (c, r) <- char ',' rest2
                              Right ((), r)
        (restValues, rest4) <- parseValuesJSON rest3
        Right (value : restValues, rest4)

parseStringJSON :: Parser JSONValue
parseStringJSON input = do
    (str, rest) <- parseQuoted input
    Right (JString str, rest)

parseNumberJSON :: Parser JSONValue
parseNumberJSON input =
    let (num, rest) = span (\c -> isDigit c || c `elem` ".-eE+") input
    in if null num
        then Left "Error: Expected a number"
        else case reads num :: [(Double, String)] of
            [(n, "")] -> Right (JNumber n, rest)
            _ -> Left $ "Error: Invalid number format: " ++ num

parseBoolJSON :: Parser JSONValue
parseBoolJSON input
    | "true" `isPrefixOf` input = Right (JBool True, drop 4 input)
    | "false" `isPrefixOf` input = Right (JBool False, drop 5 input)
    | otherwise = Left "Error: Expected true or false"

parseNullJSON :: Parser JSONValue
parseNullJSON input = do
    (_, rest) <- string "null" input
    Right (JNull, rest)

parseHeaderJSON :: [(String, JSONValue)] -> Either String HeaderFile
parseHeaderJSON attrs = do
    title <- case lookup "title" attrs of
        Just (JString t) -> Right t
        Just v -> Left $ "Error: Title must be a string, got " ++ show v
        Nothing -> Left "Error: Header missing title"
    let author = case lookup "author" attrs of
            Just (JString a) -> Just a
            _ -> Nothing
        date = case lookup "date" attrs of
            Just (JString d) -> Just d
            _ -> Nothing
    return (HeaderFile title author date)

parseContentsJSON :: [JSONValue] -> Either String [ContentElement]
parseContentsJSON values = mapM parseContentJSON values

parseContentJSON :: JSONValue -> Either String ContentElement
parseContentJSON (JArray arr) = do
    contents <- parseArrayContentsJSON arr
    return (Paragraph contents)
parseContentJSON (JObject obj) = do
    case lookup "section" obj of
        Just (JObject section) -> do
            title <- case lookup "title" section of
                Just (JString t) -> Right (Just t)
                Just JNull -> Right Nothing
                Just v -> Left $ "Error: Section title must be a string or null, got " ++ show v
                Nothing -> Right Nothing
            content <- case lookup "content" section of
                Just (JArray c) -> parseContentsJSON c
                Just v -> Left $ "Error: Section content must be an array, got " ++ show v
                Nothing -> Left "Error: Section missing content"
            return (Section title content)
        Just v -> Left $ "Error: Section must be an object, got " ++ show v
        Nothing -> case lookup "codeblock" obj of
            Just (JArray [JString c]) -> return (CodeBlock c)
            Just v -> Left $ "Error: Codeblock must be an array with a single string, got " ++ show v
            Nothing -> case lookup "list" obj of
                Just (JArray items) -> do
                    parsedItems <- mapM parseListItemJSON items
                    return (ListElement parsedItems)
                Just v -> Left $ "Error: List must be an array, got " ++ show v
                Nothing -> case lookup "link" obj of
                    Just (JObject link) -> do
                        src <- case lookup "url" link of
                            Just (JString s) -> Right s
                            Just v -> Left $ "Error: Link url must be a string, got " ++ show v
                            Nothing -> Left "Error: Link missing url"
                        content <- case lookup "content" link of
                            Just (JArray c) -> parseArrayContentsJSON c
                            Just v -> Left $ "Error: Link content must be an array, got " ++ show v
                            Nothing -> Left "Error: Link missing content"
                        return (LinkElement (Link src (concatMap showContent content)))
                    Just v -> Left $ "Error: Link must be an object, got " ++ show v
                    Nothing -> case lookup "image" obj of
                        Just (JObject image) -> do
                            src <- case lookup "url" image of
                                Just (JString s) -> Right s
                                Just v -> Left $ "Error: Image url must be a string, got " ++ show v
                                Nothing -> Left "Error: Image missing url"
                            content <- case lookup "alt" image of
                                Just (JArray c) -> parseArrayContentsJSON c
                                Just v -> Left $ "Error: Image alt must be an array, got " ++ show v
                                Nothing -> Left "Error: Image missing alt"
                            return (ImageElement (Image src (concatMap showContent content)))
                        Just v -> Left $ "Error: Image must be an object, got " ++ show v
                        Nothing -> case lookup "bold" obj of
                            Just (JString text) -> return (TextElement (Text text False True False))
                            Just v -> Left $ "Error: Bold must be a string, got " ++ show v
                            Nothing -> case lookup "italic" obj of
                                Just (JString text) -> return (TextElement (Text text True False False))
                                Just v -> Left $ "Error: Italic must be a string, got " ++ show v
                                Nothing -> case lookup "code" obj of
                                    Just (JString text) -> return (TextElement (Text text False False True))
                                    Just v -> Left $ "Error: Code must be a string, got " ++ show v
                                    Nothing -> Left $ "Error: Unknown content object: " ++ show (map fst obj)
parseContentJSON v = Left $ "Error: Content must be an object or array, got " ++ show v

parseArrayContentsJSON :: [JSONValue] -> Either String [ContentElement]
parseArrayContentsJSON = mapM parseArrayContentJSON
  where
    parseArrayContentJSON (JString s) = Right (TextElement (Text s False False False))
    parseArrayContentJSON (JObject obj) = parseContentJSON (JObject obj)
    parseArrayContentJSON v = Left $ "Error: Array content must be a string or object, got " ++ show v

showContent :: ContentElement -> String
showContent (TextElement (Text s _ _ _)) = s
showContent (LinkElement (Link _ s)) = s
showContent (ImageElement (Image _ s)) = s
showContent _ = ""

parseListItemJSON :: JSONValue -> Either String Item
parseListItemJSON (JArray arr) = do
    contents <- parseArrayContentsJSON arr
    return (Item contents)
parseListItemJSON v = Left $ "Error: List item must be an array, got " ++ show v
