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
import Data.Char (isSpace, isAlphaNum, isDigit)
import Data.List (isPrefixOf, stripPrefix)
import Text.Read (readMaybe)

type Parser a = String -> Either String (a, String)

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

-- ** XML Parsing **

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

-- ** JSON Parsing **

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

-- ** Markdown Parsing **

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
