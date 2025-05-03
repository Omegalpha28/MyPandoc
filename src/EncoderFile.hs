{-
-- EPITECH PROJECT, 2025
-- mypandoc
-- File description:
-- encoder file for the pandoc.
-}

module EncoderFile (
    encodeDocument,
    encodeToXML,
    encodeToJSON,
    encodeToMarkdown
) where

import Library
import Data.List (intercalate)

selectTag :: Bool -> Bool -> Bool -> String
selectTag True _ _ = "code"
selectTag _ True _ = "italic"
selectTag _ _ True = "bold"
selectTag _ _ _ = "text"

formatText :: Bool -> Bool -> Bool -> String -> String
formatText True _ _ txt = "`" ++ txt ++ "`"
formatText _ True _ txt = "*" ++ txt ++ "*"
formatText _ _ True txt = "**" ++ txt ++ "**"
formatText _ _ _ txt = txt

encodeDocument :: String -> Documents -> Either String String
encodeDocument "xml" doc = Right $ encodeToXML doc
encodeDocument "json" doc = Right $ encodeToJSON doc
encodeDocument "markdown" doc = Right $ encodeToMarkdown doc
encodeDocument fmt _ = Left $
    "Error: Invalid output format. Must be xml, json, or markdown."

encodeToXML :: Documents -> String
encodeToXML (Documents header body) =
    "<document>\n" ++
    encodeHeaderXML header ++
    "  <body>\n" ++
    concatMap (encodeContentXML 4) (content body) ++
    "  </body>\n" ++
    "</document>"

encodeHeaderXML :: HeaderFile -> String
encodeHeaderXML (HeaderFile title author date) =
    let titleAttr = " title=\"" ++ escapeXML title ++ "\""
        authAttr = maybe "" (\a -> " author=\"" ++ escapeXML a ++ "\"") author
        dateAttr = maybe "" (\d -> " date=\"" ++ escapeXML d ++ "\"") date
    in "  <header" ++ titleAttr ++ authAttr ++ dateAttr ++ "></header>\n"

encodeContentXML :: Int -> ContentElement -> String
encodeContentXML indent element = case element of
    Paragraph contents -> encodeParagraphXML indent contents
    Section title contents -> encodeSectionXML indent title contents
    CodeBlock code -> encodeCodeBlockXML indent code
    TextElement txt -> encodeTextElementXML indent txt
    LinkElement link -> encodeLinkElementXML indent link
    ImageElement img -> encodeImageElementXML indent img
    ListElement items -> encodeListElementXML indent items

encodeParagraphXML :: Int -> [ContentElement] -> String
encodeParagraphXML indent contents =
    let indentStr = replicate indent ' '
    in indentStr ++ "<paragraph>\n" ++
       concatMap (encodeContentXML (indent + 2)) contents ++
       indentStr ++ "</paragraph>\n"

encodeSectionXML :: Int -> Maybe String -> [ContentElement] -> String
encodeSectionXML indent title contents =
    let indentStr = replicate indent ' '
        titleAttr = maybe "" (\t -> " title=\"" ++ escapeXML t ++ "\"") title
    in indentStr ++ "<section" ++ titleAttr ++ ">\n" ++
       concatMap (encodeContentXML (indent + 2)) contents ++
       indentStr ++ "</section>\n"

encodeCodeBlockXML :: Int -> String -> String
encodeCodeBlockXML indent code =
    let indentStr = replicate indent ' '
    in indentStr ++ "<codeblock>" ++ escapeXML code ++ "</codeblock>\n"

encodeTextElementXML :: Int -> Text -> String
encodeTextElementXML indent (Text txt italic bold code) =
    let indentStr = replicate indent ' '
        tag = selectTag code italic bold
        openTag = "<" ++ tag ++ ">"
        closeTag = "</" ++ tag ++ ">\n"
    in indentStr ++ openTag ++ escapeXML txt ++ closeTag

encodeLinkElementXML :: Int -> Link -> String
encodeLinkElementXML indent (Link src content) =
    let indentStr = replicate indent ' '
    in indentStr ++ "<link src=\"" ++ escapeXML src ++ "\">" ++
       escapeXML content ++ "</link>\n"

encodeImageElementXML :: Int -> Image -> String
encodeImageElementXML indent (Image src content) =
    let indentStr = replicate indent ' '
    in indentStr ++ "<image src=\"" ++ escapeXML src ++ "\">" ++
       escapeXML content ++ "</image>\n"

encodeListElementXML :: Int -> [Item] -> String
encodeListElementXML indent items =
    let indentStr = replicate indent ' '
    in indentStr ++ "<list>\n" ++
       concatMap (encodeItemXML (indent + 2)) items ++
       indentStr ++ "</list>\n"

encodeItemXML :: Int -> Item -> String
encodeItemXML indent (Item contents) =
    let indentStr = replicate indent ' '
    in indentStr ++ "<item>\n" ++
       concatMap (encodeContentXML (indent + 2)) contents ++
       indentStr ++ "</item>\n"

escapeXML :: String -> String
escapeXML = concatMap escapeChar
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar c = [c]

encodeToJSON :: Documents -> String
encodeToJSON (Documents header body) =
    "{\n" ++
    "  \"header\": " ++ encodeHeaderJSON header ++ ",\n" ++
    "  \"body\": [\n" ++
    intercalate ",\n" (map (encodeContentJSON 4) (content body)) ++ "\n" ++
    "  ]\n" ++
    "}"

encodeHeaderJSON :: HeaderFile -> String
encodeHeaderJSON (HeaderFile title author date) =
    let titleField = "\"title\": \"" ++ escapeJSON title ++ "\""
        authorField =
            maybe "" (\a -> ", \"author\": \"" ++ escapeJSON a ++ "\"") author
        dateField =
            maybe "" (\d -> ", \"date\": \"" ++ escapeJSON d ++ "\"") date
    in "{" ++ titleField ++ authorField ++ dateField ++ "}"

encodeContentJSON :: Int -> ContentElement -> String
encodeContentJSON indent element = indentStr ++ case element of
    Paragraph contents -> encodeParagraphJSON contents
    Section title contents -> encodeSectionJSON title contents
    CodeBlock code -> encodeCodeBlockJSON code
    TextElement txt -> encodeTextElementJSON txt
    LinkElement link -> encodeLinkElementJSON link
    ImageElement img -> encodeImageElementJSON img
    ListElement items -> encodeListElementJSON items
  where
    indentStr = replicate indent ' '

encodeParagraphJSON :: [ContentElement] -> String
encodeParagraphJSON contents =
    "{\"type\": \"paragraph\", \"content\": [" ++
    intercalate ", " (map (encodeContentJSON 0) contents) ++ "]}"

encodeSectionJSON :: Maybe String -> [ContentElement] -> String
encodeSectionJSON title contents =
    let titleStr = maybe "null" (\t -> "\"" ++ escapeJSON t ++ "\"") title
        contentStr = intercalate ", " (map (encodeContentJSON 0) contents)
    in "{\"type\": \"section\", \"title\": " ++ titleStr ++
       ", \"content\": [" ++ contentStr ++ "]}"

encodeCodeBlockJSON :: String -> String
encodeCodeBlockJSON code =
    "{\"type\": \"codeblock\", \"content\": \"" ++ escapeJSON code ++ "\"}"

encodeTextElementJSON :: Text -> String
encodeTextElementJSON (Text txt italic bold code) =
    let typeStr = selectTag code italic bold
        contentStr = escapeJSON txt
    in "{\"type\": \"" ++ typeStr ++
       "\", \"content\": \"" ++ contentStr ++ "\"}"

encodeLinkElementJSON :: Link -> String
encodeLinkElementJSON (Link src content) =
    "{\"type\": \"link\", \"src\": \"" ++ escapeJSON src ++
    "\", \"content\": \"" ++ escapeJSON content ++ "\"}"

encodeImageElementJSON :: Image -> String
encodeImageElementJSON (Image src content) =
    "{\"type\": \"image\", \"src\": \"" ++ escapeJSON src ++
    "\", \"content\": \"" ++ escapeJSON content ++ "\"}"

encodeListElementJSON :: [Item] -> String
encodeListElementJSON items =
    let itemsStr = intercalate ", " (map (encodeItemJSON 0) items)
    in "{\"type\": \"list\", \"items\": [" ++ itemsStr ++ "]}"

encodeItemJSON :: Int -> Item -> String
encodeItemJSON indent (Item contents) =
    "{\"type\": \"item\", \"content\": [" ++
    intercalate ", " (map (encodeContentJSON 0) contents) ++ "]}"

escapeJSON :: String -> String
escapeJSON = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar c = [c]

encodeToMarkdown :: Documents -> String
encodeToMarkdown (Documents header body) =
    encodeHeaderMarkdown header ++
    "\n" ++
    concatMap encodeContentMarkdown (content body)

encodeHeaderMarkdown :: HeaderFile -> String
encodeHeaderMarkdown (HeaderFile title author date) =
    "---\n" ++
    "title: " ++ title ++ "\n" ++
    maybe "" (\a -> "author: " ++ a ++ "\n") author ++
    maybe "" (\d -> "date: " ++ d ++ "\n") date ++
    "---"

encodeContentMarkdown :: ContentElement -> String
encodeContentMarkdown element = case element of
    Paragraph contents -> encodeParagraphMarkdown contents
    Section title contents -> encodeSectionMarkdown title contents
    CodeBlock code -> encodeCodeBlockMarkdown code
    TextElement txt -> encodeTextElementMarkdown txt
    LinkElement link -> encodeLinkElementMarkdown link
    ImageElement img -> encodeImageElementMarkdown img
    ListElement items -> encodeListElementMarkdown items

encodeParagraphMarkdown :: [ContentElement] -> String
encodeParagraphMarkdown contents =
    concatMap encodeContentMarkdown contents ++ "\n\n"

encodeSectionMarkdown :: Maybe String -> [ContentElement] -> String
encodeSectionMarkdown title contents =
    maybe "" (\t -> "## " ++ t ++ "\n") title ++
    concatMap encodeContentMarkdown contents ++ "\n"

encodeCodeBlockMarkdown :: String -> String
encodeCodeBlockMarkdown code =
    "```\n" ++ code ++ "\n```\n"

encodeTextElementMarkdown :: Text -> String
encodeTextElementMarkdown (Text txt italic bold code) =
    formatText code italic bold txt

encodeLinkElementMarkdown :: Link -> String
encodeLinkElementMarkdown (Link src content) =
    "[" ++ content ++ "](" ++ src ++ ")"

encodeImageElementMarkdown :: Image -> String
encodeImageElementMarkdown (Image src content) =
    "![" ++ content ++ "](" ++ src ++ ")"

encodeListElementMarkdown :: [Item] -> String
encodeListElementMarkdown items =
    concatMap encodeItemMarkdown items ++ "\n"

encodeItemMarkdown :: Item -> String
encodeItemMarkdown (Item contents) =
    "- " ++ concatMap encodeContentMarkdown contents
