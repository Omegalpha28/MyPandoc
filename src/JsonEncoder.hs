{-
-- EPITECH PROJECT, 2025
-- mypandoc
-- File description:
-- encoder file for the pandoc.
-}

module JsonEncoder (
    encodeToJSON
) where

import Library
import UtilsEncoder (selectTag, formatText)
import Data.List (intercalate)

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
