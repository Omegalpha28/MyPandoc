{-
-- EPITECH PROJECT, 2025
-- mypandoc
-- File description:
-- encoder file for the pandoc.
-}

module XmlEncoder (
    encodeToXML
) where

import Library
import UtilsEncoder (selectTag, formatText)

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
