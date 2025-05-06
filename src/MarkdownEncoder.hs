{-
-- EPITECH PROJECT, 2025
-- mypandoc
-- File description:
-- encoder file for the pandoc.
-}

module MarkdownEncoder (
    encodeToMarkdown
) where

import Library
import UtilsEncoder (selectTag, formatText)

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
