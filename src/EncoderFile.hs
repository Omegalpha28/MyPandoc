{-
-- EPITECH PROJECT, 2025
-- mypandoc
-- File description:
-- encoder file for the pandoc.
-}

module EncoderFile (
    encodeDocument
) where

import Library
import XmlEncoder (encodeToXML)
import JsonEncoder (encodeToJSON)
import MarkdownEncoder (encodeToMarkdown)
import Data.List (intercalate)

encodeDocument :: String -> Documents -> Either String String
encodeDocument "xml" doc = Right $ encodeToXML doc
encodeDocument "json" doc = Right $ encodeToJSON doc
encodeDocument "markdown" doc = Right $ encodeToMarkdown doc
encodeDocument fmt _ = Left $
    "Error: Invalid output format. Must be xml, json, or markdown."
