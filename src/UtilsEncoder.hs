{-
-- EPITECH PROJECT, 2025
-- mypandoc
-- File description:
-- encoder file for the pandoc.
-}

module UtilsEncoder (
    selectTag,
    formatText
) where

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
