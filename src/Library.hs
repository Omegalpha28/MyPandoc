{-
-- EPITECH PROJECT, 2025
-- mypandoc
-- File description:
-- Library with all data for the pandoc.
-}

module Library  (Documents(..),
                HeaderFile(..),
                BodyFile(..),
                ContentElement(..),
                Text(..),
                Link(..),
                Image(..),
                Item(..)) where

data Documents = Documents {
    header :: HeaderFile,
    body :: BodyFile
} deriving (Show)

data HeaderFile = HeaderFile {
    title :: String,
    author :: Maybe String,
    date :: Maybe String
} deriving (Show)

data BodyFile = BodyFile {
    content :: [ContentElement]
} deriving (Show)

data ContentElement
    = Paragraph [ContentElement]
    | Section (Maybe String) [ContentElement]
    | CodeBlock String
    | TextElement Text
    | LinkElement Link
    | ImageElement Image
    | ListElement [Item]
    deriving (Show)

data Text = Text {
    text :: String,
    italic :: Bool,
    bold :: Bool,
    code :: Bool
} deriving (Show)

data Link = Link {
    srcLink :: String,
    additionalContent :: String
} deriving (Show)

data Image = Image {
    srcImg :: String,
    addCon :: String
} deriving (Show)

data Item = Item {
    itemContent :: [ContentElement]
} deriving (Show)
