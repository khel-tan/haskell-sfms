{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib
  ( module Lib,
  )
where

-- class FSItem a where
--     getName :: a -> String
--     getContent :: a -> String

-- data File = File
--   { fileName :: String
--   , fileContent :: String
--   } deriving (Show)

-- data Directory = Directory
--   { dirName :: String
--   , dirContents :: [FileSystemItemWrapper]
--   } deriving (Show)

type Name = String
type Content = String
data File = TextFile
    { fileName :: Name,
    fileContent :: Content
  } deriving (Show)
data FSItem = Entry File | Directory Name [FSItem] deriving (Show)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSCrumbTrail = [FSCrumb]
type Filesystem = (FSItem, FSCrumbTrail)

simpleFS ::FSItem
simpleFS = Directory "root"
                [Directory "lectures" [
                    Entry $ TextFile "computer_systems_1" "some stuff",
                    Entry $ TextFile "NPP_5" "some stuff"
                ],
                    Entry $ TextFile "README.md" "Some markdown"
                ]


navigate :: Name -> Filesystem -> Filesystem
navigate name filesystem = case name of
    ".." -> navigateUp filesystem
    _ -> navigateDown name filesystem


navigateDown :: Name -> Filesystem -> Filesystem
navigateDown name (Directory dirName contents, crumbs) =
    let (ls, rest) = break isNamed contents
    in case rest of
        (target:rs) -> (target, FSCrumb dirName ls rs:crumbs)
        [] -> error $ "Directory or file named " ++ name ++ " not found in " ++ dirName
    where
        isNamed (Entry textFile) = fileName textFile == name
        isNamed (Directory subdirName _) = subdirName == name
navigateDown _ _ = error "Cannot navigate down from a file"

navigateUp :: Filesystem -> Filesystem
navigateUp (currentDir, FSCrumb parentName ls rs:crumbs) =
    (Directory parentName (ls ++ currentDir:rs), crumbs)



createDirectory :: Name -> Filesystem -> Filesystem
createDirectory subdirName (Directory dirName contents, crumbs) =
    let subdir = Directory subdirName []
    in (Directory dirName (subdir:contents), crumbs)

createFile :: Filesystem -> Name -> Filesystem
createFile (Directory dirName contents, crumbs) name =
    let file = Entry $ TextFile name ""
    in (Directory dirName (file:contents), crumbs)

listContents :: Filesystem -> String
listContents (state, _) = listContentsHelper 0 state
    where
        indentPerLevel = 4
        whitespace = ' '
        listContentsHelper :: Int -> FSItem -> String
        listContentsHelper indent (Entry file) = replicate indent whitespace ++ "-" ++ fileName file ++ "\n"
        listContentsHelper indent (Directory dirName contents) =
                dirIndent ++ "-" ++ dirName ++ "\n"
                ++ dirIndent ++ "|" ++ "\n"
                ++ unlines (map (listContentsHelper $ indent+indentPerLevel) contents)
                ++ "\n"
            where
                dirIndent = replicate indent whitespace



getName::FSItem -> String
getName (Entry textFile) = fileName textFile
getName (Directory dirName _) = dirName
