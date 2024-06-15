module Lib
  ( module Lib,
  )
where

import Data.List (intercalate)

-- Type definitions
type Name = String
type FileName = Name
type DirName = Name
type Content = String
type Path = String

data File = TextFile
    { fileName :: FileName,
    fileContent :: Content
  } deriving (Show)
data FSItem = Entry File | Directory DirName [FSItem] deriving (Show)

data FSCrumb = FSCrumb DirName [FSItem] [FSItem]
type FSCrumbTrail = [FSCrumb]
type Filesystem = (FSItem, FSCrumbTrail)

instance Show FSCrumb where
    show (FSCrumb dirName _ _) = dirName


-- Utility functions
getPath :: FSCrumbTrail -> String
getPath [] = ""
getPath crumbs = intercalate "/" (reverse (map show crumbs)) ++ "/"

printWorkingDirectory :: Filesystem -> String
printWorkingDirectory (Entry _, _) = ""
printWorkingDirectory (state, crumbs) =
    let (Directory dirName _) = state
    in '>' : getPath crumbs ++ dirName


-- Test filesystems
simpleFS ::FSItem
simpleFS = Directory "root"
                [Directory "lectures" [
                    Entry $ TextFile "computer_systems_1" "some stuff",
                    Entry $ TextFile "NPP_5" "some stuff"
                ],
                    Entry $ TextFile "README.md" "Some markdown"
                ]

-- Navigation
navigate :: Path -> Filesystem -> Maybe Filesystem
navigate "" filesystem = Just filesystem
navigate path filesystem =
    case targetDir of
        ".." -> navigate cleanRest $ navigateUp filesystem 
        "." -> navigate cleanRest filesystem
        "root" -> navigate cleanRest $ navigateToRoot filesystem
        _ -> do
            let target = navigateDown targetDir filesystem
            case target of
                Nothing -> Nothing
                Just newFilesystem -> navigate cleanRest newFilesystem
    where
        (targetDir, rest) = break ('/'==) path
        cleanRest = case rest of
            '/' : xs -> xs
            _ -> rest


navigateDown :: DirName -> Filesystem -> Maybe Filesystem
navigateDown dirName filesystem = case searchDirectory dirName filesystem of
    Just (directory, crumbs) -> Just (directory, crumbs)
    _ -> Nothing

navigateUp :: Filesystem -> Filesystem
navigateUp (currentDir, []) = (currentDir, [])
navigateUp (currentDir, FSCrumb parentName ls rs:crumbs) =
    (Directory parentName (ls ++ currentDir:rs), crumbs)

navigateToRoot :: Filesystem -> Filesystem
navigateToRoot (currentDir, []) = (currentDir, [])
navigateToRoot filesystem = navigateToRoot $ navigateUp filesystem

searchDirectory :: Name ->  Filesystem -> Maybe Filesystem
searchDirectory _ (Entry _, _) = Nothing
searchDirectory name (Directory dirName contents, crumbs) =
    let matches = filter isTarget contents
     in
        case matches of
            [] -> Nothing
            (file:_) -> let (ls, _:rs) = break isTarget contents
                        in Just (file, FSCrumb dirName ls rs:crumbs)
    where
        isTarget (Entry textFile) = fileName textFile == name
        isTarget (Directory subdirName _) = subdirName == name

-- navigate :: DirName -> Filesystem -> Filesystem
-- navigate name filesystem = case name of
--     "" -> filesystem
--     ".." -> navigateUp filesystem
--     _ -> navigateDown name filesystem


-- navigateDown :: DirName -> Filesystem -> Filesystem
-- navigateDown name (Directory dirName contents, crumbs) =
--     let (ls, rest) = break isNamed contents
--     in case rest of
--         (target:rs) -> (target, FSCrumb dirName ls rs:crumbs)
--         [] -> error $ "Directory or file named " ++ name ++ " not found in " ++ dirName
--     where
--         isNamed (Entry textFile) = fileName textFile == name
--         isNamed (Directory subdirName _) = subdirName == name
-- navigateDown _ _ = error "Cannot navigate down from a file"

-- navigateUp :: Filesystem -> Filesystem
-- navigateUp (currentDir, []) = (currentDir, [])
-- navigateUp (currentDir, FSCrumb parentName ls rs:crumbs) =
--     (Directory parentName (ls ++ currentDir:rs), crumbs)



-- createDirectory :: DirName -> Filesystem -> Filesystem 
-- createDirectory subdirName (Directory dirName contents, crumbs) =
--     let subdir = Directory subdirName []
--     in (Directory dirName (subdir:contents), crumbs)
-- createDirectory _ _ = error "Cannot create a directory when we are at a file"

-- createFile :: Filesystem -> FileName -> Filesystem
-- createFile (Directory dirName contents, crumbs) name =
--     let file = Entry $ TextFile name ""
--     in (Directory dirName (file:contents), crumbs)
-- createFile _ _ = error "Cannot create a file when we are at a file"

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



-- getName::FSItem -> String
-- getName (Entry textFile) = fileName textFile
-- getName (Directory dirName _) = dirName
