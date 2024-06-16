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
getPath :: FSCrumbTrail -> Path
getPath [] = ""
getPath crumbs = intercalate "/" (reverse (map show crumbs)) ++ "/"

printWorkingDirectory :: Filesystem -> String
printWorkingDirectory (Entry _, _) = ""
printWorkingDirectory (state, crumbs) =
    let (Directory dirName _) = state
    in '>' : getPath crumbs ++ dirName

fileExists :: Name -> Filesystem -> Bool
fileExists name filesystem = let searchResults = searchDirectory name filesystem in 
    case searchResults of
    Nothing -> False
    Just (Directory _ _, _) -> False
    Just (Entry _, _) -> True

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

-- CRUD and meta logic for files and directories
createItem :: FSItem -> Filesystem -> Maybe Filesystem
createItem _ (Entry _, _) = Nothing
createItem item (Directory dirName contents, crumbs) = Just (Directory dirName (item:contents), crumbs)

createFile :: FileName -> Filesystem -> Maybe Filesystem
createFile name = createItem (Entry $ TextFile name "")

createDirectory :: DirName -> Filesystem -> Maybe Filesystem
createDirectory dirName = createItem (Directory dirName [])

deleteItem :: Name -> Filesystem -> Maybe Filesystem
deleteItem name filesystem = case focus of
            Nothing -> Just filesystem
            Just (_, FSCrumb dirName ls rs:crumbs) -> Just (Directory dirName (ls++rs), crumbs)
            _ -> Nothing
    where
        focus = searchDirectory name filesystem

readItem :: Name -> Filesystem -> String
readItem name filesystem = case focus of
        Nothing -> ""
        Just (Entry file, _) -> fileContent file
        Just (Directory _ _, _) -> ""
    where
        focus = searchDirectory name filesystem

updateFile :: Name -> Content -> Filesystem -> Maybe Filesystem
updateFile name content filesystem = let item = searchDirectory name filesystem in
    case item of
        Nothing -> Nothing
        Just (Directory _ _, _) -> Just filesystem
        Just (Entry _, crumbs) -> Just $ navigateUp (Entry $ TextFile name content, crumbs)

copy :: Filesystem -> (Path, Path) -> Maybe Filesystem
copy filesystem (srcPath, destPath) = do
    -- Navigate to the source file
    (srcFile, _) <- navigate srcPath filesystem
    -- Navigate to the destination path
    (destFile, destCrumbs) <- navigate destPath filesystem
    -- Ensure the destination is a directory
    case destFile of
        Entry _ -> Nothing
        Directory dirName contents -> let newContents = srcFile : contents
                                          newDir = Directory dirName newContents
                                          newFilesystem = navigateToRoot (newDir, destCrumbs)
                                        in navigate (getPath crumbs) newFilesystem
                                        where
                                            (_, crumbs) = filesystem


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