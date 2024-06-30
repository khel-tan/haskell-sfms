module Lib
  ( module Lib,
  )
where

import Data.List (intercalate, find)


-- Type definitions
type Name = String
type Content = String
type Path = String

-- Only one textfile for now
-- TODO: Add creation and modification dates
data File = TextFile
    { fileName :: Name,
    fileContent :: Content
  } deriving (Show)
data FSItem = Entry File | Directory Name [FSItem] deriving (Show)

-- Necessary definitions for zipper
data FSCrumb = FSCrumb Name [FSItem] [FSItem]
type FSCrumbTrail = [FSCrumb]
type Filesystem = (FSItem, FSCrumbTrail)

instance Show FSCrumb where
    show (FSCrumb dirName _ _) = dirName


--Display functions
getPath :: FSCrumbTrail -> Path
getPath [] = ""
getPath crumbs = (intercalate "/" . map show . reverse $ crumbs) ++ "/"

printWorkingDirectory :: Filesystem -> String
printWorkingDirectory (Directory dirName _, crumbs) = getPath crumbs ++ dirName ++ " >"
printWorkingDirectory filesystem = printWorkingDirectory $ navigateUp filesystem

-- Function to list the contents of a filesystem with indentation
listContents :: Filesystem -> String
listContents (rootItem, _) = listContentsHelper 0 rootItem
  where
    indentPerLevel = 4 -- Number of spaces for each indentation level
    listContentsHelper :: Int -> FSItem -> String
    listContentsHelper indent (Entry file) =
        replicate indent ' ' ++ "- " ++ fileName file ++ "\n"
    listContentsHelper indent (Directory dirName contents) =
        replicate indent ' ' ++ "- " ++ dirName ++ "/\n" ++
        concatMap (listContentsHelper (indent + indentPerLevel)) contents


-- Utility functions

-- fileExists :: Name -> Filesystem -> Bool
-- fileExists name = isJust . searchDirectory name

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
navigate :: Filesystem -> Path -> Either String Filesystem
navigate filesystem "" = Right filesystem
navigate filesystem path =
    case targetDir of
        ".." -> navigate (navigateUp filesystem) cleanRest
        "." -> navigate filesystem cleanRest
        "root" -> navigate (navigateToRoot filesystem) cleanRest
        _ -> navigateDown filesystem targetDir >>=  (`navigate` cleanRest)
    where
        (targetDir, rest) = break ('/'==) path
        cleanRest = case rest of
            '/' : xs -> xs
            _ -> rest
navigateDown :: Filesystem -> Name -> Either String Filesystem
navigateDown filesystem name = 
    let result = searchDirectory filesystem name
    in case result of
        Left _ -> Left $ printWorkingDirectory filesystem ++ name ++ " is not a valid path."
        _ -> result

navigateUp :: Filesystem -> Filesystem
navigateUp (currentDir, FSCrumb parentName ls rs:crumbs) =
    (Directory parentName (ls ++ currentDir:rs), crumbs)
navigateUp filesystem = filesystem

navigateToRoot :: Filesystem -> Filesystem
navigateToRoot (currentDir, []) = (currentDir, [])
navigateToRoot filesystem = navigateToRoot $ navigateUp filesystem

-- Search

searchDirectory :: Filesystem -> Name -> Either String Filesystem
searchDirectory (Entry _, _) _ = 
    Left "Cannot search for a file when we are focused on a file!"
searchDirectory (Directory dirName contents, crumbs) targetName =
    case break isTarget contents of
        (_, []) -> Left "File not found."
        (left, item:right) -> Right (item, FSCrumb dirName left right : crumbs)
  where
    isTarget :: FSItem -> Bool
    isTarget (Entry file) = fileName file == targetName
    isTarget (Directory subdirName _) = subdirName == targetName

-- searchRecursive :: Name -> Filesystem -> Maybe Filesystem
-- searchRecursive _ (Entry _, _) = Nothing
-- searchRecursive targetName dir =
--     let (Directory dirName contents, crumbs) = dir in
--         case searchDirectory targetName dir of
--             Nothing -> fromJust $ find isJust (map (searchRecursive targetName) $ mapMaybe (go dir) contents)
--             result -> result
--             where
--                 go :: Filesystem -> FSItem -> Maybe Filesystem
--                 go fs (Entry file) = navigateDown (fileName file) fs
--                 go fs (Directory dirName _) = navigateDown dirName fs

-- CRUD and meta logic for files and directories
createItem :: Filesystem -> FSItem -> Either String Filesystem
createItem (Entry _, _) _ = 
    Left "Cannot create an item inside a file. This action is legal only inside a directory!"
createItem (Directory dirName contents, crumbs) item = 
    Right (Directory dirName (item:contents), crumbs)

createFile :: Filesystem -> Name -> Either String Filesystem
createFile filesystem name = createItem filesystem (Entry $ TextFile name "")

createDirectory :: Filesystem -> Name -> Either String Filesystem
createDirectory filesystem dirName = createItem filesystem (Directory dirName [])

-- deleteItem :: Name -> Filesystem -> Maybe Filesystem
-- deleteItem name filesystem = case searchDirectory name filesystem of
--             Nothing -> Just filesystem
--             Just (_, FSCrumb dirName ls rs:crumbs) -> Just $ navigateUp (Directory dirName (ls++rs), crumbs)
--             _ -> Nothing

readItem :: Filesystem -> Name -> Either String String
readItem filesystem name = 
    let item = searchDirectory filesystem name
    in case item of
        Right (Entry file, _) -> Right $ fileContent file
        Right (Directory _ _, _) -> Left $ name ++ " is a directory."
        Left errorMsg -> Left errorMsg

-- updateFile :: Name -> Content -> Filesystem -> Maybe Filesystem
-- updateFile name content filesystem = case searchDirectory name filesystem of
--         Nothing -> Nothing
--         Just (Directory _ _, _) -> Just filesystem
--         Just (Entry oldFile, crumbs) ->
--             Just $ navigateUp (Entry oldFile {fileContent = content}, crumbs)

-- copy :: Filesystem -> (Path, Path) -> Maybe Filesystem
-- copy filesystem (srcPath, destPath) = do
--     -- Navigate to the source file
--     (srcFile, _) <- navigate filesystem srcPath
--     -- Navigate to the destination path
--     (destFile, destCrumbs) <- navigate filesystem destPath
--     -- Ensure the destination is a directory
--     case destFile of
--         Entry _ -> Nothing
--         Directory dirName contents -> let newContents = srcFile : contents
--                                           newDir = Directory dirName newContents
--                                           newFilesystem = navigateToRoot (newDir, destCrumbs)
--                                         in navigate newFilesystem (getPath crumbs)
--                                         where
--                                             (_, crumbs) = filesystem


