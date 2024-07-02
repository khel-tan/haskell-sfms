module Lib
  ( module Lib,
  )
where

import Data.List (intercalate, tails, isPrefixOf)
import Data.Either (rights)


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
data FSItem = Entry File | Directory Name [FSItem]

-- Necessary definitions for zipper
data FSCrumb = FSCrumb Name [FSItem] [FSItem]
type FSCrumbTrail = [FSCrumb]
type Filesystem = (FSItem, FSCrumbTrail)

instance Show FSCrumb where
    show (FSCrumb dirName _ _) = dirName

instance Show FSItem where
    show (Entry file) = fileName file
    show (Directory dirName _) = dirName ++ "/"

-- instance Eq FSItem where
--     (==) (Entry file1) (Entry file2) = fileName file1 == fileName file2 &&
--                                         fileContent file1 == fileContent file2
--     (==) (Directory dir1Name _) (Directory dir2Name _) = dir1Name == dir2Name
--     (==) (Entry file1)
--     (==) _ _ = False
--     (/=) item1 item2 = not (item1 == item2)
--Display functions
-- Constructs Path string from crumb trail
getPath :: FSCrumbTrail -> Path
getPath [] = ""
getPath crumbs = (intercalate "/" . map show . reverse $ crumbs) ++ "/"

-- prints the current directory
-- The part between $ and # is the currently open file.
printWorkingDirectory :: Filesystem -> String
printWorkingDirectory (Directory dirName _, crumbs) = getPath crumbs ++ dirName ++ "$ "
printWorkingDirectory filesystem = let (Entry file, _) = filesystem
                                    in (printWorkingDirectory . navigateUp) filesystem
                                    ++ fileName file ++ "# "

-- Function to list the contents of a filesystem with indentation
listContents :: Filesystem -> String
listContents (Entry _, _) = ""
listContents (Directory _ contents, _) = intercalate "\n" (map show contents)

printFSTree :: Filesystem -> String
printFSTree (rootItem, _) = go 0 rootItem
  where
    indentPerLevel = 4 -- Number of spaces for each indentation level
    go :: Int -> FSItem -> String
    go indent (Entry file) =
        replicate indent ' ' ++ "- " ++ fileName file ++ "\n"
    go indent (Directory dirName contents) =
        replicate indent ' ' ++ "- " ++ dirName ++ "/\n" ++
        concatMap (go (indent + indentPerLevel)) contents


-- Utility functions

nameIs :: Name -> FSItem -> Bool
nameIs name (Entry file) = name == fileName file
nameIs name (Directory dirName _) = name == dirName

nameContains :: String -> FSItem -> Bool
nameContains substr (Entry file) = any (isPrefixOf substr) (tails $ fileName file)
nameContains substr (Directory dirName _) = any (isPrefixOf substr) (tails dirName)

sameName :: FSItem -> FSItem -> Bool
sameName (Entry file) (Directory dirName _) = fileName file == dirName
sameName (Directory dirName _) (Entry file) = dirName == fileName file
sameName (Entry file1) (Entry file2) = fileName file1 == fileName file2
sameName (Directory dir1Name _) (Directory dir2Name _) = dir1Name == dir2Name
-- Test filesystems
simpleFS ::FSItem
simpleFS = Directory "root"
                [Directory "lectures" [
                    Entry $ TextFile "computer_systems_1" "some stuff",
                    Entry $ TextFile "NPP_5" "some stuff"
                ],
                    Entry $ TextFile "README.md" "Some markdown"
                ]

-- Default file system with only the root
emptyFS :: FSItem
emptyFS = Directory "root" []


-- Navigation
navigate :: Filesystem -> Path -> Either String Filesystem
-- navigate (Entry _, _) _ = Left "Cannot navigate upwards from a file. Please consider using `close`."
navigate filesystem "" = Right filesystem
navigate filesystem path =
    case targetDir of
        ".." -> navigate (navigateUp filesystem) cleanRest
        "." -> navigate filesystem cleanRest
        -- "root" -> navigate (navigateToRoot filesystem) cleanRest
        _ -> navigateDown filesystem targetDir >>=  (`navigate` cleanRest)
    where
        (targetDir, rest) = break ('/'==) path
        -- Clean the rest of the path as having the '/' as head confuses `navigate`
        cleanRest = case rest of
            '/' : xs -> xs
            _ -> rest
navigateDown :: Filesystem -> Name -> Either String Filesystem
navigateDown filesystem name =
    let result = focus filesystem name
    in case result of
        Left _ -> Left $ printWorkingDirectory filesystem ++ name ++ " is not a valid path."
        -- Right (Entry _, _) -> Left $ name ++ " : Not a directory"
        _ -> result

navigateUp :: Filesystem -> Filesystem
navigateUp (currentDir, FSCrumb parentName ls rs:crumbs) =
    (Directory parentName (ls ++ currentDir:rs), crumbs)
navigateUp filesystem = filesystem

navigateToRoot :: Filesystem -> Filesystem
navigateToRoot (currentDir, []) = (currentDir, [])
navigateToRoot filesystem = (navigateToRoot . navigateUp) filesystem

-- Search
-- Tries to search the specified file/directory in the current directory and focuses on it
focus :: Filesystem -> Name -> Either String Filesystem
focus (Entry _, _) _ =
    Left "Cannot search for a file when we are focused on a file!"
focus (Directory dirName contents, crumbs) targetName =
    case break isTarget contents of
        (_, []) -> Left "File not found."
        (left, item:right) -> Right (item, FSCrumb dirName left right : crumbs)
  where
    isTarget :: FSItem -> Bool
    isTarget (Entry file) = fileName file == targetName
    isTarget (Directory subdirName _) = subdirName == targetName

-- Looks for items in the current directory that satisfy the given functions
searchCurrentDirectory :: Filesystem -> (FSItem -> Bool) -> Either String [Path]
searchCurrentDirectory (Entry _, _) _ = Left "Cannot perform search on a file"
searchCurrentDirectory (Directory _ contents, crumbs) f =
    let path = getPath crumbs
    in Right $ map ((path++) . show) (filter f contents)

-- searchCurrentDirectory but recursive version
searchRecursive ::(FSItem -> Bool) -> Filesystem ->  Either String [Path]
searchRecursive f (Directory dirName contents, crumbs) =
        case result of
            [] -> Left "The specified entry does not exist"
            _ -> Right result
        where
            dir = (Directory dirName contents, crumbs)
            go :: [FSItem] -> [Name]
            go = map show
            result = concat $ rights $ searchCurrentDirectory dir f:rest
            rest = map (searchRecursive f)
                $ rights $ map (navigateDown dir) (go contents)
searchRecursive f filesystem = searchCurrentDirectory filesystem f

-- CRUD Logic for files and directories
createItem :: Filesystem -> FSItem -> Either String Filesystem
createItem (Entry _, _) _ =
    Left "Cannot create an item inside a file. This action is legal only inside a directory!"
createItem (Directory dirName contents, crumbs) item
    | not $ any (sameName item) contents = 
        Right (Directory dirName (item:contents), crumbs)
    | otherwise = Left "Item already exists!"

createFile :: Filesystem -> Name -> Either String Filesystem
createFile filesystem name = createItem filesystem (Entry $ TextFile name "")

createDirectory :: Filesystem -> Name -> Either String Filesystem
createDirectory filesystem dirName = createItem filesystem (Directory dirName [])

deleteItem :: Filesystem -> Name -> Either String Filesystem
deleteItem filesystem name =
    let result = focus filesystem name
        in case result of
        Right (_, FSCrumb dirName ls rs:crumbs) -> Right $ navigateUp (Directory dirName (ls++rs), crumbs)
        _ -> result

readItem :: Filesystem -> Name -> Either String String
readItem filesystem name =
    let item = focus filesystem name
    in case item of
        Right (Entry file, _) -> Right $ fileContent file
        Right (Directory _ _, _) -> Left $ name ++ " is a directory."
        Left errorMsg -> Left errorMsg

openFile :: Filesystem -> Name -> Either String Filesystem
openFile filesystem name =
    let result = focus filesystem name
    in case result of
        Right (Directory _ _, _) -> Left "Cannot open a directory!"
        _ -> result

closeFile :: Filesystem -> Either String Filesystem
closeFile (Directory _ _, _) = Left "Cannot close a directory!"
closeFile fileFocus = Right $ navigateUp fileFocus

-- Rename directories and files
rename :: Filesystem -> Name -> Either String Filesystem
rename (Directory _ contents, crumbs) newDirName = Right (Directory newDirName contents, crumbs)
rename (Entry (TextFile _ content), crumbs) newFileName = Right (Entry $ TextFile newFileName content, crumbs)

-- Updates the currently open file. The terminal takes in input and the file is updated,
update :: Filesystem -> IO (Either String Filesystem)
update (Directory _ _, _) = return $ Left "Is a directory."
update (Entry file, crumbs) = do
    putStrLn "Please enter new content."
    newContent <- getLine
    return $ Right (Entry $ TextFile (fileName file) newContent, crumbs)

printFile :: Filesystem -> Either String String
printFile (Directory _ _, _) = Left "Cannot print a directory!"
printFile (Entry file, _) = Right $ "File Name : " ++ fileName file ++ "\n" ++
                                    "File contents : " ++ fileContent file

copy :: Filesystem -> (Path, Path) -> Either String Filesystem
copy filesystem (srcPath, destPath) = do
    -- Navigate to the source file
    (srcFile, _) <- navigate filesystem srcPath
    -- Navigate to the destination path
    (destFile, destCrumbs) <- navigate filesystem destPath
    -- Ensure the destination is a directory
    case destFile of
        Entry _ -> Left "The destination must be a directory!"
        Directory _ _ -> let    (_, crumbs) = filesystem
                                newItem =  createItem (destFile, destCrumbs) srcFile
                        in copyHelper newItem crumbs
        where
            copyHelper :: Either String Filesystem -> [FSCrumb] -> Either String Filesystem
            copyHelper (Left errorMsg) _ = Left errorMsg
            copyHelper (Right newFilesystem) crumbs =
                navigate (navigateToRoot newFilesystem) (getPath crumbs)


