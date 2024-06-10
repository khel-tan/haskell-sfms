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

navigateDown :: Name -> Filesystem -> Filesystem
navigateDown name (Directory dirName contents, crumbs) =
    let (ls, target:rs) = break isNamed contents
    in (target, FSCrumb dirName ls rs:crumbs)
    where
        isNamed (Entry textFile) = fileName textFile == name
        isNamed (Directory subdirName _) = subdirName == name
navigateDown _ _ = error "Cannot navigate down from a file"
 

getName::FSItem -> String
getName (Entry textFile) = fileName textFile
getName (Directory dirName _) = dirName


--   deriving (Show)
-- data File = Text TextFile | Directory String [File] deriving (Show)


-- newtype Crumb = String File deriving (Show)
-- type Breadcrumbs = [Crumb]
-- type Zipper = (File, Breadcrumbs)

-- simpleFS ::File
-- simpleFS = Directory "root"
--                 [Directory "lectures" [
--                     Text (TextFile "computer_systems_1" "some stuff"),
--                     Text (TextFile "NPP_5" "some stuff")
--                 ],
--                 Text (TextFile "README.md" "Some markdown")
--                 ]

-- listFS :: File -> String
-- listFS = listContentsHelper 0
--     where
--         listContentsHelper :: Int -> File -> String
--         listContentsHelper indent (Text textFile) = replicate indent '-' ++ fileName textFile
--         listContentsHelper indent (Directory dirName dirContent)
--             = replicate indent '-' ++ dirName
--                 ++ "\n" ++ replicate indent ' ' ++ "|\n"
--                 ++ unlines (map (listContentsHelper (indent+1)) dirContent)

-- printFileProperty :: File -> (File -> String) -> Path -> String
-- -- printFileName file (p:path) =  
-- printFileProperty filesystem printFunction [name] = printFunction $ searchInDir filesystem name

-- -- printFileContent :: File -> Path -> String
-- -- -- printFileName file (p:path) =  
-- -- printFileContent filesystem [fileName] = getName $ searchInDir filesystem fileName

-- searchInDir :: File -> String -> File
-- searchInDir (Directory _ files) name = searchInDirHelper files
--     where
--         searchInDirHelper::[File]-> File
--         searchInDirHelper [] = Text (TextFile "Null" "")
--         searchInDirHelper (x:xs) = if getName x == name then x else searchInDirHelper xs

-- getName :: File -> String
-- getName (Directory name _) = name
-- getName (Text textFile) = fileName textFile