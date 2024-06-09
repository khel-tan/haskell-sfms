{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib
  ( module Lib,
  )
where

data TextFile = TextFile
    { fileName :: String,
    content :: String
  }
  deriving (Show)
data File = Text TextFile | Directory String [File] deriving (Show)

type Path = [String]

simpleFS ::File
simpleFS = Directory "root"
                [Directory "lectures" [
                    Text (TextFile "computer_systems_1" "some stuff"),
                    Text (TextFile "NPP_5" "some stuff")
                ],
                Text (TextFile "README.md" "Some markdown")
                ]

listFS :: File -> String
listFS = listContentsHelper 0
    where
        listContentsHelper :: Int -> File -> String
        listContentsHelper indent (Text textFile) = replicate indent '-' ++ fileName textFile
        listContentsHelper indent (Directory dirName dirContent)
            = replicate indent '-' ++ dirName
                ++ "\n" ++ replicate indent ' ' ++ "|\n"
                ++ unlines (map (listContentsHelper (indent+1)) dirContent)

printFileProperty :: File -> (File -> String) -> Path -> String
-- printFileName file (p:path) =  
printFileProperty filesystem printFunction [name] = printFunction $ searchInDir filesystem name

-- printFileContent :: File -> Path -> String
-- -- printFileName file (p:path) =  
-- printFileContent filesystem [fileName] = getName $ searchInDir filesystem fileName

searchInDir :: File -> String -> File
searchInDir (Directory _ files) name = searchInDirHelper files
    where
        searchInDirHelper::[File]-> File
        searchInDirHelper [] = Text (TextFile "Null" "")
        searchInDirHelper (x:xs) = if getName x == name then x else searchInDirHelper xs

getName :: File -> String
getName (Directory name _) = name
getName (Text textFile) = fileName textFile