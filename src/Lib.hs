module Lib
    ( someFunc,simpleFS,listContents
    ) where


import Control.Monad.State

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data File = File {
    name :: String,
    content :: String
} deriving (Show)

data Tree = Node File | Directory String [Tree]
type Filesystem = State Tree

-- Create some files
file1 :: File
file1 = File "file1.txt" "This is the content of file1."

file2 :: File
file2 = File "file2.txt" "This is the content of file2."

file3 :: File
file3 = File "file3.txt" "This is the content of file3."

-- Create a simple filesystem
simpleFS :: Tree
simpleFS = Directory "root"
    [ Node file1
    , Node file2
    , Directory "subdir"
        [ Node file3 ]
    ]

-- Lists the names of all the files and directories at the current location
-- Emulates `ls` on UNIX-systems
listContents :: Tree -> String
listContents filesystem = listContentsHelper 0 filesystem
    where
        listContentsHelper :: Int -> Tree -> String
        listContentsHelper indent (Node (File fileName _)) = replicate indent '-' ++ fileName
        listContentsHelper indent (Directory dirName dirContent)
            = replicate indent '-' ++ dirName
                ++ "\n" ++ replicate indent ' ' ++ "|\n"
                ++ unlines (map (listContentsHelper (indent+1)) dirContent)
