module Main (main) where

import Lib
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  let initialState = simpleFS
      crumbs = []
      filesystem =  (initialState, crumbs)
  loop filesystem

resolveFilesystemUpdate :: Filesystem -> Maybe Filesystem -> Filesystem
resolveFilesystemUpdate oldFS Nothing = oldFS
resolveFilesystemUpdate _ (Just newFS) = newFS

loop :: Filesystem -> IO ()
loop filesystem = do
  putStr $ printWorkingDirectory filesystem
  hFlush stdout
  input <- getLine
  putStrLn ""
  let inputList = words input
  case inputList of
    [] -> do
      putStrLn "No command entered..."
      loop filesystem
    (command:args) -> case command of
      "exit" -> putStrLn "Exiting"
      _ -> do
        (newFilesystem, output ) <- handleCommand filesystem command args
        putStrLn output
        loop $ resolveFilesystemUpdate filesystem newFilesystem

handleCommand :: Filesystem -> String -> [String] -> IO (Maybe Filesystem, String)
handleCommand filesystem command args =
  case command of
      "cd" -> return (navigate filesystem $ head args, "Switching directory...")
      "search" -> return (searchDirectory (head args) filesystem, "Searching...")
      "mkdir" -> return (createDirectory (head args) filesystem, "Creating directory...")
      
      "create" -> return (createFile (head args) filesystem, "Creating file...")
      "cat" -> return (Just filesystem, readItem (head args) filesystem)
      "update" -> if fileExists name filesystem
                  then do
                      putStrLn "Please enter new content"
                      newContent <- getLine
                      return (updateFile name newContent filesystem, "Updating...")
                  else return (Just filesystem, "File not found...")
                  where
                    name = head args
      "rm" -> return (deleteItem (head args) filesystem, "Deleting...")
      "ls" -> return (Just filesystem, listContents filesystem)
      "cp" -> return (copy filesystem (head args, last args), "Copying...")
      
      _ -> return (Just filesystem, "Command invalid. Try again...")

