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
  case (command, args) of
      ("cd", []) -> return (navigate filesystem "root", "Returning to root...")
      ("cd", [path]) -> return (navigate filesystem path, "Switching directory...")
      ("search", [target]) -> return (searchDirectory target filesystem, "Searching...")
      ("mkdir", [dirName]) -> return (createDirectory dirName filesystem, "Creating directory...")
      
      ("create", [name]) -> return (createFile name filesystem, "Creating file...")
      ("cat", [itemName]) -> return (Just filesystem, readItem itemName filesystem)
      ("update", [name]) -> if fileExists name filesystem
                  then do
                      putStrLn "Please enter new content"
                      newContent <- getLine
                      return (updateFile name newContent filesystem, "Updating...")
                  else return (Just filesystem, "File not found...")
      ("rm", [itemName]) -> return (deleteItem itemName filesystem, "Deleting...")
      ("ls", []) -> return (Just filesystem, listContents filesystem)
      ("cp", [src, dest]) -> return (copy filesystem (src, dest), "Copying...")
      
      _ -> return (Just filesystem, "Command invalid. Try again...")

