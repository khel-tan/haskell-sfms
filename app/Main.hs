module Main (main) where

import Lib
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  let initialState = simpleFS
      crumbs = []
      filesystem =  (initialState, crumbs)
  loop filesystem


loop :: Filesystem -> IO ()
loop filesystem = do
  putStrLn "Looping"
  putStr $ printWorkingDirectory filesystem
  hFlush stdout
  input <- getLine
  let inputList = words input
  case inputList of
    [] -> do
      putStrLn "No command entered..."
      loop filesystem
    (command:args) -> case command of
      "exit" -> putStrLn "Exiting..."
      _ -> do
        
        (newFilesystem, output ) <- handleCommand filesystem command args
        putStrLn output
        loop newFilesystem

handleCommand :: Filesystem -> String -> [String] -> IO (Filesystem, String)
handleCommand filesystem command args =
  case (command, args) of
      ("cd", []) -> return $ handleModify (navigate filesystem "root") "Returning to root..."
      ("cd", [path]) -> return $ handleModify (navigate filesystem path) "Switching directories "

      -- ("search", [target]) -> return (searchRecursive target $ navigateToRoot  filesystem, "Searching all...")
      -- ("search", [".", target]) -> return (searchDirectory target filesystem, "Searching...")
      ("mkdir", [dirName]) -> return $ handleModify (createDirectory filesystem dirName) "Creating directory"
      
      -- ("create", [name]) -> return (createFile name filesystem, "Creating file...")
      ("cat", [itemName]) -> return $ handleRead (readItem filesystem itemName)
      -- ("update", [name]) -> if fileExists name filesystem
      --             then do
      --                 putStrLn "Please enter new content"
      --                 newContent <- getLine
      --                 return (updateFile name newContent filesystem, "Updating...")
      --             else return (Just filesystem, "File not found...")
      -- ("rm", [itemName]) -> return (deleteItem itemName filesystem, "Deleting...")
      ("ls", []) -> return (filesystem, listContents filesystem)
      -- ("cp", [src, dest]) -> return (copy filesystem (src, dest), "Copying...")
      
      _ -> return (filesystem, "Command invalid. Try again...")
  where
    handleModify :: Either String Filesystem -> String -> (Filesystem, String)
    handleModify (Left errorMsg) _ = (filesystem, errorMsg)
    handleModify (Right newFilesystem) msg = (newFilesystem, msg)
    handleRead :: Either String String -> (Filesystem, String)
    handleRead (Left errorMsg) = (filesystem, errorMsg)
    handleRead (Right output) = (filesystem, output)

