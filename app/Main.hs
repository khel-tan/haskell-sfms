module Main (main) where

import Lib
import System.IO (hFlush, stdout)
import Data.List (intercalate)
import Lib (rename)

main :: IO ()
main = do
  let initialState = simpleFS
      crumbs = []
      filesystem =  (initialState, crumbs)
  loop filesystem


loop :: Filesystem -> IO ()
loop filesystem = do
  -- putStrLn "Looping"
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
      ("search", [".", "--substring", target]) ->
          return $ handleSearch (searchRecursive (nameContains target) filesystem)
      ("search", [".", "--full-name", target]) ->
          return $ handleSearch (searchRecursive (nameIs target) filesystem )
      ("mkdir", [dirName]) -> return $ handleModify (createDirectory filesystem dirName) "Creating directory"
      
      -- CRUD Operations
      ("create", [name]) -> return $ handleModify (createFile filesystem name) "Creating file..."
      ("cat", [itemName]) -> return $ handleRead (readItem filesystem itemName)
      ("open", [itemName]) -> return $ handleModify (openFile filesystem itemName) "Opening file..."
      ("close", []) -> return $ handleModify (closeFile filesystem) "Closing file..."
      ("rename", [name]) -> return $ handleModify (rename filesystem name) "Renaming file..." 
      -- ("update")
      -- ("rename")
      -- ("?")
      -- ("open", [itemName]) -> 
      -- ("update", [name]) -> if fileExists name filesystem
      --             then do
      --                 putStrLn "Please enter new content"
      --                 newContent <- getLine
      --                 return (updateFile name newContent filesystem, "Updating...")
      --             else return (Just filesystem, "File not found...")
      ("rm", [itemName]) -> return $ handleModify (deleteItem filesystem itemName) "Item deleted!"
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

    handleSearch :: Either String [Path] -> (Filesystem, String)
    handleSearch (Left errorMsg) = (filesystem, errorMsg)
    handleSearch (Right paths) = (filesystem, intercalate "\n" paths)

