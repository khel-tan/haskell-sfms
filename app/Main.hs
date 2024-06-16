module Main (main) where

import Lib

main :: IO ()
main = do
  let initialState = simpleFS
      crumbs = []
      filesystem =  (initialState, crumbs)
  loop filesystem

checkForFailure :: Filesystem -> Maybe Filesystem -> Filesystem
checkForFailure oldFS Nothing = oldFS
checkForFailure _ (Just newFS) = newFS

loop :: Filesystem -> IO ()
loop filesystem = do
  putStrLn $ printWorkingDirectory filesystem
  input <- getLine
  let args = words input
  
  case args of
    [] -> do
      putStrLn "No command entered..."
      loop filesystem
    (command:arguments) -> case command of
      "exit" -> putStrLn "Exiting"
      "search" -> do
        let result = searchDirectory (head arguments) filesystem
        print result
        loop filesystem
      "cd" -> do
        let result = navigate (head arguments) filesystem
        case result of
          Nothing -> do
            putStrLn "Path is invalid! Reverting back to previous state..."
            loop filesystem
          Just newFilesystem -> do
            loop newFilesystem
      "mkdir" -> do
        let result = createDirectory (head arguments) filesystem
        case result of
          Nothing -> do
            putStrLn "Unable to create directory!"
            loop filesystem
          Just newFilesystem -> do
            loop newFilesystem
      "create" -> do
        let result = createFile (head arguments) filesystem
        case result of
          Nothing -> do
            putStrLn "Unable to create directory!"
            loop filesystem
          Just newFilesystem -> do
            loop newFilesystem
      "delete" -> do
        let result = deleteItem (head arguments) filesystem
        case result of
          Nothing -> do
            putStrLn "Unable to delete!"
            loop filesystem
          Just newFilesystem -> do
            loop newFilesystem
      "read" -> do
        let result = readItem (head arguments) filesystem
        putStrLn result
        loop filesystem
      "update" -> do
        let name = (head arguments)
        if fileExists name filesystem
          then do
            putStrLn "Enter new content"
            newContent <- getLine
            let result = updateFile name newContent filesystem
            case result of
              Nothing -> loop filesystem
              Just newFilesystem -> loop newFilesystem
        else do
          putStrLn "File does not exist!"
          loop filesystem
      "ls" -> do
        putStrLn $ listContents filesystem
        loop filesystem
      "cp" -> do
        let newFilesystem = copy filesystem (head arguments, last arguments)
        -- putStrLn $ listContents $ checkForFailure filesystem newFilesystem
        print $ checkForFailure filesystem newFilesystem
        loop $ checkForFailure filesystem newFilesystem
      _ -> do
        putStrLn "Command not recognised. Try again..."
        loop filesystem