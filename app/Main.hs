module Main (main) where

-- import Control.Monad.State (runState)
import Lib


-- import Lib (printFileProperty)
-- import Lib (listFS)

main :: IO ()
main = do
  let initialState = simpleFS
      crumbs = []
      filesystem =  (initialState, crumbs)
  loop filesystem

loop :: Filesystem -> IO ()
loop filesystem = do
  putStrLn $ printWorkingDirectory filesystem
  input <- getLine
  let args = words input
  
  case args of
    [] -> do
      putStrLn "No command entered..."
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
      "ls" -> do
        putStrLn $ listContents filesystem
        loop filesystem
      _ -> do
        putStrLn "Command not recognised. Try again..."
        loop filesystem