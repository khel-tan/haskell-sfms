module Main (main) where

-- import Control.Monad.State (runState)
import Lib

-- import Lib (printFileProperty)
-- import Lib (listFS)

main :: IO ()
main = do
  let initialState = simpleFS
      crumbs = []
      filesystem = (initialState, crumbs)
  loop filesystem

loop :: Filesystem -> IO ()
loop filesystem = do
  putStrLn ">root"
  input <- getLine
  let args = words input
  print args
  let (state, crumbs) = filesystem
  case head args of
    "exit" -> putStrLn "Exiting"
    "cd" -> do
      let newFilesystem = navigate (args!!1) filesystem
      loop newFilesystem
    "ls" -> do
      putStrLn $ listContents filesystem
      loop filesystem
    "mkdir" -> do
      let newFilesystem = createDirectory (args!!1) filesystem
      loop newFilesystem
    "touch" -> do
      let newFilesystem = createFile filesystem (args!!1)
      loop newFilesystem
    _ -> do
      putStrLn "Command not recognised. Try again..."
      loop filesystem