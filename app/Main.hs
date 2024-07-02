module Main (main) where

import Lib
import System.IO (hFlush, stdout, isEOF)
import Data.List (intercalate)
import Data.Either(fromRight)
import System.Exit(exitSuccess)
import Control.Monad(when)

main :: IO ()
main = do
  let initialState = emptyFS
      crumbs = []
      filesystem =  (initialState, crumbs)
  putStrLn "Please type in `?` to see a list of legal commands."
  loop filesystem


loop :: Filesystem -> IO ()
loop filesystem = do
  -- putStrLn "Looping"
  
  putStr $ printWorkingDirectory filesystem
  hFlush stdout
  eof <- isEOF
  when eof $ putStrLn "" >> exitSuccess
  input <- getLine
  let inputList = words input
  case inputList of
    [] -> do
      putStrLn "No command entered..."
      loop filesystem
    (command:args) -> case command of
      "exit" -> putStrLn "" >> exitSuccess
      _ -> do
        
        (newFilesystem, output ) <- handleCommand filesystem command args
        putStrLn output
        loop newFilesystem

handleCommand :: Filesystem -> String -> [String] -> IO (Filesystem, String)
handleCommand filesystem command args =
  case (command, args) of
      ("cd", []) -> return $ handleModify (navigate filesystem "root") "Returning to root..."
      ("cd", [path]) -> return $ handleModify (navigate filesystem path) "Switching directories "

      ("search", [path, "--substring", target]) ->
          do
            let newFilesystem = fromRight filesystem $ navigate filesystem path
            return $ handleSearch (searchRecursive (nameContains target) newFilesystem)
      ("search", [path, "--full-name", target]) ->
          do
            let newFilesystem = fromRight filesystem $ navigate filesystem path
            return $ handleSearch (searchRecursive (nameIs target) newFilesystem)
      ("cat", [itemName]) -> return $ handleRead (readItem filesystem itemName)
      ("mkdir", [dirName]) -> return $ handleModify (createDirectory filesystem dirName) "Creating directory"
      ("create", [name]) -> return $ handleModify (createFile filesystem name) "Creating file..."


      -- Commands that involve opening a file
      ("open", [itemName]) -> return $ handleModify (openFile filesystem itemName) "Opening file..."
      ("close", []) -> return $ handleModify (closeFile filesystem) "Closing file..."
      ("rename", [name]) -> return $ handleModify (rename filesystem name) "Renaming file..." 
      ("update", []) -> do
                          result <- update filesystem
                          return $ handleModify result "Updating..."


      ("print", []) -> return $ handleRead (printFile filesystem)
      ("rm", [itemName]) -> return $ handleModify (deleteItem filesystem itemName) "Item deleted!"
      ("ls", []) -> return (filesystem, listContents filesystem)
      ("tree", []) -> return (filesystem, printFSTree filesystem)
      ("cp", [src, dest]) -> return $ handleModify (copy filesystem (src, dest)) "Copying..."

      ("?", []) -> return (filesystem, listAllCommands)
      ("?", [cmd]) -> return (filesystem, explainCommand cmd)
      
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

listAllCommands :: String
listAllCommands = intercalate "\n" (explanations ++ commands)
  where
    explanations = ["The text before the $ indicates the directory. When we have opened a file,\
                  \ the prompt changes to `[Path] $ OpenedFileName #`."]
    commands = ["? : Information about legal commands",
                "cd : Change directory", 
              "search : Search files and directories with a given name or substring", 
              "cat : Print the contents of a file", 
              "create : Create a new file in the current directory", 
              "mkdir : Create a new directory",
              
              "open : Open a file",
              "close : Close file",
              "rename : Rename a file that is currently open",
              "update : Update the contents of an open file",
              "rm : Remove a file or a directory",
              "print : Print the name and contents of an open file",
              "ls : List the contents inside the current directory",
              "tree : Print a pretty tree of the filesystem with the current directory as the root",
              "cp : Copy a file/directory from one place to another" ]

explainCommand :: String -> String
explainCommand command =
  case command of
    "?" -> help_text
    "cd" -> cd_text
    "search" -> search_text
    "cat" -> cat_text
    "create" ->create_text
    "mkdir" -> mkdir_text
    "open" -> open_text
    "close" -> close_text
    "rename" -> rename_text
    "update" -> update_text
    "print" -> print_text
    "rm" -> rm_text
    "ls" -> ls_text
    "tree" -> tree_text
    "cp" -> cp_text
    _ -> "Command unrecognised! Type `?` to see a list of all legal commands."
  where
    help_text = "? : ? [Optionally command name]\
                \ Just type in `?` to see a list of available commands.\n\
                \ Type `? [command]` to see a brief explanation of how said command works."
    cd_text = "cd : cd [Dir]\n\
          \ Change the directory as specified. `..` indicates parent directory.\n\
          \`.` indicates the current directory. An empty argument navigates us to root."
    search_text = "search : search [Path] [--substring|--full-name] [str]\n\
            \         Treats the specified path as the root and \
            \looks for the string in all descendents.\n"
    cat_text = "cat : cat [Filename]\n\
          \       Prints the contents of the specified file"
    create_text = "create : create [Filename]\n\
            \     Creates a new file in the current directory."
    mkdir_text = "mkdir : mkdir [DirName]\n\
            \     Creates a new directory."
    open_text = "open : open [FileName]\n\
            \     Opens the specified file and then we can modify or rename it."
    close_text = "close : close\n\
            \     Closes the open file." 
    rename_text = "rename : rename\n\
            \     Renames the open file. Does not work if we have not opened a file!"
    update_text = "update : update\n\
            \     Updates the contents of the open file. \
            \Does not work if we have not opened a file!"
    print_text = "print : print\n\
            \     Prints the name and content of a file that is currently open."
    rm_text = "rm : rm [FileName|DirName]\n\
            \     Removes the specified file or directory."
    ls_text = "ls : ls\n\
            \     Lists the content of the current directory."
    tree_text = "tree : tree\n\
            \     Prints a pretty tree with the current directory as the root of the tree."
    cp_text = "cp : cp [SrcFilePath|SrcDirPath] [DestDirPath]\n\
            \ Copies the source file or directory to the destination directory."
    
    
