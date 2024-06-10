module Main (main) where

-- import Control.Monad.State (runState)
import Lib 
-- import Lib (printFileProperty)
-- import Lib (listFS)

main :: IO ()
main = do
  -- let (_, finalState) =
  --       runState
  --         ( do
  --             addFile file1
  --             addFile file2
  --             addDirectory "practice"
  --         )
  --         initialState
  -- putStrLn result
  -- putStrLn $ listContents finalState
  -- let state = simpleFS
  -- putStrLn $ listFS state
  -- putStrLn $ printFileProperty state getName ["README.md"]
  let (state, crumbs) = navigateDown "lectures" (simpleFS, [])
  putStrLn "Hello"
  print state
  print crumbs
