module Main (main) where

import Lib (listContents, simpleFS)

main :: IO ()
main = putStrLn $ listContents simpleFS
