module Main where

import System.Environment
import System.Directory
import System.Process

import Parse
import Pass
import Texp

enterTestDir = do
  setCurrentDirectory "/home/kasra/projects/cornerstone-tests/tests/blockify"
    
main' = do  
  args <- getArgs
  wd <- getCurrentDirectory
  putStrLn wd
  enterTestDir
  let filename = case args of
                   [] -> "string.bb"
                   [x] -> x
                   x:xs -> x
  result <- parse filename
  putStrLn $ show result

main = do  
  args <- getArgs
  wd <- getCurrentDirectory
  putStrLn wd
  enterTestDir
  let filename = case args of
                   [] -> "argcall.ok"
                   [x] -> x
                   x:xs -> x
  result <- readFile filename
  putStrLn $ (show . unshow) result
