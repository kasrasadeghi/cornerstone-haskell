module Main where

import System.Environment
import System.Directory
import System.Process

import Parse
import Pass

enterTestDir = do
  setCurrentDirectory "/home/kasra/projects/cornerstone-test/tests/blockify"
    
main = do  
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
           
tParse = do
  enterTestDir
  result <- parse "argcall.bb"
  putStrLn $ show result

tBlockify = do
  enterTestDir
  result <- parse "argcall.bb"
  putStrLn $ show $ blockify result
  expected <- readProcess "cat" ["argcall.ok"] ""
  putStr expected

