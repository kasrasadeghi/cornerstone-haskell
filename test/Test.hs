module Main where

import System.Environment
import System.Directory
import System.Process

import Parse
import Pass

enterTestDir = do
  setCurrentDirectory "C:/Users/Kasra/Projects/cornerstone-tests/tests/blockify"
    
main = tBlockify
           
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
