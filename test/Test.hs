module Main where

import System.Environment
import System.Directory
import System.Process

import Parse
import Pass
import Texp

enterTestDir = do
  -- setCurrentDirectory "C:/Users/Kasra/Projects/cornerstone-tests/tests/blockify"
  setCurrentDirectory "/home/kasra/projects/cornerstone-tests/tests/blockify"
    
main = tBlockify
           
tParse = do
  enterTestDir
  result <- parse "argcall.bb"
  putStrLn $ show result

tBlockify = do
  enterTestDir
  result <- parse "argcall.bb"
  (putStrLn . treeTexp  . blockify) result
  expected <- readProcess "cat" ["argcall.ok"] ""
  putStr expected

tUnshow = do
  enterTestDir
  contents <- readFile "argcall.bb"
  putStrLn "contents:"
  putStrLn contents
  putStrLn "show . unshow $ contents"
  putStrLn (show (unshow contents))
  
