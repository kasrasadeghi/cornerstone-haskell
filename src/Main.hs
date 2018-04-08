module Main where

import System.Environment
import System.Directory

import Parse

main = do  
   args <- getArgs
   wd <- getCurrentDirectory
   putStrLn wd
   setCurrentDirectory "../tests/parser"
   let filename = case args of
                  [] -> "string.bb"
                  [x] -> x
                  x:xs -> x
   result <- parse filename
   putStrLn $ show result

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x
