module Test where

import System.Environment
import System.Directory
import System.Info

import Control.Monad
import Control.Exception
import Data.List

import Parse
import Pass
import Texp

import TestUtil
    
data OK = OK deriving (Eq, Show)
data Err = Err String deriving (Eq, Show)
         
test = do
  -- testBlockify
  -- testUnshow
  testBecomeify
  testBecomeify'

testBecomeify' = do
  putStrLn "\ntesting becomeify'"
  tests <- listTests "become"
  results <- forM tests $ \tn -> do
               putStrLn $ " - testing: " ++ tn
               tBecomeify' tn
  testCheck results tests

testBecomeify = do
  putStrLn "\ntesting becomeify"
  tests <- listTests "become"
  results <- forM tests $ \tn -> do
               putStrLn $ " - testing: " ++ tn
               tBecomeify tn
  testCheck results tests
  
testBlockify = do  
  putStrLn "\ntesting blockify"
  tests <- listTests "blockify"
  results <- forM tests $ \tn -> do
               putStrLn $ " - testing: " ++ tn
               tBlockify tn
  testCheck results tests  

testUnshow = do
  putStrLn "\ntesting unshow"
  enterTestDir "blockify"
  ls <- listDirectory "."
  let tests = map (dropEnd 3 ) . filter (isSuffixOf ".bb") $ ls
  results <- forM tests $ \tn -> do
               putStrLn $ " - testing: " ++ tn
               tUnshow tn
  if all (== Right OK) results then putStrLn "passed all tests"
  else print $ map snd $ filter ((/= Right OK).fst) $ zip results tests  
       
------ test util ---------------------------------
      
testDir = if os == "linux" then "/home/kasra/projects/cornerstone-tests/tests/"
          else                  "C:/Users/Kasra/Projects/cornerstone-tests/tests/"

resultPrint :: Either Err OK -> IO ()
resultPrint result = do
  case result of
    Left (Err s) -> putStrLn s
    Right OK -> print "OK"

testCheck :: [Either Err OK] -> [String] -> IO ()
testCheck results tests = do
  forM_ (zip results tests) $ \(result, testname) -> do
    putStrLn ""
    putStrLn testname
    resultPrint result
  -- if all (== Right OK) results then putStrLn "passed all tests"
  -- else print $ map snd $ filter ((/= Right OK).fst) $ zip results tests

enterTestDir subdir = do
  setCurrentDirectory $ (testDir ++ subdir)
                                
dropEnd n = reverse . (drop n) . reverse
            
listTests testdir = do
  enterTestDir testdir
  ls <- listDirectory "."
  tests <- filterM doesFileExist $
           map ((++ ".bb") . (dropEnd 3)) . filter (isSuffixOf ".ok") $ ls
  return $ map (dropEnd 3) tests 
     
------ test suites -------------------------------
           
tParse testname = do -- testname == "argcall"
  enterTestDir "parser"
  result <- parse $ testname ++ ".bb"
  putStrLn $ show result

tUnshow testname = do -- testname == "argcall"
  enterTestDir "blockify"
  contents <- readFile $ testname ++ ".bb"
  let texp = pProgram testname contents
      expected = show texp
      result = show . unshow $ show texp
  if (expected == result) then return $ Right OK
  else return $ Left "show . unshow is not the identity after showing a texp"

reportEqErr :: Texp -> Texp -> Err
reportEqErr expected result = Err . unlines $  zipConcat (lines ("expected:\n" ++ show expected)) (lines ("result:\n" ++ show result))
-- reportEqErr expected result = Err (show expected ++ "\n" ++ show result)

tBlockify = tPass blockify "blockify"
tBecomeify = tPass (becomeify . blockify) "become"
tBecomeify' = tPass (flattenDo . becomeify' . blockify) "become"
                              
tPass :: (Texp -> Texp) -> String -> String -> IO (Either Err OK)
tPass f testDir testname = do -- testDir == "blockify", testname == "argcall"
  enterTestDir testDir
  let filename = testname ++ ".bb"
  src <- readFile filename
  let texp = pProgram filename src
  let result = f texp
  expected' <- readFile $ testname ++ ".ok"
  let expected = unshow expected'
  if (expected == result) then return $ Right OK
  else                         return $ Left $ reportEqErr expected result
