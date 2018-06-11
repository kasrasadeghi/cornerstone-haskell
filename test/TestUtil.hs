module TestUtil where

import Text.Printf
import Control.Monad

-- zipConcat :: [String] -> [String] -> [String]
zipConcat a b =
  let
    alines = sameLength a
    blines = sameLength b
  in
    map (uncurry (++)) $ zip alines blines

sameLength :: [String] -> [String]
sameLength ls =
  let
    lens = map length ls
    maxlen = maximum lens
    linef = printf ("%-" ++ show maxlen ++ "s | ")
  in
    map linef ls

-- main = do
--   forM_ (zipConcat ["hello world", " hi"] ["hi my name ", " is bob"]) $ \t -> do
--     putStrLn t
