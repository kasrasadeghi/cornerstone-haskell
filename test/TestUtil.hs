module TestUtil where

import Text.Printf
import Control.Monad

mbalance :: Monoid m => [m] -> [m] -> ([m], [m])
mbalance as bs
  | length as > length bs = mbalance as (bs ++ [mempty])
  | length as == length bs = (as, bs)
  | length as < length bs = mbalance (as ++ [mempty]) bs

zipConcat :: [String] -> [String] -> [String]
zipConcat a b =
  let
    (as, bs) = mbalance a b
    alines = sameLength as
    blines = sameLength bs
  in
    map (uncurry (++)) $ zip alines blines

sameLength :: [String] -> [String]
sameLength ls =
  let
    lengths = map length ls
    maxlen = maximum lengths
    linef = printf ("%-" ++ show maxlen ++ "s | ")
  in
    map linef ls

-- main = do
--   forM_ (zipConcat ["hello world", " hi"] ["hi my name ", " is bob"]) $ \t -> do
--     putStrLn t
