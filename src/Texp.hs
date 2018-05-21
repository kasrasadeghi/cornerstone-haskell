module Texp where

import Parse
import Data.Char

instance Show Texp where
    show texp = tail $ treeTexp texp

duplicate string n = concat $ replicate n string
newline i string = duplicate "  " i
                             
-- tree rep
treeTexp :: Texp -> String
treeTexp = treeTexp' 0 

treeTexp' :: Int -> Texp -> String
treeTexp' i (Texp value [])  = "\n" ++ (duplicate "  " i) ++ value
treeTexp' i (Texp value arr) = "\n" ++ (duplicate "  " i) ++ value ++ (concat $ map (treeTexp' (succ i)) arr)

-- paren rep
parenTexp :: Texp -> String
parenTexp (Texp value [])  = value
parenTexp (Texp value arr) = "(" ++ value ++ " " ++ (unwords (filter (not . all isSpace) $ map show arr)) ++ ")"
