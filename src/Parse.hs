module Parse where

import Data.List
import Data.Char

type Parser a = String -> (a, String)
    
data Texp = Texp String [Texp]
instance Show Texp where
    show (Texp value [])  = value
    show (Texp value arr) = "(" ++ value ++ " " ++ (unwords (filter (not . all isSpace) $ map show arr)) ++ ")"

parse :: String -> IO Texp
parse filename = do
  str <- readFile filename
  putStrLn str
  return (pProgram filename str)

-- Texp filename [Texp "my" [], Texp "name" [], Texp "is" [], Texp "" []]
pProgram :: String -> String -> Texp
pProgram filename str = Texp filename [fst $ pTexp (pWhitespace str)]

many :: (String -> (Texp, String)) -> (String -> Bool) -> (String -> [Texp] -> ([Texp], String))
many parse until str acc = if until str then
                               (acc, str)
                           else let (texp, rest) = parse str in
                                many parse until rest (acc ++ [texp])

pTexp :: String -> (Texp, String)
pTexp str = if head str == '(' then pList (tail str) else pAtom str

splitAtFirst :: (Char -> Bool) -> String -> (String, String)
splitAtFirst pred str = case findIndex pred str of
                          Nothing -> ([], str)
                          Just i -> (take i str, drop i str)
            
pAtom :: String -> (Texp, String)
pAtom str = (Texp a [], b)
    where (a, b) = case head str of
                     -- '\'' -> pChar str
                     -- '"' -> pString str
                     _ -> pWord str

pChar :: String -> (Texp, String)
pChar 

pWord :: String -> (String, String)
pWord str = splitAtFirst (\c -> c == '(' || c == ')' || isSpace c) str

pList :: String -> (Texp, String)
pList str = let (value, afterValue) = pWord str in
            let (texp, rest) = (pTexp . pWhitespace) afterValue in
            (Texp value [texp], drop 1 rest) -- drop the ')'

pWhitespace :: String -> String
pWhitespace str = dropWhile isSpace str
