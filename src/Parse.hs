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

pProgram :: String -> String -> Texp
pProgram filename str =
    let (texps, rest) = many (pTexp . pWhitespace) (\s -> s == "" || head s == ')') [] str in 
    Texp filename texps -- assert rest == ""

many :: Parser Texp -> (String -> Bool) -> [Texp] -> Parser [Texp]
many parse until acc str =
    if until str then
        (acc, str)
    else let (texp, rest) = parse str in
         many parse until (acc ++ [texp]) rest

pTexp :: Parser Texp
pTexp str = if head str == '(' then pList (tail str) else pAtom str

splitAtFirst :: (Char -> Bool) -> String -> (String, String)
splitAtFirst pred str = case findIndex pred str of
                          Nothing -> ([], str)
                          Just i -> (take i str, drop i str)
            
pAtom :: Parser Texp
pAtom str = (Texp a [], b)
    where (a, b) = case head str of
                     '\'' -> pChar $ tail str
                     '"' -> pString $ tail str
                     _ -> pWord str

pChar :: Parser String
pChar str = let (char, rest) = splitAtFirst (\c -> c == '\'') str in ("\'" ++ char ++ "\'", tail rest)

pString :: Parser String
pString str = let (string, rest) = splitAtFirst (\c -> c == '\"') str in ("\"" ++ string ++ "\"", tail rest)
 -- splitAtFirst (\c -> c == '\"') str >>> second tail

pWord :: Parser String
pWord str = splitAtFirst (\c -> c == '(' || c == ')' || isSpace c) str
                                   
pList :: Parser Texp
pList str = -- TODO assertions
    let (value, afterValue) = pWord str in
    let (texps, rest) = many (pTexp . pWhitespace) (\s -> head s == ')' || s == "") [] afterValue in 
    (Texp value texps, tail rest) -- drop the ')'

pWhitespace :: String -> String
pWhitespace str = dropWhile isSpace str
