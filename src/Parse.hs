module Parse where

import Data.Char
import Data.List
import Control.Arrow
import Control.Monad

data Texp = Texp String [Texp]
type Parser a = String -> (a, String)

------ parser combinators ------------------------

-- many parser combinator
many :: Parser a -> (String -> Bool) -> Parser [a]
many parse until str =
    let loop parse until str acc = 
            if until str then (acc, str)
            else
                let (x, rest) = parse str in
                loop parse until rest (acc ++ [x])
    in loop parse until str []

char :: Char -> Parser String
char c ""     = ("", "")
char c (x:xs) = if x == c then ([x], xs) else ("", x:xs)

-- TODO Maybe Monad bind?
andThen :: Parser String -> Parser String -> Parser String
andThen f g = f >=> g

-- TODO
-- string :: String -> Parser String
-- string s = 

count :: Parser a -> (String -> Bool) -> Parser Int
count parse until str = let (parses, rest) = many parse until str in (length parses, rest)

------ inverse tree representation ---------------

pTab :: Parser String
pTab = andThen (char ' ') (char ' ')

tabCount :: String -> (Int, String)
tabCount = count pTab $ not . isPrefixOf "  "

unshow :: String -> Texp
unshow = head . childify . map tabCount . lines
  -- assert that childify produces a list of size 1 when unshowing a parse texp

-- TODO maybe: implement with many?
childify :: [(Int, String)] -> [Texp]
childify [] = []
childify ((l, value):st) =  -- assert l == 0
  let
    st' = map (first pred) st
    p = (>= 0) . fst
    (children, rest) = second (map (first succ)) $ span p st'
  in
  (Texp value (childify children)):childify rest

------ parser ------------------------------------

parse :: String -> IO Texp
parse filename = do
  str <- readFile filename
  putStrLn str
  return (pProgram filename str)

pProgram :: String -> String -> Texp
pProgram filename str =                         
    let                   -- we should strip input                      -- anyof [(== ""), (== ')') . head]
        (texps, rest) = many (second pWhitespace . pTexp) (\s -> s == "" || head s == ')') str
    in Texp filename texps -- assert rest == ""

pTexp :: Parser Texp
pTexp "" = error "parsing texp on empty string"
pTexp (h:t) = if h == '(' then pList t else pAtom (h:t)
            
pAtom :: Parser Texp
pAtom str = (Texp a [], b)
    where (a, b) = case head str of
                     '\'' -> pChar $ tail str
                     '"' -> pString $ tail str
                     _ -> pWord str

splitAtFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitAtFirst pred str = case findIndex pred str of
                          Nothing -> ([], str)
                          Just i -> (take i str, drop i str)
                          
pChar :: Parser String
pChar str = let (char, rest) = splitAtFirst (== '\'') str in ("\'" ++ char ++ "\'", tail rest)

pString :: Parser String
pString str = let (string, rest) = splitAtFirst (== '\"') str in ("\"" ++ string ++ "\"", tail rest)
 -- splitAtFirst (\c -> c == '\"') str >>> second tail

pWord :: Parser String
pWord = splitAtFirst (\c -> c == '(' || c == ')' || isSpace c)
                                   
pList :: Parser Texp
pList str = -- TODO assertions
    let
        (value, afterValue) = pWord str         -- anyof [(== ""), (== ')') . head]
        (texps, rest) = many (pTexp . pWhitespace) (\s -> s == "" || head s == ')') afterValue
    in
      (Texp value texps, tail rest) -- drop the ')'

pWhitespace :: String -> String
pWhitespace = dropWhile isSpace

------ experimental ------------------------------
safeHead l = if length l >= 1 then Just (head l) else Nothing
anyOf preds a = any (== True) (map ($ a) preds)
