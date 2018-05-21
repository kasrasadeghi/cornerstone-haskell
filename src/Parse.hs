module Parse where

import Data.Char
import Data.List
import Control.Arrow


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

char :: Char -> Parser (Maybe Char)
char c ""     = (Nothing, "")
char c (x:xs) = if x == c then (Just x, xs) else (Nothing, x:xs)

-- TODO Maybe Monad bind?
andThen :: Parser (Maybe Char) -> Parser (Maybe Char) -> Parser String
andThen f g s = case f s of
              (Nothing, s) -> ("", s)
              (Just fc, t) -> case g t of
                                (Nothing, t) -> ("", s)
                                (Just gc, t') -> ([fc, gc], t')

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
pProgram filename str =                         -- anyof [(== ""), (== ')') . head]
    let (texps, rest) = many (pTexp . pWhitespace) (\s -> s == "" || head s == ')') str in 
    Texp filename texps -- assert rest == ""

pTexp :: Parser Texp
pTexp str = if head str == '(' then pList (tail str) else pAtom str
            
pAtom :: Parser Texp
pAtom str = (Texp a [], b)
    where (a, b) = case head str of
                     '\'' -> pChar $ tail str
                     '"' -> pString $ tail str
                     _ -> pWord str

pChar :: Parser String
pChar str = let (char, rest) = span (== '\'') str in ("\'" ++ char ++ "\'", tail rest)

pString :: Parser String
pString str = let (string, rest) = span (== '\"') str in ("\"" ++ string ++ "\"", tail rest)
 -- splitAtFirst (\c -> c == '\"') str >>> second tail

pWord :: Parser String
pWord = span (\c -> c == '(' || c == ')' || isSpace c)
                                   
pList :: Parser Texp
pList str = -- TODO assertions
    let (value, afterValue) = pWord str in      -- anyof [(== ""), (== ')') . head]
    let (texps, rest) = many (pTexp . pWhitespace) (\s -> s == "" || head s == ')') afterValue in 
    (Texp value texps, tail rest) -- drop the ')'

pWhitespace :: String -> String
pWhitespace = dropWhile isSpace

------ experimental ------------------------------
safeHead l = if length l >= 1 then Just (head l) else Nothing
anyOf preds a = any (== True) (map ($ a) preds)
