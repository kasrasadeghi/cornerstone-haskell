module Pass where

import Parse

{-

The passes go in the following order:
 - blockify
 - becomeify
 - call as a stmt
{ - bindify
  - normalize }
 - qualify

-}

passes = becomeify . blockify
passes' = flattenDo . becomeify' . blockify

--------------------- BLOCKIFY ------------------------
    
blockify = blockifyDef . blockifyIf

-- prog
blockifyDef (Texp value tls) =
  Texp value $ map blockifyDefTL tls

-- toplevel
blockifyDefTL tl = case tl of
          {- def -}  Texp "def" (a:b:c:rest) -> Texp "def" (a:b:c:[Texp "do" rest])
                     _ -> tl

-- prog
blockifyIf (Texp value tls) =
  Texp value $ map blockifyIfTL tls

-- toplevel
blockifyIfTL tl = case tl of
         {- def -}  Texp "def" (a:b:c:rest) -> Texp "def" (a:b:c:map blockifyIfStmt rest)
                    _ -> tl

-- stmt
blockifyIfStmt stmt = case stmt of
         {- if-stmt -}  Texp "if" (cond:stmts) -> Texp "if" (cond:[Texp "do" (map blockifyIfStmt stmts)])
         {- do-stmt -}  Texp "do" stmts -> Texp "do" (map blockifyIfStmt stmts)
                        _ -> stmt
                                                       
--------------------- BECOMEIFY -----------------------

-- prog
becomeify (Texp value tls) =
  Texp value $ map becomeifyTL tls

-- toplevel
becomeifyTL tl = case tl of
                   Texp "def" [a, b, c, Texp "do" stmts] -> Texp "def" [a, b, c, Texp "do" (concatMap becomeifyStmt stmts)]
                   _ -> tl

becomeifyStmt stmt = case stmt of
                       Texp "become" [name, types, Texp "void" [], args] -> [Texp "call-tail" [name, types, Texp "void" [], args], Texp "return" [Texp "void" []]]
                       Texp "become" [name, types, return_type, args]      -> [Texp "return" [Texp "call-tail" [name, types, return_type, args], return_type]]
                       _ -> [stmt]

--------------------- BECOMEIFY ALTERNATE -------------

-- prog
becomeify' (Texp value tls) =
  Texp value $ map becomeifyTL' tls

-- toplevel
becomeifyTL' tl = case tl of
                   Texp "def" [a, b, c, Texp "do" stmts] -> Texp "def" [a, b, c, Texp "do" (map becomeifyStmt' stmts)]
                   _ -> tl

becomeifyStmt' stmt = case stmt of
                       Texp "become" [name, types, Texp "void" [], args] -> Texp "do" [Texp "call-tail" [name, types, Texp "void" [], args], Texp "return" [Texp "void" []]]
                       Texp "become" [name, types, return_type, args]      -> Texp "return" [Texp "call-tail" [name, types, return_type, args], return_type]
                       _ -> stmt

-- flattenDo should take in a block and then flatten any extraneous do's.
-- it should recurse through if-statements and inner-do statements

-- prog
flattenDo (Texp value tls) =
  Texp value $ map flattenDoTL tls

flattenDoTL tl = case tl of
                   Texp "def" [a, b, c, Texp "do" stmts] -> Texp "def" [a, b, c, Texp "do" $ concatMap flattenDoStmt stmts]
                   _ -> tl

flattenDoStmt stmt = case stmt of
                       Texp "do" children -> children
                       Texp "if" [cond, Texp "do" stmts] -> [Texp "if" [cond, Texp "do" $ concatMap flattenDoStmt stmts]]
                       _ -> [stmt]
