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

--------------------- BLOCKIFY ------------------------
    
blockify = blockifyDef . blockifyIf

-- prog
blockifyDef (Texp value tls) =
  Texp value $ (flip map) tls blockifyDefTL

-- toplevel
blockifyDefTL tl = case tl of
          {- def -}  Texp "def" (a:b:c:rest) -> Texp "def" (a:b:c:[(Texp "do" rest)])
                     _ -> tl

-- prog
blockifyIf (Texp value tls) =
  Texp value $ (flip map) tls blockifyIfTL

-- toplevel
blockifyIfTL tl = case tl of
         {- def -}  Texp "def" (a:b:c:rest) -> Texp "def" (a:b:c:(map blockifyIfStmt rest))
                    _ -> tl

-- stmt
blockifyIfStmt stmt = case stmt of
         {- if-stmt -}  (Texp "if" (cond:stmts)) -> (Texp "if" (cond:[Texp "do" (map blockifyIfStmt stmts)]))
         {- do-stmt -}  (Texp "do" stmts) -> (Texp "do" (map blockifyIfStmt stmts))
                        _ -> stmt
                                                       
--------------------- BECOMEIFY -----------------------

-- prog
becomeify (Texp value tls) =
  Texp value $ (flip map) tls becomeifyTL

-- toplevel
becomeifyTL tl = case tl of
                   Texp "def" [a, b, c, (Texp "do" stmts)] -> Texp "def" [a, b, c, (Texp "do" (map becomeifyStmt stmts))]
                   _ -> tl

becomeifyStmt stmt = case stmt of
                       Texp "become" [name, types, (Texp "void" []), args] -> Texp "do" [Texp "call-tail" [name, types, Texp "void" [], args], Texp "return" [Texp "void" []]]
                       Texp "become" [name, types, return_type, args] -> Texp "return" [Texp "call-tail" [name, types, return_type, args], args]
                       _ -> stmt
