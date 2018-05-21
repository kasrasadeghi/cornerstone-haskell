module Pass where

import Parse

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

                                                       
------------------ 
