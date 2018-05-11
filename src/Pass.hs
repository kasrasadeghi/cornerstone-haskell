module Pass where

import Parse
    
blockifyDef (Texp value tls) =
    Texp value $ (flip map) tls (\tl -> case tl of
                                          (Texp "def" (a:b:c:rest)) -> (Texp "def" (a:b:c:[(Texp "do" rest)]))
                                          _ -> tl)
                                                
-- blockifyTL tl = case tl of
--                  (Texp "def" (a:b:c:rest)) -> (Texp "def" (a:b:c:[(Texp "do" rest)]))
--                  _ -> tl

blockifyIf (Texp value tls) =
    Texp value $ (flip map) tls
