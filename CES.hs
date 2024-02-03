module CES where

{- This module is for functions relating to 
 - the CES machine including:
 -      - compiling a DeBruijn notation expression
 -      - doing a step on the CES machine
 -      - running a CES machine
 -}

import           Lib.AST

-- | ``autoGraderCompileToCes``
-- This function will be used by the autograder to test if your 
-- code is correct!
-- Please write this function so that it converts a DeBruijnTerm to 
-- a list of CES instructions...
--
-- See pg 16  of ``ces.pdf``
autoGraderCompileToCes :: DeBruijnTerm -> [Instr]
autoGraderCompileToCes = deBruijnToCes


deBruijnToCes :: DeBruijnTerm -> [Instr]
-- I just compile the deBruijn terms to CES instructions 
-- according to the table page 16 of the pdf doc
deBruijnToCes (Abs () t  ) = [IClo (deBruijnToCes t ++ [IRet], [])]
deBruijnToCes (App t1 t2 ) = deBruijnToCes t2 ++ deBruijnToCes t1 ++ [IApp]
deBruijnToCes (Var   i   ) = [IAccess i]
deBruijnToCes (Const i   ) = [IConst i]
deBruijnToCes (Add  t1 t2) = deBruijnToCes t2 ++ deBruijnToCes t1 ++ [IAdd]
deBruijnToCes (Mul  t1 t2) = deBruijnToCes t2 ++ deBruijnToCes t1 ++ [IMul]
deBruijnToCes (BLeq t1 t2) = deBruijnToCes t2 ++ deBruijnToCes t1 ++ [ILeq]
deBruijnToCes BTrue        = [ITrue]
deBruijnToCes BFalse       = [IFalse]
deBruijnToCes (BIf ift thent elset) =
    deBruijnToCes ift
        ++ [IIf (deBruijnToCes thent ++ [IRet], deBruijnToCes elset ++ [IRet])]

deBruijnToCes LNil = [INil]
deBruijnToCes (LCons a b) =
    deBruijnToCes b ++ deBruijnToCes a ++ [ICons Nothing]
deBruijnToCes (LCase t t0 (_, t1)) =
    deBruijnToCes t
        ++ [ICase (deBruijnToCes t0 ++ [IRet], deBruijnToCes t1 ++ [IRet])]
deBruijnToCes (FFix0 _ t) = [IFFix0 (deBruijnToCes t ++ [IRet], [])]
deBruijnToCes (FFix1 _ t) = [IFFix1 (deBruijnToCes t ++ [IRet], [])]

-- | ``autoGraderRunCes``
-- This function will be used by the autograder to test if your 
-- code is correct!
-- Please write this function so that it will run a CES machine
-- until the termination condition is reached (if it exists!)
--
-- See pg. 17 of ``ces.pdf``
autoGraderRunCes :: CES -> Instr
autoGraderRunCes (code, env, stack) = if null s then INil else head s
    where (c, e, s) = runCes (code, env, stack)

-- this function runs ces recursively until the list of codes becomes empty
-- I used the table at page 17 to implement it
-- for FixClo and FixcClo (that we didn't have any constructors for them in Instr)
-- I used IFFix1 and IFFix0 respectively, as it won't make any problems
-- in the process of running the machine
runCes :: CES -> CES
runCes ([]              , e, s) = ([], e, s)
runCes (IClo (c', _) : c, e, s) = runCes (c, e, IClo (c', e) : s)
runCes (IApp : c, e, IClo (c', e') : v : s) =
    runCes (c', v : e', IClo (c, e) : s)
runCes (IApp : c, e, IFFix1 (c', e') : v : s) =
    runCes (c', v : IFFix1 (c', e') : e', IClo (c, e) : s)
runCes (IApp : c, e, IFFix0 (c', e') : s) =
    runCes (c', IFFix0 (c', e') : e', IClo (c, e) : s)
runCes (IAccess n : c, e, s                    ) = runCes (c, e, e !! n : s)
runCes (IRet      : c, e, v : IClo (c', e') : s) = runCes (c', e', v : s)
runCes (IConst k  : c, e, s                    ) = runCes (c, e, IConst k : s)
runCes (IAdd : c, e, IConst n : IConst m : s) =
    runCes (c, e, IConst (n + m) : s)
runCes (IMul : c, e, IConst n : IConst m : s) =
    runCes (c, e, IConst (n * m) : s)
runCes (ILeq : c, e, IConst n : IConst m : s) =
    runCes (c, e, (if n <= m then ITrue else IFalse) : s)
runCes (ITrue          : c, e, s          ) = runCes (c, e, ITrue : s)
runCes (IFalse         : c, e, s          ) = runCes (c, e, IFalse : s)
runCes (IIf (c0, c1)   : c, e, ITrue : s  ) = runCes (c0, e, IClo (c, e) : s)
runCes (IIf (c0, c1)   : c, e, IFalse : s ) = runCes (c1, e, IClo (c, e) : s)
runCes (INil           : c, e, s          ) = runCes (c, e, INil : s)
runCes (ICons _ : c, e, v1 : v2 : s) = runCes (c, e, ICons (Just (v1, v2)) : s)
runCes (ICase (c1, c2) : c, e, INil : s   ) = runCes (c1, e, IClo (c, e) : s)
runCes (ICase (c1, c2) : c, e, ICons (Just (v1, v2)) : s) =
    runCes (c2, v2 : v1 : e, IClo (c, e) : s)
runCes (IFFix0 (c', _) : c , e, s) = runCes (c, e, IFFix0 (c', e) : s)
runCes (IFFix1 (c', _) : c , e, s) = runCes (c, e, IFFix1 (c', e) : s)
runCes (c              : c', e, s) = runCes (c', e, s)