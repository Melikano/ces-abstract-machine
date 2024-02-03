module DeBruijn where

{- This module is for functions relating to 
 - converting a LambdaTerm to its DeBruijnTerm..
 -}

import           Lib.AST
import           Lib.Monads

-- | ``autoGraderToDebruijn``
-- This function will be used by the autograder to test if your 
-- code is correct!
-- Please write this function so that it converts a LambdaTerm
-- to its corresponding DeBruijnTerm.. 
--
--  - You MUST start indexing at 0.
--
-- For example, the lambda term ``\x . x`` should be ``\ _ . #0``
-- where ``#0`` denotes the DeBruijn index and _ denotes the removed 
-- argument (note how from class we would normally just write 
-- ``\ . #0`` for the DeBruijn notation).
--
-- Moreover, this function should ``desugar`` multiple arguments
-- ONLY for lambda abstractions...
-- For example, the lambda term ``\a b c -> a`` should be changed 
-- to ``\a -> \b -> \c -> a``.
--
-- In the case that there is a free variable, please return:
--      ``Left "some helpful error message here"``.
autoGraderToDeBruijn :: LambdaTerm -> Either String DeBruijnTerm
autoGraderToDeBruijn = deBruijn []

-- takes a list (initially empty) as bound vars and a lambda term and converts the lambda term to deBruijn notation
deBruijn :: [String] -> LambdaTerm -> Either String DeBruijnTerm
-- if it is a Var then search for it in bound variables, if found return it's index else it is a free var and so return error
deBruijn boundVars (Var x) = case findIndex boundVars x of
    Nothing -> Left $ "free variable error: " ++ x
    Just i  -> Right (Var i)
-- if App t1 t2, then call the the function for t1 and t2 with bound vars stack
deBruijn boundVars (App t1 t2) = case (deBruijn boundVars t1) of
    Left  msg -> Left msg
    Right dt1 -> case (deBruijn boundVars t2) of
        Left  msg -> Left msg
        Right dt2 -> Right $ App dt1 dt2
-- if Abs, then update bound vars stack by pushing all bound vars in multibinder
-- then call the function recursively for it's term
deBruijn boundVars (Abs (b1, bs) t) = case (deBruijn updatedBoundVars t) of
    Left  msg -> Left msg
--  then add Abs for all existing bound vars (desuguring)
--  first map all bound vars to (Abs ()) and then apply them one after the other using fold 
    Right dt  -> Right $ foldr (\x y -> x y) (Abs () dt) abss
  where
    updatedBoundVars = reverse bs ++ b1 : boundVars
    abss             = map (\_ -> Abs ()) bs
-- if fix, then we have some bound variables again, so push them to bound vars stack
-- and call the function recursively for it's term with updated bound vars
deBruijn boundVars (FFix0 sb t) = case (deBruijn updatedBoundVars t) of
    Left  msg -> Left msg
    Right dt  -> Right $ FFix0 () dt
    where updatedBoundVars = sb : boundVars
-- really similar to fix0, just tow singlebinders here
deBruijn boundVars (FFix1 (sb1, sb2) t) = case (deBruijn updatedBoundVars t) of
    Left  msg -> Left msg
    Right dt  -> Right $ FFix1 ((), ()) dt
    where updatedBoundVars = sb2 : sb1 : boundVars
-- add is similar to map, just call function recursively
deBruijn boundVars (Add t1 t2) = case (deBruijn boundVars t1) of
    Left  msg -> Left msg
    Right dt1 -> case (deBruijn boundVars t2) of
        Left  msg -> Left msg
        Right dt2 -> Right $ Add dt1 dt2
-- just call function recursively for the inner terms
deBruijn boundVars (Mul t1 t2) = case (deBruijn boundVars t1) of
    Left  msg -> Left msg
    Right dt1 -> case (deBruijn boundVars t2) of
        Left  msg -> Left msg
        Right dt2 -> Right $ Mul dt1 dt2
-- just call function recursively for the inner terms
deBruijn boundVars (BLeq t1 t2) = case (deBruijn boundVars t1) of
    Left  msg -> Left msg
    Right dt1 -> case (deBruijn boundVars t2) of
        Left  msg -> Left msg
        Right dt2 -> Right $ BLeq dt1 dt2
-- just call function recursively for the inner terms
deBruijn boundVars (BIf t1 t2 t3) = case (deBruijn boundVars t1) of
    Left  msg -> Left msg
    Right dt1 -> case (deBruijn boundVars t2) of
        Left  msg -> Left msg
        Right dt2 -> case (deBruijn boundVars t3) of
            Left  msg -> Left msg
            Right dt3 -> Right $ BIf dt1 dt2 dt3
-- just call function recursively for the inner terms
deBruijn boundVars (LCons t1 t2) = case (deBruijn boundVars t1) of
    Left  msg -> Left msg
    Right dt1 -> case (deBruijn boundVars t2) of
        Left  msg -> Left msg
        Right dt2 -> Right $ LCons dt1 dt2
-- just call function recursively for the two first terms
-- we have binders again for third term, so push bound vars to stack
-- and then call function recursively for the third term
deBruijn boundVars (LCase t1 t2 (((sb1, sb2), t3))) =
    case (deBruijn boundVars t1) of
        Left  msg -> Left msg
        Right dt1 -> case (deBruijn boundVars t2) of
            Left  msg -> Left msg
            Right dt2 -> case deBruijn updatedBoundVars t3 of
                Left  msg -> Left msg
                Right dt3 -> Right $ LCase dt1 dt2 (((), ()), dt3)
                where updatedBoundVars = sb2 : sb1 : boundVars

-- and these don't need any changes!
deBruijn _ (Const i) = Right $ Const i
deBruijn _ BFalse    = Right BFalse
deBruijn _ BTrue     = Right BTrue
deBruijn _ LNil      = Right LNil


-- a helper function that takes an element of type a and a list of type a and 
-- returns the index of that element if it exists in the list
-- by zipping the list with [0..length list - 1] we get a list of tupples 
-- that each element has it's index along with itself
-- so we can return the index whenever we find the element
findIndex :: Eq a => [a] -> a -> Maybe Int
findIndex lst val = search val (zip lst [0 .. length lst - 1])  where
    search _ [] = Nothing
    search v ((x, i) : xs) | x == v    = Just i
                           | otherwise = search v xs