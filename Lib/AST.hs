module Lib.AST where

import Data.List
import Control.Monad
import Lib.Monads

{- AST.hs
 - This file includes the data type of the AST of the lambda terms you should use,
 - and the data type of the CES code you should use as well.
 -
 - Moreover, this includes the function ``termPrettyPrint`` for pretty showing
 - the data type of the lambda term.
 -}

-- A type alias for a user inputted lambda term
type LambdaTerm = Term String (String, [String]) String 

-- A type alias the DeBruijn indexed type of a 
-- lambda term.
type DeBruijnTerm = Term () () Int

-- | Term singlebinder multibinder var
-- The type of a lambda term.
-- Some notes on the type definition -- for ``Term singlebinder multibinder var``, 
--      - the type variable ``singlebinder`` is the type of an argument 
--          which binds exactly one variable
--      - the type variable ``multibinder`` is the type of an argument 
--          which binds one or more variables.
--      - the type variable ``var`` is the type of a variable used in the body of 
--            a lambda term
data Term singlebinder multibinder var =
    -- A lambda abstraction
    Abs multibinder (Term singlebinder multibinder var)
    -- An application
    | App (Term singlebinder multibinder var) (Term singlebinder multibinder var)
    -- A variable
    | Var var

    -- A constant value
    | Const Int
    -- Primitive addition 
    | Add (Term singlebinder multibinder var) (Term singlebinder multibinder var)
    -- Primitive multiplication 
    | Mul (Term singlebinder multibinder var) (Term singlebinder multibinder var)

    -- Primitive if
    | BIf (Term singlebinder multibinder var) (Term singlebinder multibinder var) (Term singlebinder multibinder var)
    -- Primitive Leq 
    | BLeq (Term singlebinder multibinder var) (Term singlebinder multibinder var)
    -- Primitive True
    | BTrue
    -- Primitive False
    | BFalse

    -- fixed point combinators for a function which takes 0 arguments
    -- and 1 argument.
    -- (see the pg.19 of ces.pdf for more details). 
    -- This corresponds to Fixc:
    | FFix0 singlebinder (Term singlebinder multibinder var)
    -- This corresponds to Fix:
    | FFix1 (singlebinder, singlebinder) (Term singlebinder multibinder var)

    -- Primitive list
    | LNil
    -- Primitive cons
    | LCons (Term singlebinder multibinder var) (Term singlebinder multibinder var)  --
    -- Primitive list case
    | LCase 
        -- (term to pattern match on)
        (Term singlebinder multibinder var)                
        -- (what to output in the LNil case)
        (Term singlebinder multibinder var)                 
        -- ( a : b, term to output where ``a`` and ``b`` are bound to the constructor)
        ((singlebinder, singlebinder), Term singlebinder multibinder var)
  deriving (Show, Eq)


-- Type aliases for the ``Code``, ``Env``, and ``Stack``
-- of a CES machine respectively..
type Code = [Instr]
type Env = [Instr]
type Stack = [Instr]

-- Type alias for a CES machine
type CES = (Code, Env, Stack)


-- The ``Instr`` type is the instructions used for the
-- CES machine..
-- See pg 17 of ``ces.pdf``
data Instr = 
    IClo (Code, Env) -- ( code, environment )
    | IApp 
    | IAccess Int
    | IRet

    | IConst Int
    | IAdd 
    | IMul
    | ILeq

    | ITrue
    | IFalse
    | IIf (Code, Code)

    -- 0 arity fix
    | IFFix0 (Code, Env) -- ( code, environment )
    -- 1 arity fix
    | IFFix1 (Code, Env) -- ( code, environment )

    | ICons (Maybe (Instr, Instr))
    | INil 
    | ICase (Code, Code) -- (the [] case, the (_:_) case)
  deriving (Show, Eq)


-- | Function for specializing pretty printing for a lambda term
lambdaTermPrettyPrint :: LambdaTerm -> String
lambdaTermPrettyPrint = termPrettyPrint

-- | Function for specializing pretty printing for a De Bruijn term
deBruijnTermPrettyPrint :: LambdaTerm -> String
deBruijnTermPrettyPrint = termPrettyPrint


--------------------------------------
-- Everything past here, you don't need to worry about..
--------------------------------------

-- | TermPrettyPrinter is a pretty printer for a ``Term a b``.
-- Write 
--      `` termPrettyPrint (someTerm :: LambdaTerm)`` 
--      or
--      `` termPrettyPrint (someTerm :: DeBruijnTerm)`` 
-- to get the pretty printed string
-- If you are in GHCI, you can just print it by typing:
--      `` putStrLn $ termPrettyPrint (someTerm :: LambdaTerm)`` 
--      or
--      `` putStrLn $ termPrettyPrint (someTerm :: DeBruijnTerm)`` 
-- which will print the escaped characters as they are meant to be 
-- seen.
-- Please report bugs!
--
-- As a future TODO, this code is a bit messy and needs to be cleaned
-- up... ``TermPrettyPrinter" is really just used for pretty printing
-- multiple function args (and the whole thing), and ``TermValuePrinter"
-- should be used for printing individual args (very loosely how this
-- should be written)...
class TermPrettyPrinter a where
    termPrettyPrint :: a -> String

class TermValuePrinter a where
    termValuePrint :: a -> String

instance TermValuePrinter Char where
    termValuePrint = pure

instance TermValuePrinter a => TermValuePrinter [a] where
    termValuePrint = concatMap termValuePrint

instance TermValuePrinter () where
    termValuePrint = const "_"

-- This is only called when calling a DeBruijn index
instance TermValuePrinter Int where
    termValuePrint n = "#"++ show n 

instance TermValuePrinter a => TermPrettyPrinter [a] where
    termPrettyPrint = intercalate " " . map termValuePrint

instance TermPrettyPrinter () where
    termPrettyPrint = const ""

-- We only use tuples of (a,b) as a multiarg binder..
-- Honestly, this is a bit of a weird hack that could be fixed with
-- type families -- but alas, we will avoid using GHC extensions..
-- Also, some strange hacks to get the spacing a bit prettier..
instance (TermValuePrinter a, TermPrettyPrinter b) => TermPrettyPrinter (a, b) where
    termPrettyPrint (a, b) = termValuePrint a ++ b'
      where
        b' = case termPrettyPrint b of
            [] -> []
            pb -> ' ' : pb

instance (TermValuePrinter a, TermPrettyPrinter b) => TermValuePrinter (a, b) where
    termValuePrint (a,b) = termValuePrint a ++ b'
      where
        b' = case termPrettyPrint b of
            [] -> []
            pb -> ' ' : pb


-- functions for inserting the brackets in 
-- the right spots..
bAssocParen :: String -> String
bAssocParen str = concat ["(", str, ")"]

appAssocL lt@(l :< lp) = ContT $ \k -> case lp of
    VarF _ -> return lt
    ConstF _ -> return lt
    BTrueF -> return lt
    BFalseF -> return lt
    LNilF -> return lt
    AppF _ _ -> return lt
    _ -> k lt

appAssocR rt@(r :< rp) = ContT $ \k -> case rp of
    VarF _ -> return rt
    ConstF _ -> return rt
    BTrueF -> return rt
    BFalseF -> return rt
    LNilF -> return rt
    -- AbsF _ _ -> return rt
    _ -> k rt

mulAssocL rt@(r :< rp) = ContT $ \k -> case rp of
    MulF _ _ -> return rt
    _ -> k rt
mulAssocR rt@(r :< rp) = ContT $ \k -> case rp of
    AppF _ _ -> return rt
    _ -> k rt

addAssocL rt@(r :< rp) = ContT $ \k -> case rp of
    AddF _ _ -> return rt
    _ -> k rt
addAssocR rt@(r :< rp) = ContT $ \k -> case rp of
    MulF _ _ -> return rt
    _ -> k rt

consAssocL rt@(r :< rp) = ContT $ \k -> case rp of
    _ -> k rt
consAssocR rt@(r :< rp) = ContT $ \k -> case rp of
    LConsF _ _ -> return rt
    AddF _ _ -> return rt
    _ -> k rt

leqAssoc rt@(r :< rp) = ContT $ \k -> case rp of
    _ -> k rt

assocParen (a :< as) =  return $ concat ["(",a,")"] :< as

instance 
    ( TermPrettyPrinter a
    , TermValuePrinter a
    , TermPrettyPrinter b
    , TermValuePrinter b
    , TermValuePrinter c ) =>
    TermPrettyPrinter (Term a b c) where
    termPrettyPrint = histo f 
      where
        runAssocParen ct = case runCont ct id of
            str :< _ -> str

        f :: 
            ( TermPrettyPrinter a
            , TermValuePrinter a
            , TermPrettyPrinter b
            , TermValuePrinter b
            , TermValuePrinter c
            ) =>
            TermF a b c (Cofree (TermF a b c) String) -> String
        f term = case term of
            AbsF arg (bdy :< _) -> concat    
                [ "\\"
                -- , intercalate " " $ map termPrettyPrint $ uncurry (:) arg
                , termValuePrint arg
                , " -> "
                , bdy
                ]
            {- infixity copies Haskell, so we have:
                infix 4 <=
                infixr 5 :
                infixl 6 +
                infixl 7 *
             and function application has the highest precedence.
             -}

            AppF l r -> concat 
                [ runAssocParen (appAssocL >=> assocParen $ l)
                , " "
                , runAssocParen (appAssocR >=> assocParen $ r)
                ]
            MulF l r -> concat
                [ runAssocParen (appAssocL >=> mulAssocL >=> assocParen $ l)
                , " * "
                , runAssocParen (appAssocR >=> mulAssocR >=> assocParen $ r)
                ]

            AddF l r -> concat
                [ runAssocParen (appAssocL >=> mulAssocL >=> addAssocL >=> assocParen $ l)
                , " + "
                , runAssocParen (appAssocR >=> mulAssocR >=> addAssocR >=> assocParen $ r)
                ]
            LConsF l r -> concat
                [ runAssocParen (appAssocL >=> mulAssocL >=> addAssocL >=> consAssocL >=> assocParen $ l)
                , " : "
                , runAssocParen (appAssocR >=> mulAssocR >=> addAssocR >=> consAssocR >=> assocParen $ r)
                ]
            BLeqF l r  -> concat
                [ runAssocParen (appAssocL >=> mulAssocL >=> addAssocL >=> consAssocL >=> leqAssoc >=> assocParen $ l)
                , " <= "
                , runAssocParen (appAssocR >=> mulAssocR >=> addAssocR >=> consAssocR >=> leqAssoc >=> assocParen $ r)
                ]

            VarF v -> termValuePrint v 
            -- just show constants.. No need to use
            -- to use ``termValuePrint`` since ``termValuePrint``
            -- would expect a DeBruijn index to print not just an 
            -- Int
            ConstF v -> show v


            BIfF (cond :< _) (thenc :< _) (elsec :< _) -> intercalate " "
                [ "if"
                , cond
                , "then"
                , thenc
                , "else"
                , elsec
                ]


            BTrueF -> "True"
            BFalseF -> "False"

            FFix1F (a, b) (bdy :< _) -> concat
                [ "fix"
                , " ("
                , "\\"
                , termValuePrint a
                , " "
                , termValuePrint b
                , " -> "
                , bdy
                , ")"
                ]

            FFix0F a (bdy :< _) -> concat
                [ "fix"
                , " ("
                , "\\"
                , termValuePrint a
                , " -> "
                , bdy
                , ")"
                ]

            LNilF -> "[]"
            LCaseF (caseon :< _) (nilc :< _) ((arg0,arg1), consc :< _) -> intercalate " "
                [ "lcase"
                , caseon
                , "of"
                , "[]"
                , "->"
                , nilc
                , ";"
                , termValuePrint arg0
                , ":"
                , termValuePrint arg1
                , "->"
                , consc
                ]

----------------------------
-- Everything past here is just some helper functions for working with
-- recursive types in general. You don't need to worry about this :D
----------------------------
cata :: (TermF a b c z -> z) -> Term a b c -> z
cata f = f . fmap g . project 
  where
    g term = cata f term

para :: (TermF a b c (Term a b c, z) -> z) -> Term a b c -> z
para f = f . fmap g . project 
  where
    g term = (term, para f term)

data Cofree f a = a :< f (Cofree f a)

instance Functor f => Functor (Cofree f) where
    fmap f (a :< as) = f a :< fmap (fmap f) as
    

histo :: (TermF a b c (Cofree (TermF a b c) z) -> z) -> Term a b c -> z
histo f = g . h 
  where
    g (v :< _) = v

    h term = uncurry (:<) 
        $ (\termf -> (f termf, termf))
        $ fmap h (project term)

project :: Term a b c -> TermF a b c (Term a b c)
project (Abs a_a1wl0 a_a1wl1) = (AbsF a_a1wl0) a_a1wl1
project (App a_a1wl2 a_a1wl3) = (AppF a_a1wl2) a_a1wl3
project (Var a_a1wl4) = VarF a_a1wl4 
project (Const a_a1wl5) = ConstF a_a1wl5
project (Add a_a1wl6 a_a1wl7) = (AddF a_a1wl6) a_a1wl7
project (Mul a_a1wl8 a_a1wl9) = (MulF a_a1wl8) a_a1wl9
project (BIf a_a1wla a_a1wlb a_a1wlc) = ((BIfF a_a1wla) a_a1wlb) a_a1wlc
project (BLeq a_a1wld a_a1wle) = (BLeqF a_a1wld) a_a1wle
project BTrue = BTrueF
project BFalse = BFalseF
project (FFix0 a_a1wlf a_a1wlg) = (FFix0F a_a1wlf) a_a1wlg
project (FFix1 a_a1wlh a_a1wli) = (FFix1F a_a1wlh) a_a1wli
project LNil = LNilF
project (LCons a_a1wlj a_a1wlk) = (LConsF a_a1wlj) a_a1wlk
project (LCase a_a1wll a_a1wlm a_a1wln) = ((LCaseF a_a1wll) a_a1wlm) a_a1wln

embed :: TermF a b c (Term a b c) -> Term a b c
embed (AbsF a_ac8U a_ac8V) = (Abs a_ac8U) a_ac8V
embed (AppF a_ac8W a_ac8X) = (App a_ac8W) a_ac8X
embed (VarF a_ac8Y) = Var a_ac8Y
embed (ConstF a_ac8Z) = Const a_ac8Z
embed (AddF a_ac90 a_ac91) = (Add a_ac90) a_ac91
embed (MulF a_ac92 a_ac93) = (Mul a_ac92) a_ac93
embed (BIfF a_ac94 a_ac95 a_ac96) = ((BIf a_ac94) a_ac95) a_ac96
embed (BLeqF a_ac97 a_ac98) = (BLeq a_ac97) a_ac98
embed BTrueF = BTrue
embed BFalseF = BFalse
embed (FFix0F a_ac99 a_ac9a) = (FFix0 a_ac99) a_ac9a
embed (FFix1F a_ac9b a_ac9c) = (FFix1 a_ac9b) a_ac9c
embed LNilF = LNil
embed (LConsF a_ac9d a_ac9e) = (LCons a_ac9d) a_ac9e
embed (LCaseF a_ac9f a_ac9g a_ac9h) = ((LCase a_ac9f) a_ac9g) a_ac9h

data TermF singlebinder_a1w8b multibinder_a1w8c var_a1w8d rec_a1wkZ
  = AbsF multibinder_a1w8c rec_a1wkZ |
    AppF rec_a1wkZ rec_a1wkZ |
    VarF var_a1w8d |
    ConstF Int |
    AddF rec_a1wkZ rec_a1wkZ |
    MulF rec_a1wkZ rec_a1wkZ |
    BIfF rec_a1wkZ rec_a1wkZ rec_a1wkZ |
    BLeqF rec_a1wkZ rec_a1wkZ |
    BTrueF |
    BFalseF |
    FFix0F singlebinder_a1w8b rec_a1wkZ |
    FFix1F (singlebinder_a1w8b, singlebinder_a1w8b) rec_a1wkZ |
    LNilF |
    LConsF rec_a1wkZ rec_a1wkZ |
    LCaseF rec_a1wkZ rec_a1wkZ ((singlebinder_a1w8b,
                                 singlebinder_a1w8b),
                                rec_a1wkZ)

instance Functor (TermF a b c) where
    fmap f_afeD (AbsF a1_afeE a2_afeF) = AbsF ((\ b1_afeG -> b1_afeG) a1_afeE) (f_afeD a2_afeF)
    fmap f_afeH (AppF a1_afeI a2_afeJ) = AppF (f_afeH a1_afeI) (f_afeH a2_afeJ)
    fmap f_afeK (VarF a1_afeL) = VarF ((\ b1_afeM -> b1_afeM) a1_afeL)
    fmap f_afeN (ConstF a1_afeO) = ConstF ((\ b1_afeP -> b1_afeP) a1_afeO)
    fmap f_afeQ (AddF a1_afeR a2_afeS) = AddF (f_afeQ a1_afeR) (f_afeQ a2_afeS)
    fmap f_afeT (MulF a1_afeU a2_afeV) = MulF (f_afeT a1_afeU) (f_afeT a2_afeV)
    fmap f_afeW (BIfF a1_afeX a2_afeY a3_afeZ) = BIfF (f_afeW a1_afeX) (f_afeW a2_afeY) (f_afeW a3_afeZ)
    fmap f_aff0 (BLeqF a1_aff1 a2_aff2) = BLeqF (f_aff0 a1_aff1) (f_aff0 a2_aff2)
    fmap f_aff3 BTrueF = BTrueF 
    fmap f_aff4 BFalseF = BFalseF
    fmap f_aff5 (FFix0F a1_aff6 a2_aff7) = FFix0F ((\ b1_aff8 -> b1_aff8) a1_aff6) (f_aff5 a2_aff7)
    fmap f_aff9 (FFix1F a1_affa a2_affb) = FFix1F ((\ b1_affc -> b1_affc) a1_affa) (f_aff9 a2_affb)
    fmap f_affd LNilF = LNilF
    fmap f_affe (LConsF a1_afff a2_affg) = LConsF (f_affe a1_afff) (f_affe a2_affg)
    fmap f_affh (LCaseF a1_affi a2_affj a3_affk) = LCaseF
          (f_affh a1_affi) (f_affh a2_affj)
          ((\ b2_affl
              -> case b2_affl of {
                   ((,) a1_affm a2_affn)
                     -> (,) ((\ b1_affo -> b1_affo) a1_affm) (f_affh a2_affn) })
             a3_affk)

