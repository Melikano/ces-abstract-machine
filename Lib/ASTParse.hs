module Lib.ASTParse where

import Text.ParserCombinators.ReadP (ReadP, (<++))
import qualified Text.ParserCombinators.ReadP as ReadP

import Data.Char
import Data.Functor
import Data.Maybe

import qualified Lib.AST as AST
import Lib.AST (LambdaTerm)

{- This includes a collection of functions used to parse a lambda term..
 -
 - How to use:
 -      ``unsafeParseLambdaTerm <some lambda term here>``
 - and this should return a LambdaTerm. If the parse fails,
 - then this will throw an error.
 -
 - Known bugs:
 -      - Error messages are terrible -- the author (Jared) wanted
 -       to try using the ReadP parser combinators from ``base`` Haskell,
 -       but it turns out that these combinators don't support having 
 -       error messages.
 -
 - Please report any other bugs to the author (Jared) and 
 - we can hopefully get them resolved!
 -
 - See ``Examples.hs`` for examples of programs that it parses...
 -}

----------------------------
-- Functions to parse lambda term..
----------------------------
unsafeParseLambdaTerm :: String -> LambdaTerm
unsafeParseLambdaTerm inp = case parseLambdaTerm inp of
    Just res -> res
    Nothing -> error "Parse error."

parseLambdaTerm :: String -> Maybe LambdaTerm
parseLambdaTerm inp = case ReadP.readP_to_S (junk *> term0 <* ReadP.eof) inp of
    [(ast, [])] -> Just ast
    _ -> Nothing

----------------------------
-- Some basic parser definitions for
-- this parser built on top of 
-- Text.ParserCombinators.ReadP
----------------------------
junk :: ReadP ()
junk = 
    ReadP.skipSpaces
    *> void (many (ReadP.string "--" 
            *> (ReadP.munch (/='\n'))
            *> ReadP.skipSpaces))

token :: ReadP a -> ReadP a
token pa  = pa <* junk

symbol :: String -> ReadP String
symbol = token . ReadP.string

keywords :: [String] 
keywords = ["fix", "if", "then", "else", "of", "lcase", "True", "False"]

ident :: ReadP String
ident = token $ do
    inp <- ReadP.look
    case inp of
        c:_ 
            | isAlpha c -> do 
                cs <- ReadP.munch1 isAlphaNum
                if cs `notElem` keywords
                    then return cs
                    else ReadP.pfail
            | otherwise -> ReadP.pfail
        [] -> ReadP.pfail

-- the default readP chainl1, and chainr1 use ``+++" (i.e., symmetric choice)
-- which is not what we want -- we want to use <++ biased choice,
-- since we are only interested in the ``longest parse``
-- not any intermediate parses which may be successful..
chainl1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainl1 pa pop = pa >>= rst
  where
    rst acc = (do  
        op <- pop
        b <- pa
        rst (op acc b))
            <++ pure acc

chainr1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainr1 pa pop = pa >>= rst
  where
    rst a = (do
        op <- pop
        acc <- chainr1 pa pop
        return (op a acc))
            <++ pure a

-- similarly, with option
option :: a -> ReadP a -> ReadP a
option x p = p <++ pure x

optional :: ReadP a -> ReadP ()
optional p = (p *> pure ()) <++ pure ()

many :: ReadP a -> ReadP [a]
many p = many1 p <++ pure [] 

many1 :: ReadP a -> ReadP [a]
many1 p = (:) <$> p <*> many p


----------------------------
-- Parsing the lambda term
----------------------------
{- infixity copies Haskell, so we have:
    infix 4 <=
    infixr 5 :
    infixl 6 +
    infixl 7 *
 and function application has the highest precedence.
 -}

term0 :: ReadP LambdaTerm
term0 = do
    a <- terms1 
    b <- option Nothing (fmap Just $ symbol "<=" *> terms1)
    maybe (pure a) (pure . AST.BLeq a) b

terms1 :: ReadP LambdaTerm
terms1 = chainr1 terms2 $ symbol ":" $> AST.LCons 

terms2 :: ReadP LambdaTerm
terms2 = chainl1 terms3 $ (symbol "+" $> AST.Add)

terms3 :: ReadP LambdaTerm
terms3 = chainl1 terms4 $ (symbol "*" $> AST.Mul)

terms4 :: ReadP LambdaTerm
-- terms4 = chainl1 term5 $ (pure AST.App)
-- Need to do a little more work to get the
-- associativity right for certain special operators
-- like: if-then-else, lcase, fix
terms4 = term5 >>= rst
  where
    rst acc = (do
            a <- term5'
            rst (AST.App acc a)
        ) <++ pure acc

-- This is the ``first" term5..
-- note that the first one has all the right associativity
-- options of each of the operators..
term5 :: ReadP LambdaTerm
term5 = ReadP.choice
    [ absterm   -- this should be moved up
    , varterm
    , trueterm
    , falseterm
    , constterm
    , listterm
    , ifterm
    , listcaseterm
    , fixterm
    , ReadP.between (symbol "(") (symbol ")") term0
    ]

-- this is the ``second" (or any after) term5s.
-- Note that we need to shift around the grammar a bit
-- to force the right brackets and associativity of the operators..
-- In particular, we want to reject things like ``a fix a`` which would
-- otherwise necessarily parse as ``a (fix a)``...
-- We do this by allowing a parse with things that DO NOT necessarily
-- need brackets, then using the original parsing mechanism in the beginning
-- to allow a parse of anything in brackets
term5' :: ReadP LambdaTerm
term5' = ReadP.choice
    [ varterm
    , trueterm
    , falseterm
    , constterm
    , listterm
    , ReadP.between (symbol "(") (symbol ")") $ term0
    ]
        

funargs :: ReadP (String, [String])
funargs = (,) 
    <$> (symbol "\\" *> ident )
    <*> (many ident <* (symbol "." <++ symbol "->"))

absterm :: ReadP LambdaTerm
absterm = AST.Abs <$> funargs <*> term0

varterm :: ReadP LambdaTerm
varterm = AST.Var <$> ident

trueterm :: ReadP LambdaTerm
trueterm = symbol "True" $> AST.BTrue

falseterm :: ReadP LambdaTerm
falseterm = symbol "False" $> AST.BFalse

fixterm :: ReadP LambdaTerm
fixterm = symbol "fix" *> (fixbody <++ ReadP.between (symbol "(") (symbol ")") fixbody)
  where
    fixbody = do
        (r, rs) <- funargs
        bdy <- term0
        case rs of
            [] -> return $ AST.FFix0 r bdy
            -- t:ts -> return $ AST.FFix1 (r, t) $ AST.Abs ts bdy
            [t] -> return $ AST.FFix1 (r, t) bdy
            t0:t1:ts -> return $ AST.FFix1 (r, t0) $ AST.Abs (t1, ts) bdy
            -- _ -> ReadP.pfail

constterm :: ReadP LambdaTerm
constterm = token 
        $ fmap (AST.Const . (read :: String -> Int)) 
        $ option id (ReadP.char '-' $> ('-':)) 
            <*> ReadP.munch1 isDigit

ifterm :: ReadP LambdaTerm
ifterm = AST.BIf <$> (symbol "if" *> term0) <*> (symbol "then" *> term0) <*> (symbol "else" *> term0)

listterm :: ReadP LambdaTerm
listterm = ReadP.between (symbol "[") (symbol "]") $ fmap (foldr AST.LCons AST.LNil) $ ReadP.sepBy term0 (symbol ",")

listcaseterm :: ReadP LambdaTerm
listcaseterm = do
    _ <- symbol "lcase"
    patton <- term0
    _ <- symbol "of"
    (AST.LCase patton <$> (nillistcaseterm <* symbol ";") <*> conslistcaseterm)
        <++ (flip (AST.LCase patton) <$> (conslistcaseterm <* symbol ";") <*> nillistcaseterm)
     

nillistcaseterm :: ReadP LambdaTerm
nillistcaseterm = symbol "[]" *> symbol "->" *> term0

conslistcaseterm :: ReadP ((String,String), LambdaTerm)
conslistcaseterm = (,) 
    <$> (conspatt <++ ReadP.between (symbol "(") (symbol ")") conspatt)
    <*> (symbol "->" *> term0)
  where
    conspatt = ((,) <$> (ident <* symbol ":") <*> ident)
