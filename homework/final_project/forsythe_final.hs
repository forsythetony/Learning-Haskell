{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Final where

import Prelude hiding (LT, GT, EQ)
import System.IO
import Base
import Data.Maybe
import Data.List
import Operators
import RecursiveFunctionsAST
import RecursiveFunctionsParse
import Test.Hspec
import Control.Exception (evaluate,AsyncException(..))
-- Uncomment the following if you choose to do Problem 3.
{-
import System.Environment
import System.Directory (doesFileExist)
import System.Process
import System.Exit
import System.Console.Haskeline
--        ^^ This requires installing haskeline: cabal update && cabal install haskeline
-}


--
-- The parsing function, parseExp :: String -> Exp, is defined for you.
--

facvar   = parseExp ("var fac = function(n) { if (n==0) 1 else n * fac(n-1) };" ++
                   "fac(5)")

facrec   = parseExp ("rec fac = function(n) { if (n==0) 1 else n * fac(n-1) };" ++
                   "fac(5)")

exp1     = parseExp "var a = 3; var b = 8; var a = b, b = a; a + b"
exp2     = parseExp "var a = 3; var b = 8; var a = b; var b = a; a + b"
exp3     = parseExp "var a = 2, b = 7; (var m = 5 * a, n = b - 1; a * n + b / m) + a"
exp4     = parseExp "var a = 2, b = 7; (var m = 5 * a, n = m - 1; a * n + b / m) + a"
-- N.b.,                                                  ^^^ is a free occurence of m (by Rule 2)

-- Function: eval
--
-- @original description
--  The evaluation function for the recursive function language.
--
-- @type
--  Exp -> Env -> Value
--
-- @description:
--    Evaluates a given expression with an environment and returns the computed value
--
eval :: Exp -> Env -> Value
eval (Literal v) env                = v
eval (Unary op a) env               = unary  op (eval a env)
eval (Binary op a b) env            = binary op (eval a env) (eval b env)
eval (If a b c) env                 = let BoolV test = eval a env
                                      in if test then  eval b env else eval c env
eval (Variable x) env               = fromJust x (lookup x env)
  where fromJust x (Just v)         = v
        fromJust x Nothing          = errorWithoutStackTrace ("Variable " ++ x ++ " unbound!")
eval (Function x body) env          = ClosureV x body env
-----------------------------------------------------------------
eval (Declare decls body) env = eval body newEnv         -- This clause needs to be changed.
  where vars = map fst decls
        exps = map snd decls
        vals = map (\e -> eval e env) exps
        newEnv = zip vars vals
-----------------------------------------------------------------
eval (RecDeclare x exp body) env    = eval body newEnv
  where newEnv = (x, eval exp newEnv) : env
eval (Call fun arg) env = eval body newEnv
  where ClosureV x body closeEnv    = eval fun env
        newEnv = (x, eval arg env) : closeEnv

-- Use this function to run your eval solution.
execute :: Exp -> Value
execute exp = eval exp []
-- Example usage: execute exp1

{-

Hint: it may help to remember that:
   map :: (a -> b) -> [a] -> [b]
   concat :: [[a]] -> [a]
when doing the Declare case.

-}


freeChecker seen free ((v,e):rest)  = freeChecker (v:seen) (freeByRule1 seen e ++ free) rest
freeChecker seen free []            = free

-- Function: freeByRule1
--
-- @type
--  [String] -> Exp -> [String]
--
-- @description:
--
--
freeByRule1 :: [String] -> Exp -> [String]
freeByRule1 seen (Literal _)            = []
freeByRule1 seen (Unary _ e)            = freeByRule1 seen e
freeByRule1 seen (Binary _ e1 e2)       = freeByRule1 seen e1 ++ freeByRule1 seen e2
freeByRule1 seen (If t e1 e2)           = freeByRule1 seen t ++ freeByRule1 seen e1 ++ freeByRule1 seen e2
freeByRule1 seen (Variable x)           = if (elem x seen) then [] else [x]
freeByRule1 seen (Declare ds body)     = let r1 = freeChecker seen [] ds
                                            in  r1 ++ freeByRule1 (vars ++ seen) body
                                            where vars = map fst ds

freeByRule1 seen (RecDeclare x e body)  = freeByRule1 (x:seen) e ++ freeByRule1 (x:seen) body
freeByRule1 seen (Function x e)         = freeByRule1 (x:seen) e
freeByRule1 seen (Call e1 e2)           = freeByRule1 seen e1 ++ freeByRule1 seen e2


-- Function: freeByRule2
--
-- @type
--  [String] -> Exp -> [String]
--
-- @description:
--
--
freeByRule2 :: [String] -> Exp -> [String]
freeByRule2 seen (Literal _)            = []
freeByRule2 seen (Unary _ e)            = freeByRule2 seen e
freeByRule2 seen (Binary _ e1 e2)       = freeByRule2 seen e1 ++ freeByRule2 seen e2
freeByRule2 seen (If t e1 e2)           = freeByRule2 seen t ++ freeByRule2 seen e1 ++ freeByRule2 seen e2
freeByRule2 seen (Variable x)           = if (elem x seen) then [] else [x]
{--
  Declare
    Variables from `x1` use to check the body using the variables that we've already
    seen.
--}
freeByRule2 seen (Declare x1 body)      = concat free ++ freeByRule2 (seen ++ vars) body
  where   vars = map fst x1
          exps = map snd x1
          free = map (freeByRule2 seen) exps


freeByRule2 seen (RecDeclare x e body)  = freeByRule2 (x:seen) e ++ freeByRule2 (x:seen) body
freeByRule2 seen (Function x e)         = freeByRule2 (x:seen) e
freeByRule2 seen (Call e1 e2)           = freeByRule2 seen e1 ++ freeByRule2 seen e2

---- Problem 3.

-- repl :: IO ()
-- repl = do
--          putStr "RecFun> "
--          iline <- getLine
--          process iline
--
-- process :: String -> IO ()
-- process "quit" = return ()
-- process iline  = do
--   putStrLn (show v ++ "\n")
--   repl
--    where e = parseExp i
--
--
--    line
--          v = eval e []




test_probs = do
  test_prob1
  test_prob2

test_prob1 = hspec $ do
  describe "Problem 1 from Final Project" $ do

    context "Basic" $ do
      it "should return IntV 120" $ do
        execute facrec `shouldBe` IntV 120

test_prob2 = hspec $ do
  describe "Problem 2 from Final Project (`freeByRule` problems)" $ do

    context "freeByRule1 test with exp3" $ do
      it "Should return empty list" $ do
        freeByRule1 [] exp3 `shouldBe` []

    context "freeByRule1 test with exp4" $ do
      it "Should return empty list" $ do
        freeByRule1 [] exp4 `shouldBe` []

    context "freeByRule2 test with exp3" $ do
      it "Should return empty list" $ do
        freeByRule2 [] exp3 `shouldBe` []

    context "freeByRule2 test with exp4" $ do
      it "Should return a list containing [\"m\"]" $ do
        freeByRule2 [] exp4 `shouldBe` ["m"]
