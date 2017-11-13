{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Homework3 where

import Test.Hspec
import RPNAST
import Control.Exception


-- Function prob1
--
-- @type
--  String -> PExp
--
-- @param
--    This function takes the following as inputs:
--      - A string containing operators, integer consnants, and whitespace.
--        ex: "200 + - / *"
--
-- @output
--    A PExp which will be a list of the operators (Op) in the string.
--    ex: [Val 200, Plus, Minus, IntDiv, Mul]
--
-- @description:
--    This function will first use the built in `words` function to convert the
--    string into a list of words that have been delimited by whitespace. Next
--    it will recursively operate of the list and built a PExp by using a
--    sub-function to convert the current element to an Op type before adding it
--    to the list (PExp) that is eventually returned.
--
prob1   :: String -> PExp
prob1 x   = prob1' (words x)
  where prob1' []     = []
        prob1' (x:xs) = typeValue x : prob1' xs
          where typeValue "+" = Plus
                typeValue "-" = Minus
                typeValue "/" = IntDiv
                typeValue "*" = Mul
                typeValue a = Val (read a)


-- Function prob2
--
--
-- @type
--  PExp -> Int
--
-- @param
--    This function takes the following as inputs:
--      - A PExp in Reverse Polish Notation that you want evaluated
--
-- @output
--    The integer value that is the result of evaluating the expression. A
--    success will be returned as an Int value. If the input expression
--    is malformed then and error will be thrown. Attempting to divide by zero
--    will also result in an error. Errors will be returned as the default
--    'error "error message"' value.
--
-- @description:
--    This function uses a helper function `prob2'` that takes as its initial
--    input a PExp value and an empty 'stack'. It will then recursively operate
--    over the PExp input and follow three general paths depending on what it
--    pulls off the head of the PExp input.
--
--        Case 1, a 'Val' Op:
--            In this case it will grab the Int value and push it onto the
--            stack by calling `prob2'` with the remaining PExp expression and
--            the current Int stack plus the value that was just pulled off.
--
--        Case 2, an 'arithmetic operator' Op:
--            In this case it will grab two Int values off of the stack and
--            treat them as the left and right operands. Then, using the operator
--            passed in it will evaluate the expression and push it onto the Int
--            stack.
--
--        Case 3, an empty PExp list:
--            If this is the case then we know we have evaluated everyting in
--            the expression and the final result will be the only value
--            remaining in the Int stack. We will return this value.
--
prob2    :: PExp -> Int
prob2 ops = prob2' ops []


prob2' :: PExp -> [Int] -> Int
prob2' ((Val i):xs) ans         = prob2' xs (i:ans)
prob2' (Plus:xs) (r:l:ins)      = prob2' xs ((l + r):ins)
prob2' (Minus:xs) (r:l:ins)     = prob2' xs ((l - r):ins)
prob2' (Mul:xs) (r:l:ins)       = prob2' xs ((l * r):ins)
prob2' (IntDiv:xs) (0:l:ins)    = error "Division by zero!"
prob2' (IntDiv:xs) (r:l:ins)    = prob2' xs ((l `div` r):ins)
prob2' [] [i]                   = i
prob2' _  _                     = error "Some undefined error in problem 2"


-- Function prob3
--
--
-- @type
--  PExp -> RPNResult
--
-- @param
--    This function takes the following as inputs:
--      - A PExp in Reverse Polish Notation that you want evaluated
--
-- @output
--    Will return an RPNResult value that is a `Success Int` value or an
--    RPNError value [DivByZero, BadSyntax]. The success value is just the
--    integer evaluation of the expression.
--
-- @description:
--    For a description of this functions basic operation see the description
--    for 'prob2'. The only difference with this function is that it will
--    an RPNResult type (described in a bit more detail in the section above)
--    instead of an Int value.
prob3    :: PExp -> RPNResult
prob3 ops = prob3' ops []

prob3' :: PExp -> [Int] -> RPNResult
prob3' ((Val i):xs) ans         = prob3' xs (i:ans)
prob3' (Plus:xs) (r:l:ins)      = prob3' xs ((l + r):ins)
prob3' (Minus:xs) (r:l:ins)     = prob3' xs ((l - r):ins)
prob3' (Mul:xs) (r:l:ins)       = prob3' xs ((l * r):ins)
prob3' (IntDiv:xs) (0:l:ins)    = Failure DivByZero
prob3' (IntDiv:xs) (r:l:ins)    = prob3' xs ((l `div` r):ins)
prob3' [] [i]                   = Success (i)
prob3' _  _                     = Failure BadSyntax


-- Function prob4
--
--
-- @type
--  PExp -> String
--
-- @param
--    This function takes the following as inputs:
--      - A PExp in Reverse Polish Notation that you want converted to a fully
--        parenthesized expression
--
-- @output
--    Will return a fully parenthesized string that represents the input
--    expression
--
-- @description:
--    The operation of this function is very similar to that of `prob3` and
--    `prob4`. In this case instead of pushing the integer value it will push
--    a string value onto the stack. The three general cases are detailed
--    below.
--
--        Case 1, a 'Val' Op:
--            In this case it will use the built in `show` function to convert
--            the Int value to its String representation. It will then push
--            this onto the string stack.
--
--        Case 2, an 'arithmetic operator' Op:
--            In this case it will grab two strings off of the string stack.
--            The final string (let's call it Y) will be made by surrounding
--            the values pulled off the stack (making sure to infix the proper
--            operator character) with two paranetheses. It will then push Y
--            back onto the stack.
--
--        Case 3, an empty PExp list:
--            In this case it will return the final, fully parenthesized, string.
--
prob4    :: PExp -> Result String String
prob4    ops = prob4' ops []

prob4' :: PExp -> [String] -> Result String String
prob4' ((Val i):xs) ans         = prob4' xs ((show i):ans)
prob4' (Plus:xs) (r:l:ins)      = prob4' xs ((concat ["(",(l)," + ",(r),")"]):ins)
prob4' (Minus:xs) (r:l:ins)     = prob4' xs ((concat ["(",(l)," - ",(r),")"]):ins)
prob4' (Mul:xs) (r:l:ins)       = prob4' xs ((concat ["(",(l)," * ",(r),")"]):ins)
prob4' (IntDiv:xs) (r:l:ins)    = prob4' xs ((concat ["(",(l)," / ",(r),")"]):ins)
prob4' [] [i]                   = Success i
prob4' _  _                     = Failure "Bad Syntax"

-- Write your Hspec Tests below

test_probs = do
  test_prob1
  test_prob2
  test_prob3
  test_prob4

test_prob1 = hspec $ do
  describe "Prob1 from HW 3" $ do

    context "Basic expression \"3 2 +\"" $ do
      it "should return [Val 3, Val 2, Plus]" $ do
        prob1 "3 2 +" `shouldBe` [Val 3, Val 2, Plus]

    context "Slightly more complicated expression of \"200 + - * /\"" $ do
      it "should return [Val 200, Plus, Minus, Mul, IntDiv]" $ do
        prob1 "200 + - * /" `shouldBe` [Val 200, Plus, Minus, Mul, IntDiv]

test_prob2 = hspec $ do
  describe "Prob2 from HW 3" $ do

    context "Evaluation for basic integer division -> [Val 4, Val 2, IntDiv]" $ do
      it "should return 2" $ do
        prob2 [Val 4, Val 2, IntDiv] `shouldBe` 2

    context "Evaluation for bad input [Mul]" $ do
      it "should throw an error" $ do
        evaluate (prob2 [Mul]) `shouldThrow` anyException


test_prob3 = hspec $ do
  describe "Prob3 from HW 3" $ do
    context "Evaluation for division by zero with input -> [Val 5, Val 0, IntDiv]" $ do
      it "Should return failure" $ do
        prob3 [Val 5, Val 0, IntDiv] `shouldBe` Failure DivByZero

    context "Invalid input -> [IntDiv, Plus, Val 0]" $ do
      it "Should return failure" $ do
        prob3 [IntDiv, Plus, Val 0] `shouldBe` Failure BadSyntax

    context "Valid input -> [Val 5, Val 1, Val 1, Plus, Mul]" $ do
      it "Should return `Success 10`" $ do
        prob3 [Val 5, Val 1, Val 1, Plus, Mul] `shouldBe` Success 10

test_prob4 = hspec $ do
  describe "Prob3 from HW 3" $ do
    context "Valid input of form -> [Val 1, Val 1, Plus]" $ do
      it "Should return Success \"(1 + 1)\"" $ do
        prob4 [Val 1, Val 1, Plus] `shouldBe` Success "(1 + 1)"

    context "Invalid input of form -> [Plus]" $ do
      it "Should return Failure \"Bad Syntax\"" $ do
        prob4 [Plus] `shouldBe` Failure "Bad Syntax"

    context "Simple valid input in the form of -> [Val 2]" $ do
      it "Should return Success \"2\"" $ do
        prob4 [Val 2] `shouldBe` Success "2"

    context "Simple valid input in the form of -> [Val 2, Val 4, Plus, Val 3, IntDiv]" $ do
      it "Should return Success \"((2 + 4) / 3)\"" $ do
        prob4 [Val 2, Val 4, Plus, Val 3, IntDiv] `shouldBe` Success "((2 + 4) / 3)"
