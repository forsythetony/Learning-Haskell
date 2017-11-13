{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Homework3 where

import Test.Hspec
import RPNAST
import Control.Exception


-- Function prob1
-- @author
--  Anthony Forsythe
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


-- Function prob1
-- @author
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



prob4    :: a
prob4    = undefined

-- Write your Hspec Tests below

test_probs = do
  test_prob1
  test_prob2
  test_prob3


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
