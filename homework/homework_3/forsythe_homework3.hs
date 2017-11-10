{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Homework3 where

import Test.Hspec
import RPNAST
import Control.Exception

prob1   :: String -> PExp
prob1 x   = prob1' (words x)
  where prob1' []     = []
        prob1' (x:xs) = typeValue x : prob1' xs
          where typeValue "+" = Plus
                typeValue "-" = Minus
                typeValue "/" = IntDiv
                typeValue "*" = Mul
                typeValue a = Val (read a)

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

test_prob1 = hspec $ do
  describe "Prob1 from HW 3" $ do
    context "Basic expression \"3 2 +\"" $ do
      it "should return [Val 3, Val 2, Plus]" $ do
        prob1 "3 2 +" `shouldBe` [Val 3, Val 2, Plus]

  describe "Pob1 from HW 3" $ do
    context "Slightly more complicated expression of \"200 + - * /\"" $ do
      it "should return [Val 200, Plus, Minus, Mul, IntDiv]" $ do
        prob1 "200 + - * /" `shouldBe` [Val 200, Plus, Minus, Mul, IntDiv]

test_prob2 = hspec $ do
  describe "Prob2 from HW 3" $ do
    context "Evaluation for basic integer division -> [Val 4, Val 2, IntDiv]" $ do
      it "should return 2" $ do
        prob2 [Val 4, Val 2, IntDiv] `shouldBe` 2

  describe "Prob2 from HW 3" $ do
    context "Evaluation for bad input [Mul]" $ do
      it "should throw an error" $ do
        evaluate (prob2 [Mul]) `shouldThrow` anyException

test_prob3 = hspec $ do
  describe "Prob3 from HW 3" $ do
    context "Evaluation for division by zero with input -> [Val 5, Val 0, IntDiv]" $ do
      it "Should return failure" $ do
        prob3 [Val 5, Val 0, IntDiv] `shouldBe` Failure DivByZero

  describe "Prob3 from HW 3" $ do
    context "Invalid input -> [IntDiv, Plus, Val 0]" $ do
      it "Should return failure" $ do
        prob3 [IntDiv, Plus, Val 0] `shouldBe` Failure BadSyntax 
