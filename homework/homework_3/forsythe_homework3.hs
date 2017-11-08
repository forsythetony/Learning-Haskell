{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Homework3 where

import Test.Hspec
import RPNAST

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
prob2' (IntDiv:xs) (r:l:ins)    = prob2' xs ((l `div` r):ins)
prob2' (IntDiv:xs) (0:l:ins)    = error "Division by zero!"
prob2' [] [i]                   = i
prob2' _  _                     = error "Some undefined error in problem 2"

prob3    :: a
prob3    = undefined

prob4    :: a
prob4    = undefined

-- Write your Hspec Tests below
