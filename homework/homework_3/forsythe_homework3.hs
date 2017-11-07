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

prob2    :: a
prob2    = undefined

prob3    :: a
prob3    = undefined

prob4    :: a
prob4    = undefined

-- Write your Hspec Tests below
