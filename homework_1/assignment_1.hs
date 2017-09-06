{-
  FIRST HOMEWORK PROBLEM
-}
prob1 :: Char -> Char
prob1 a
  | a == 'Z' = 'A'
  | a == 'z' = 'a'
  | a `elem` ['A'..'Z']   = succ a
  | a `elem` ['a'..'z']   = succ a
  | otherwise             = a


{-
  SECOND HOMEWORK PROBLEM
-}
prob2 :: Char -> Int
prob2 a
  | a == '0' = 0
  | a == '1' = 1
  | a == '2' = 2
  | a == '3' = 3
  | a == '4' = 4
  | a == '5' = 5
  | a == '6' = 6
  | a == '7' = 7
  | a == '8' = 8
  | a == '9' = 9
  | otherwise         = -1
