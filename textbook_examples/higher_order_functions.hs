--  This is just a document to help practice higher
--  order functions from the book "Learn You a Haskell"





{-
  A function that takes a number and compares it to 100

  This is the first implementation
-}
compareToAHundred :: (Num a, Ord a) => a -> Ordering
compareToAHundred x = compare 100 x

{-
  A function that takes a number and compares it to 100

  This is the second implementation

  So how does this function work?

    - The compare function has a type of (Ord a) => a -> ( a -> Ordering )
    - Calling the compare function with just a number will return the
      following function type:
        (Num a, Ord a) => a -> Ordering

        - The addition class constraint here (Num a) sneaks up because 100
          is also part of the Num typeclass
-}
compareToAHundred' :: (Num a, Ord a) => a -> Ordering
compareToAHundred' = compare 100



{-
  divideByTen
  A curried function starting with an infix function
-}
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)


{-
  applyTwice
  A function that takes a function and applies it twice to something

  Notes:
    So I'm thinking about it and I feel like I should stop thinking of the
    (n - 1) arrows like counting the arguments and more like f ( g ( x ) )
    if that makes sense. I don't know. Hopefully I can expand on that in the
    future...
-}
applyTwice :: (a -> a) -> a -> a
applyTwice f a = f (f a)


{-
  zipWith'

  This function takes a function and two lists and then joins the two lists
  by applying the function between corresponding elements
-}
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []                                    --  These are called the joining conditions
zipWith' _ _ [] = []                                    --  These are called the joining conditions
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

{-
  This is our custom implementation of the `flip` function. It basically takes a function
  and returns a function where the first two arguments are flipped

  The following can be read as a function that takes a function that
  takes an 'a' and a 'b' and returns a function that takes a 'b' and
  an 'a'. You know...flippin 'em
-}
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x


{-
  The previous flip' function can also be defined in an even simpler manner due
  to the fact that the functions returned are curried (at least I think that's
  the reason)
-}
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x






{-
    MAPS AND FILTERS
-}





{-
  The `chain` and `numLongChains` functions
-}

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n    = n : chain (n `div` 2)
  | odd n     = n : chain (n*3 + 1)


numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15
