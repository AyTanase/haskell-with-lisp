module Fact where

fact0, fact1 :: (Num a, Eq a) => a -> a

fact0 0 = 1
fact0 n = n * fact0 (n - 1)

fact1 = fact' 1
  where
    fact' p 0 = p
    fact' p n = fact' (p * n) (n - 1)


fact2 :: (Num a, Enum a) => a -> a
fact2 n = product [1..n]

fact3 :: (Num a, Enum a) => Int -> a
fact3 n = product $ take n [1..]
