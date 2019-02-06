{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Poly (Poly (..), ppure) where
import Prelude hiding (negate, (+), (-), (*))
import qualified Prelude
import Classes


newtype Poly a = Poly [a] deriving (Show)

instance (Eq a, Group a) => Eq (Poly a) where
  (Poly us) == (Poly vs) = equal us vs
    where
      zerop xs = and (map ((==) zero) xs)
      equal xs [] = zerop xs
      equal [] ys = zerop ys
      equal (x:xs) (y:ys) = (x == y) && (equal xs ys)


ppure :: a -> Poly a
ppure = Poly . pure

pmap :: (a -> b) -> Poly a -> Poly b
pmap f (Poly xs) = Poly (map f xs)


add :: Group a => [a] -> [a] -> [a]

add xs [] = xs

add [] ys = ys

add (x:xs) (y:ys) = (x + y) : (add xs ys)


instance {-# OVERLAPPING #-} (Group a) => Group (Poly a) where
  zero = Poly []
  negate = pmap negate
  (Poly xs) + (Poly ys) = Poly (add xs ys)


instance {-# OVERLAPPING #-} (Action a b) => Action a (Poly b) where
  (*) = pmap . (*)

instance {-# OVERLAPPING #-} (Action a b, Group b) => Action (Poly a) (Poly b) where
  _ * (Poly []) = Poly []
  (Poly us) * (Poly (v:vs)) = Poly (foldr (mul v vs) [] us)
    where
      mul y ys x zs = (x * y) : (add (x * ys) zs)


instance {-# OVERLAPPING #-} (Ring r) => Ring (Poly r) where
  unit = ppure unit


instance (Num a) => Num (Poly a) where
  (+) = (+)
  (-) = (-)
  (*) = (*)
  negate = negate
  abs (Poly xs) = ppure (sum (map abs xs))
  signum = undefined
  fromInteger = ppure . fromInteger
