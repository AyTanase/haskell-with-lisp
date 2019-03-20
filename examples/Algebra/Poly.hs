{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Poly (Poly (..)) where
import Prelude hiding (pure, negate, (+), (-), (*), Fractional (..))
import qualified Prelude
import Classes


newtype Poly a = Poly [a] deriving Show

instance (Eq a, Group a) => Eq (Poly a) where
  Poly us == Poly vs = equal us vs
    where
      zerop = all (== zero)
      equal xs [] = zerop xs
      equal [] ys = zerop ys
      equal (x:xs) (y:ys) = (x == y) && equal xs ys


instance {-# OVERLAPPING #-} Pure Poly a where
  pure = Poly . pure

pmap :: (a -> b) -> Poly a -> Poly b
pmap f (Poly xs) = Poly $ map f xs


add :: Group a => [a] -> [a] -> [a]

add xs [] = xs

add [] ys = ys

add (x:xs) (y:ys) = (x + y) : add xs ys


instance {-# OVERLAPPING #-} Group a => Group (Poly a) where
  zero = Poly []
  negate = pmap negate
  Poly xs + Poly ys = Poly $ add xs ys


instance {-# OVERLAPPING #-} Action a b => Action a (Poly b) where
  (*) = pmap . (*)

instance {-# OVERLAPPING #-} (Action a b, Group b) => Action (Poly a) (Poly b) where
  _ * Poly [] = zero
  Poly xs * Poly (y:ys) = Poly $ foldr (\x zs -> (x * y) : add (x * ys) zs) [] xs


instance Div a b => Div a (Poly b) where
  (/) = flip $ pmap . flip (/)


instance Num a => Num (Poly a) where
  (+) = (+)
  (-) = (-)
  (*) = (*)
  negate = negate
  abs (Poly xs) = pure . sum $ map abs xs
  signum = undefined
  fromInteger = pure . fromInteger
