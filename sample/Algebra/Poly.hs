{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Poly where
import Prelude ()
import Classes

data Poly r = Poly [r] deriving (Show)

instance (Eq a, Monoid a) => Eq (Poly a) where
  (Poly xs) == (Poly ys) = eq xs ys
    where
      eq [] xs = and (map ((==) zero) xs)
      eq xs [] = eq [] xs
      eq (x:xs) (y:ys) = (x == y) && (eq xs ys)

polyMap :: (a -> b) -> Poly a -> Poly b
polyMap f (Poly xs) = Poly (fmap f xs)

instance {-# OVERLAPPING #-} () => Pure Poly a where
  pure = Poly . pure


polyAdd :: Semigroup s => [s] -> [s] -> [s]
polyAdd [] ys = ys
polyAdd xs [] = xs
polyAdd (x:xs) (y:ys) = (x + y) : (polyAdd xs ys)

instance {-# OVERLAPPING #-} (Semigroup s) => Semigroup (Poly s) where
  (Poly xs) + (Poly ys) = Poly (polyAdd xs ys)


instance {-# OVERLAPPING #-} (Monoid m) => Join Poly m where
  join (Poly xs) = Poly (join' xs)
    where
      join' [] = []
      join' ((Poly xs) : ys) = polyAdd xs (zero : (join' ys))


instance {-# OVERLAPPING #-} (Group g) => Group (Poly g) where
  negate = polyMap negate


instance {-# OVERLAPPING #-} (Action a b) => Action a (Poly b) where
  (*) x = polyMap ((*) x)

instance {-# OVERLAPPING #-} (Action a b, Semigroup b) => Action (Poly a) (Poly b) where
  (Poly xs) * (Poly ys) = Poly (act xs ys)
    where
      act xs [] = []
      act [] ys = []
      act (x:xs) ys = (x * (head ys)) : (polyAdd (x * (tail ys)) (act xs ys))
