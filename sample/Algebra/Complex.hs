{-# LANGUAGE FlexibleInstances, MonoLocalBinds, MultiParamTypeClasses, UndecidableInstances #-}

module Complex where
import Prelude ()
import Classes

data Complex a = Complex { re :: a, im :: a } deriving (Eq)

instance (Show a) => Show (Complex a) where
  show (Complex x y) = "Complex " ++ (shows' x " ") ++ (shows' y "")
    where
      shows' = showsPrec 11
  showsPrec p = (showParen (p >= 11)) . (++) . show

instance {-# OVERLAPPING #-} (Monoid m) => Pure Complex m where
  pure x = Complex x zero

instance {-# OVERLAPPING #-} (Group g) => Join Complex g where
  join (Complex (Complex u v) (Complex x y)) = Complex (u - y) (v + x)

instance {-# OVERLAPPING #-} (Semigroup s) => Semigroup (Complex s) where
  (Complex u v) + (Complex x y) = Complex (u + x) (v + y)

instance {-# OVERLAPPING #-} (Group g) => Group (Complex g) where
  negate (Complex x y) = Complex (negate x) (negate y)

instance {-# OVERLAPPING #-} (Action a b) => Action a (Complex b) where
  r * (Complex x y) = Complex (r * x) (r * y)

instance {-# OVERLAPPING #-} (Action a b, Group b) => Action (Complex a) (Complex b) where
  (Complex u v) * (Complex x y) = Complex ((u * x) - (v * y)) ((u * y) + (v * x))

instance {-# OVERLAPPING #-} (Div a b) => Div a (Complex b) where
  (Complex x y) / z = Complex (x / z) (y / z)

instance {-# OVERLAPPING #-} (Group a, Action a a, Div a (Complex b), Action (Complex a) (Complex b)) => Div (Complex a) (Complex b) where
  z / (Complex x y) = (Complex x (negate y)) * (z / ((x * x) + (y * y)))
