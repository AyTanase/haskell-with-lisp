{-# LANGUAGE FlexibleInstances, MonoLocalBinds, MultiParamTypeClasses, UndecidableInstances #-}

module Ratio (Ratio (..)) where
import Prelude hiding (pure, negate, (+), (-), (*))
import qualified Prelude
import Classes


data Ratio a = a :% a deriving Show

instance (Eq a, Action a a) => Eq (Ratio a) where
  (u :% v) == (x :% y) = (u * y) == (v * x)


instance {-# OVERLAPPING #-} Ring a => Pure Ratio a where
  pure = (:% unit)

rmap :: (a -> a) -> Ratio a -> Ratio a
rmap f (x :% y) = f x :% y


instance {-# OVERLAPPING #-} Ring r => Group (Ratio r) where
  zero = pure zero
  negate = rmap negate
  (u :% v) + (x :% y) = ((u * y) + (v * x)) :% (v * y)


instance {-# OVERLAPPING #-} Action a b => Action a (Ratio b) where
  (*) = rmap . (*)

instance {-# OVERLAPPING #-} Action a b => Action (Ratio a) (Ratio b) where
  (u :% v) * (x :% y) = (u * x) :% (v * y)


instance {-# OVERLAPPING #-} Action a b => Div a (Ratio b) where
  (x :% y) / z = x :% (z * y)

instance {-# OVERLAPPING #-} Action a b => Div (Ratio a) (Ratio b) where
  r / (x :% y) = (y :% x) * r


instance Num a => Num (Ratio a) where
  (+) = (+)
  (-) = (-)
  (*) = (*)
  negate = negate
  abs (x :% y) = abs x :% abs y
  signum (x :% y) = pure $ signum x * signum y
  fromInteger = pure . fromInteger
