{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Complex (Complex (..)) where
import Prelude hiding (pure, negate, (+), (-), (*), Fractional (..))
import qualified Prelude
import Classes


data Complex a = Complex { re :: a, im :: a } deriving (Eq)

instance Show a => Show (Complex a) where
  show (Complex x y) = "Complex " ++ (shows' x " ") ++ (shows' y "")
    where
      shows' = showsPrec 11
  showsPrec p = (showParen (p >= 11)) . (++) . show


instance {-# OVERLAPPING #-} Group a => Pure Complex a where
  pure = flip Complex zero

cmap :: (a -> b) -> Complex a -> Complex b
cmap f (Complex x y) = Complex (f x) (f y)


instance {-# OVERLAPPING #-} Group a => Group (Complex a) where
  zero = pure zero
  negate = cmap negate
  (Complex u v) + (Complex x y) = Complex (u + x) (v + y)


instance {-# OVERLAPPING #-} Action a b => Action a (Complex b) where
  (*) = cmap . (*)

instance {-# OVERLAPPING #-} (Action a b, Group b) => Action (Complex a) (Complex b) where
  (Complex u v) * (Complex x y) = Complex ((u * x) - (v * y)) ((u * y) + (v * x))


instance {-# OVERLAPPING #-} Div a b => Div a (Complex b) where
  (/) = flip (cmap . (flip (/)))

instance {-# OVERLAPPING #-} (Div a b, Ring a, Group b) => Div (Complex a) (Complex b) where
  z / (Complex x y) = (Complex x (negate y)) * (z / ((x * x) + (y * y)))


instance Num a => Num (Complex a) where
  (+) = (+)
  (-) = (-)
  (*) = (*)
  negate = negate
  abs = undefined
  signum = undefined
  fromInteger = pure . fromInteger
