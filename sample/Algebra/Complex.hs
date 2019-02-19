{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Complex (Complex (..)) where
import Prelude hiding (pure, negate, (+), (-), (*), Fractional (..))
import qualified Prelude
import Classes


data Complex a = a :+ a deriving (Eq, Show)

instance {-# OVERLAPPING #-} Group a => Pure Complex a where
  pure = (:+ zero)

cmap :: (a -> b) -> Complex a -> Complex b
cmap f (x :+ y) = f x :+ f y


instance {-# OVERLAPPING #-} Group a => Group (Complex a) where
  zero = pure zero
  negate = cmap negate
  (u :+ v) + (x :+ y) = (u + x) :+ (v + y)


instance {-# OVERLAPPING #-} Action a b => Action a (Complex b) where
  (*) = cmap . (*)

instance {-# OVERLAPPING #-} (Action a b, Group b) => Action (Complex a) (Complex b) where
  (u :+ v) * (x :+ y) = ((u * x) - (v * y)) :+ ((u * y) + (v * x))


instance {-# OVERLAPPING #-} Div a b => Div a (Complex b) where
  (/) = flip $ cmap . flip (/)

instance {-# OVERLAPPING #-} (Div a b, Ring a, Group b) => Div (Complex a) (Complex b) where
  z / (x :+ y) = ((x :+ negate y) * z) / ((x * x) + (y * y))


instance Num a => Num (Complex a) where
  (+) = (+)
  (-) = (-)
  (*) = (*)
  negate = negate
  abs = undefined
  signum = undefined
  fromInteger = pure . fromInteger
