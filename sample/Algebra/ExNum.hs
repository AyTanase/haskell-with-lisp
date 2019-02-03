{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module ExNum where
import Prelude hiding (pure)
import GHC.Base (liftA2)
import Classes (pure, join)
import qualified GHC.Real
import qualified Classes
import AlNum
import Poly
import Ratio
import Complex


instance (Num a, Applicative f) => Num (f a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Fractional a, Applicative f) => Fractional (f a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational


polyAbs :: Num a => (Poly a) -> a
polyAbs (Poly xs) = sum (map abs xs)

instance {-# OVERLAPPING #-} (Num a) => Num (Poly a) where
  (+) = (Classes.+)
  (-) = (Classes.-)
  (*) = (Classes.*)
  negate = Classes.negate
  abs = pure . polyAbs
  signum = undefined
  fromInteger = pure . fromInteger


instance {-# OVERLAPPING #-} (Num a) => Num (Ratio a) where
  (+) = (Classes.+)
  (-) = (Classes.-)
  (*) = (Classes.*)
  negate = Classes.negate
  abs (x :% y) = (abs x) :% (abs y)
  signum (x :% y) = pure ((signum x) * (signum y))
  fromInteger = pure . fromInteger

instance {-# OVERLAPPING #-} (Num a) => Fractional (Ratio a) where
  (/) = (Classes./)
  recip = Classes.recip
  fromRational (x GHC.Real.:% y) = (fromInteger x) :% (fromInteger y)


complexAbs :: Floating a => (Complex a) -> a
complexAbs (Complex x y) = sqrt ((absq x) + (absq y))
  where
    absq = (join (*)) . abs

instance {-# OVERLAPPING #-} (Floating a) => Num (Complex a) where
  (+) = (Classes.+)
  (-) = (Classes.-)
  (*) = (Classes.*)
  negate = Classes.negate
  abs = pure . complexAbs
  signum = undefined
  fromInteger = pure . fromInteger

instance {-# OVERLAPPING #-} (Num (Complex a), Fractional a) => Fractional (Complex a) where
  (/) = (Classes./)
  recip = Classes.recip
  fromRational = pure . fromRational
