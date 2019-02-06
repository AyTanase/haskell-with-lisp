{-# LANGUAGE FlexibleInstances, MonoLocalBinds, MultiParamTypeClasses, UndecidableInstances #-}

module Classes where
import Prelude hiding (negate, (+), (-), (*), (/), recip)
import qualified Prelude
import GHC.Base (liftA2)


class Group g where
  zero :: g
  negate :: g -> g
  (+), (-) :: g -> g -> g
  negate x = zero - x
  x - y = x + (negate y)

instance (Num a) => Group a where
  zero = 0
  negate = Prelude.negate
  (+) = (Prelude.+)
  (-) = (Prelude.-)

instance {-# OVERLAPPING #-} (Group g, Applicative f) => Group (f g) where
  zero = pure zero
  negate = fmap negate
  (+) = liftA2 (+)
  (-) = liftA2 (-)


class Action a b where
  (*) :: a -> b -> b

instance {-# INCOHERENT #-} (Num a) => Action a a where
  (*) = (Prelude.*)

instance (Action a b, Functor f) => Action a (f b) where
  (*) = fmap . (*)

instance {-# OVERLAPPING #-} (Action a b, Applicative f) => Action (f a) (f b) where
  (*) = liftA2 (*)


class (Group r, Action r r) => Ring r where
  unit :: r

instance (Num a) => Ring a where
  unit = 1

instance (Ring r, Applicative f) => Ring (f r) where
  unit = pure unit


class (Action a b) => Div a b where
  (/) :: b -> a -> b

instance (Fractional a) => Div a a where
  (/) = (Prelude./)

instance (Div a b, Functor f) => Div a (f b) where
  (/) = flip (fmap . (flip (/)))

instance (Div a b, Applicative f) => Div (f a) (f b) where
  (/) = liftA2 (/)


class (Ring k, Div k k) => Field k

instance (Ring k, Div k k) => Field k

recip :: Field k => k -> k
recip x = unit / x

instance (Action k a, Field k) => Div k a where
  x / y = (recip y) * x
