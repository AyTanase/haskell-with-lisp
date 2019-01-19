{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Algebra (module Algebra, module Prelude) where
import Prelude hiding (Monoid, (+), negate, (-))
import qualified Prelude
import GHC.Base (liftA2)
class Monoid m where
  zero :: m
  (+) :: (m -> m -> m)
class (Monoid g) => Group g where
  negate :: (g -> g)
  (-) :: (g -> g -> g)
  negate = ((-) zero)
  (-) x y = (x + (negate y))
instance {-# OVERLAPPING #-} (Monoid m, Applicative f) => Monoid (f m) where
  zero = (pure zero)
  (+) = (liftA2 (+))
instance (Group g, Applicative f) => Group (f g) where
  negate = (fmap negate)
  (-) = (liftA2 (-))
instance (Num a) => Monoid a where
  zero = 0
  (+) = (Prelude.+)
instance (Num a) => Group a where
  negate = Prelude.negate
  (-) = (Prelude.-)
