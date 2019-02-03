{-# LANGUAGE FlexibleInstances, IncoherentInstances, MultiParamTypeClasses, UndecidableInstances #-}

module AlNum where
import Prelude ()
import qualified Prelude
import Classes

instance (Num a) => Semigroup a where
  (+) = (Prelude.+)

instance (Num a) => Monoid a where
  zero = 0

instance (Num a) => Group a where
  negate = Prelude.negate
  (-) = (Prelude.-)

instance (Num a) => Action a a where
  (*) = (Prelude.*)

instance (Num a) => Ring a where
  unit = 1

instance (Fractional a) => Div a a where
  (/) = (Prelude./)
