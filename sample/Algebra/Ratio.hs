{-# LANGUAGE FlexibleInstances, MonoLocalBinds, MultiParamTypeClasses, UndecidableInstances #-}

module Ratio where
import Prelude ()
import Classes

data Ratio r = r :% r deriving (Show)

instance (Action r r, Eq r) => Eq (Ratio r) where
  (a :% b) == (c :% d) = (a * d) == (b * c)

instance {-# OVERLAPPING #-} (Ring r) => Pure Ratio r where
  pure x = x :% unit

instance {-# OVERLAPPING #-} (Semigroup r, Action r r) => Semigroup (Ratio r) where
  (a :% b) + (c :% d) = ((a * d) + (b * c)) :% (b * d)

instance {-# OVERLAPPING #-} (Monoid (Ratio r), Group r) => Group (Ratio r) where
  negate (a :% b) = (negate a) :% b

instance {-# OVERLAPPING #-} (Action a b) => Action a (Ratio b) where
  x * (y :% z) = (x * y) :% z

instance {-# OVERLAPPING #-} (Action a b) => Action (Ratio a) (Ratio b) where
  (a :% b) * (c :% d) = (a * c) :% (b * d)

instance {-# OVERLAPPING #-} (Action a b) => Div a (Ratio b) where
  (x :% y) / z = x :% (z * y)

instance {-# OVERLAPPING #-} (Action a b) => Div (Ratio a) (Ratio b) where
  r / (x :% y) = (y :% x) * r

instance {-# OVERLAPPING #-} (Action r r) => Join Ratio r where
  join (x :% y) = x / y
