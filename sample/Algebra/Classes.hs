{-# LANGUAGE FlexibleInstances, MonoLocalBinds, MultiParamTypeClasses, UndecidableInstances #-}

module Classes (module Classes, module Prelude) where
import Prelude hiding (pure, Semigroup, Monoid, (+), negate, (-), (*), recip, (/))
import qualified Prelude
import qualified GHC.Base
import GHC.Base (liftA2)


class Pure f a where
  pure :: a -> f a

instance (Applicative f) => Pure f a where
  pure = Prelude.pure


class Join m a where
  join :: m (m a) -> m a

instance (Monad m) => Join m a where
  join = GHC.Base.join


class Semigroup s where
  (+) :: s -> s -> s

instance (Semigroup s, Applicative f) => Semigroup (f s) where
  (+) = liftA2 (+)


class (Semigroup m) => Monoid m where
  zero :: m

instance (Monoid m, Semigroup (f m), Pure f m) => Monoid (f m) where
  zero = pure zero


class (Monoid g) => Group g where
  negate :: g -> g
  (-) :: g -> g -> g
  negate = (-) zero
  x - y = x + (negate y)

instance (Group g, Monoid (f g), Functor f) => Group (f g) where
  negate = fmap negate


class Action a b where
  (*) :: a -> b -> b

instance {-# INCOHERENT #-} (Action a b, Functor f) => Action a (f b) where
  (*) = fmap . (*)

instance (Action a b, Applicative f) => Action (f a) (f b) where
  (*) = liftA2 (*)


class (Group r, Action r r) => Ring r where
  unit :: r

instance (Ring r, Group (f r), Action (f r) (f r), Pure f r) => Ring (f r) where
  unit = pure unit


class (Action a b) => Div a b where
  (/) :: b -> a -> b

instance (Div a b, Functor f) => Div a (f b) where
  (/) = flip (fmap . (flip (/)))

instance (Div a b, Applicative f) => Div (f a) (f b) where
  (/) = liftA2 (/)


class (Ring k, Div k k) => Field k

instance (Ring k, Div k k) => Field k

recip :: Field k => k -> k
recip = (/) unit

instance (Field k, Action k a) => Div k a where
  x / k = (recip k) * x


class (Ring r, Group m, Action r m) => Module r m

instance (Ring r, Group m, Action r m) => Module r m


class (Field k, Module k v) => Vector k v

instance (Field k, Module k v) => Vector k v
