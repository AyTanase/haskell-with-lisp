{-# LANGUAGE MultiParamTypeClasses #-}
module Classes where
import Prelude hiding (not)
import qualified Prelude (not)
import Data.Maybe
class HasNot a where
  not :: (a -> Bool)
  or :: (a -> a -> a)
  or x y = (if (not x) then y else x)
instance HasNot Bool where
  not = Prelude.not
instance HasNot (Maybe a) where
  not = isNothing
class Category cat where
  id :: (cat a a)
  (.) :: ((cat b c) -> (cat a b) -> (cat a c))
class (Category cat, Category cat') => Functor f cat cat' where
  fmap :: ((cat a b) -> (cat' (f a) (f b)))
