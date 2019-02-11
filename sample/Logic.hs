module Logic where
import Prelude (Applicative (pure, (<*>)))

const :: a -> b -> a
const = pure


id :: a -> a
id = const <*> const

infixr 0 $
($) :: (a -> b) -> a -> b
($) = id


liftA :: Applicative f => (a -> b) -> f a -> f b
liftA = const (<*>) <*> pure

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = liftA


liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 = (.) (<*>) . liftA


flip :: Applicative f => f (a -> b) -> a -> f b
flip = liftA2 (.) (<*>) $ const pure

join :: (a -> a -> b) -> a -> b
join = flip (<*>) id
