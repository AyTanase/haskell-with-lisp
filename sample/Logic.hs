module Logic where
import Prelude (Applicative (pure, (<*>)))

id :: a -> a
id = pure <*> (pure :: a -> b -> a)


liftA :: Applicative f => (a -> b) -> f a -> f b
liftA = (pure (<*>)) <*> pure


liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 = liftA (liftA (<*>)) liftA


flip :: Applicative f => f (a -> b) -> a -> f b
flip = liftA2 liftA (<*>) (pure pure)

join :: (a -> a -> b) -> a -> b
join = flip (<*>) id
