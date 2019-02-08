module Logic where
import Prelude (const, (<*>))

id :: a -> a
id = const <*> const


(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = (const (<*>)) <*> const


liftCompose :: (b -> c -> d) -> b -> (a -> c) -> a -> d
liftCompose = (.) (.)

compose2 :: (a -> b) -> (_v1 -> _v2 -> a) -> _v1 -> _v2 -> b
compose2 = liftCompose (.)

liftApply :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
liftApply = compose2 (<*>) (.)

apply2 :: (_v3 -> _v4 -> a -> b) -> (_v3 -> _v4 -> a) -> _v3 -> _v4 -> b
apply2 = liftApply (<*>)

apply3 :: (_v5 -> _v6 -> _v7 -> a -> b) -> (_v5 -> _v6 -> _v7 -> a) -> _v5 -> _v6 -> _v7 -> b
apply3 = liftApply apply2

flip :: (a -> b -> c) -> b -> a -> c
flip = apply3 const (const const)

join :: (a -> a -> b) -> a -> b
join = flip (<*>) id
