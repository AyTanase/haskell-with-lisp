module Logic where
import Prelude (const, (<*>))

id :: (a -> a)
id = ((<*>) const const)


(.) :: ((b -> c) -> (a -> b) -> a -> c)
(.) = ((<*>) (const (<*>)) const)

fixcompose = ((.) (.))
compose2 = (fixcompose (.))


fixapply = (compose2 (<*>) (.))
apply2 :: ((a -> b -> c -> d) -> (a -> b -> c) -> a -> b -> d)
apply2 = (fixapply (<*>))
apply3 = (fixapply apply2)


flip :: ((a -> b -> c) -> b -> a -> c)
flip = (apply3 const (const const))

join :: ((a -> a -> b) -> a -> b)
join = (flip (<*>) id)
