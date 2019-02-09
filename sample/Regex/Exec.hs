module Exec where
import Common
import Control.Applicative

exec :: Num a => Op -> String -> Maybe a
exec rx cs = fmap fst (exec' 0 rx cs)
  where
    exec' n Finite xs = Just (n, xs)
    exec' n (Compare x p) (_v1:xs)
      | (x == _v1) = exec' (n + 1) p xs
    exec' _ (Compare _ _) _ = Nothing
    exec' n (Split p q) xs = (exec' n p xs) <|> (exec' n q xs)
    exec' n (Atomic p q r) xs = maybe (exec' n r xs) (uncurry (flip exec' q)) (exec' n p xs)

match :: Num a => Op -> String -> Maybe (a, a)
match = match' 0
  where
    match' n rx xs = (fmap ((,) n) (exec rx xs)) <|> ((match' (n + 1) rx) =<< (safeTail xs))
